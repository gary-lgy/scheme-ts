import { Context } from '..'
import * as errors from '../errors/errors'
import { SyntaxIdentifier, SyntaxNode } from '../lang/SchemeSyntax'
import { Environment } from '../types'
import { stringify } from '../utils/stringify'
import {
  EVBuiltInProcedure,
  EVCompoundProcedure,
  EVProcedure,
  ExpressibleValue,
  makeList,
  NonTailCallExpressibleValue
} from './ExpressibleValue'
import { evaluate, ValueGenerator } from './interpreter'
import { handleRuntimeError, introduceBinding, popEnvironment, pushEnvironment } from './util'

// For BuiltIn procedures, we only need to check the number of arguments
// The parameter names are meaningless and unnecessary
export type ParameterPassingStyle = FixedArgs | VarArgs

type FixedArgs = { style: 'fixed-args'; numParams: number }
type VarArgs = { style: 'var-args'; numCompulsoryParameters: number }

// For compound procedures, we need both the number of arguments and the parameter names
// in order to extend the function environment with the arguments
export type NamedParameterPassingStyle = FixedArgsWithParameterNames | VarArgsWithParameterNames

export type FixedArgsWithParameterNames = FixedArgs & { parameters: SyntaxIdentifier[] }
export type VarArgsWithParameterNames = VarArgs & {
  compulsoryParameters: SyntaxIdentifier[]
  restParameters: SyntaxIdentifier
}

export type ParameterArgumentPair = { parameter: SyntaxIdentifier; argument: ExpressibleValue }

export const checkNumberOfArguments = (
  context: Context,
  name: string,
  argumentPassingStyle: ParameterPassingStyle,
  numArgs: number,
  callExpression: SyntaxNode
) => {
  if (argumentPassingStyle.style === 'fixed-args' && argumentPassingStyle.numParams !== numArgs) {
    handleRuntimeError(
      context,
      new errors.InvalidNumberOfArguments(
        callExpression,
        name,
        argumentPassingStyle.numParams,
        numArgs
      )
    )
  } else if (
    argumentPassingStyle.style === 'var-args' &&
    numArgs < argumentPassingStyle.numCompulsoryParameters
  ) {
    handleRuntimeError(
      context,
      new errors.NotEnoughArguments(
        callExpression,
        name,
        argumentPassingStyle.numCompulsoryParameters,
        numArgs
      )
    )
  }
}

export function* listOfArguments(
  expressions: SyntaxNode[],
  context: Context
): Generator<Context, ExpressibleValue[]> {
  const values: ExpressibleValue[] = []
  for (const expression of expressions) {
    values.push(yield* evaluate(expression, context))
  }
  return values
}

export const extendEnvironmentWithNewBindings = (
  context: Context,
  environment: Environment,
  procedureName: string,
  paramArgPairs: ParameterArgumentPair[]
): Environment => {
  const frame = {}
  const newEnvironment: Environment = {
    name: procedureName,
    tail: environment,
    head: frame,
    procedureName:
      '(' + [procedureName, ...paramArgPairs.map(pair => stringify(pair.argument))].join(' ') + ')'
  }
  paramArgPairs.forEach(pair =>
    introduceBinding(
      context,
      frame,
      pair.parameter.isFromSource,
      pair.parameter.name,
      pair.argument
    )
  )
  return newEnvironment
}

export function* apply(
  context: Context,
  procedure: EVProcedure,
  suppliedArgs: ExpressibleValue[],
  node: SyntaxNode
): Generator<Context, NonTailCallExpressibleValue> {
  while (true) {
    checkNumberOfArguments(
      context,
      procedure.name,
      procedure.parameterPassingStyle,
      suppliedArgs.length,
      node
    )

    let result: ExpressibleValue
    if (procedure.variant === 'CompoundProcedure') {
      result = yield* applyCompoundProcedure(context, procedure, suppliedArgs)
    } else {
      result = yield* applyBuiltInProcedure(context, procedure, suppliedArgs, node)
    }

    if (result.type !== 'TailCall') {
      return result
    }

    procedure = result.procedure
    suppliedArgs = result.args
    node = result.node
  }
}

function* applyCompoundProcedure(
  context: Context,
  procedure: EVCompoundProcedure,
  suppliedArgs: ExpressibleValue[]
): ValueGenerator {
  const environment = extendEnvironmentWithNewBindings(
    context,
    procedure.environment,
    procedure.name,
    matchArgumentsToParameters(procedure.parameterPassingStyle, suppliedArgs)
  )
  pushEnvironment(context, environment)

  if (procedure.body.length === 0) {
    return handleRuntimeError(
      context,
      new errors.UnreachableCodeReached('lambda body should have one or more expressions')
    )
  }

  for (let i = 0; i < procedure.body.length - 1; i++) {
    const expression = procedure.body[i]
    yield* evaluate(expression, context)
  }
  // If tail call optimization is enabled,
  // enter tail context before evaluating the last expression
  const lastExpression = procedure.body[procedure.body.length - 1]
  if (context.variant !== 'no-tco') {
    enterTailContext(context)
  }
  const result = yield* evaluate(lastExpression, context)
  exitTailContext(context)

  popEnvironment(context)
  return result
}

function* applyBuiltInProcedure(
  context: Context,
  procedure: EVBuiltInProcedure,
  suppliedArgs: ExpressibleValue[],
  node: SyntaxNode
): ValueGenerator {
  try {
    const result = procedure.body(suppliedArgs, context)
    if ('next' in result && 'throw' in result) {
      return yield* result
    } else {
      return result
    }
  } catch (e) {
    return handleRuntimeError(context, new errors.BuiltinProcedureError(e, node))
  }
}

/** Match the arguments with parameters according to the argument passing style. */
export const matchArgumentsToParameters = (
  parameterPassingStyle: NamedParameterPassingStyle,
  args: ExpressibleValue[]
): ParameterArgumentPair[] => {
  if (parameterPassingStyle.style === 'fixed-args') {
    return parameterPassingStyle.parameters.map((parameter, index) => ({
      parameter,
      argument: args[index]
    }))
  } else {
    return parameterPassingStyle.compulsoryParameters
      .map((parameter, index) => ({ parameter, argument: args[index] }))
      .concat({
        parameter: parameterPassingStyle.restParameters,
        argument: makeList(...args.slice(parameterPassingStyle.numCompulsoryParameters))
      })
  }
}

/**
 * Check whether the parent node is in tail context
 */
export const isParentInTailContext = (context: Context): boolean => {
  return context.runtime.inTailContext.length >= 2 && context.runtime.inTailContext[1]
}

/** Unconditionally enter tail context.
 * Note that this function is intentionally unexported
 * because we can enter tail context unconditionally only
 * at the end of a procedure call, which happens only within this file. */
const enterTailContext = (context: Context) => {
  context.runtime.inTailContext[0] = true
}

/** Enter tail context for the current node only if parent node is in tail context */
export const tryEnterTailContext = (context: Context) => {
  if (isParentInTailContext(context)) {
    enterTailContext(context)
  }
}

/** Exit tail context for the current node */
export const exitTailContext = (context: Context) => {
  context.runtime.inTailContext[0] = false
}
