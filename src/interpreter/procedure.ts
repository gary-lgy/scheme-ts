import { Context } from '..'
import * as errors from '../errors/errors'
import { SchemeExpression, SchemeIdentifier } from '../lang/scheme'
import { Environment } from '../types'
import {
  EVBuiltInProcedure,
  EVCompoundProcedure,
  EVProcedure,
  ExpressibleValue,
  makeList,
  NonTailCallExpressibleValue
} from './ExpressibleValue'
import { evaluate, ValueGenerator } from './interpreter'
import { handleRuntimeError, popEnvironment, pushEnvironment } from './util'

// For BuiltIn procedures, we only need to check the number of arguments
// The parameter names are meaningless and unnecessary
export type BuiltInProcedureArgumentPassingStyle = FixedArgs | VarArgs

type FixedArgs = { style: 'fixed-args'; numParams: number }
type VarArgs = { style: 'var-args'; numCompulsoryParameters: number }

// For compound procedures, we need both the number of arguments and the parameter names
// in order to extend the function environment with the arguments
export type CompoundProcedureArgumentPassingStyle =
  | FixedArgsWithParameterNames
  | VarArgsWithParameterNames

type FixedArgsWithParameterNames = FixedArgs & { parameters: SchemeIdentifier[] }
type VarArgsWithParameterNames = VarArgs & {
  compulsoryParameters: SchemeIdentifier[]
  restParameters: SchemeIdentifier
}

const checkNumberOfArguments = (
  context: Context,
  procedure: EVProcedure,
  procedureName: string,
  numArgs: number,
  callExpression: SchemeExpression
) => {
  if (
    procedure.argumentPassingStyle.style === 'fixed-args' &&
    procedure.argumentPassingStyle.numParams !== numArgs
  ) {
    handleRuntimeError(
      context,
      new errors.InvalidNumberOfArguments(
        callExpression,
        procedureName,
        procedure.argumentPassingStyle.numParams,
        numArgs
      )
    )
  } else if (
    procedure.argumentPassingStyle.style === 'var-args' &&
    numArgs < procedure.argumentPassingStyle.numCompulsoryParameters
  ) {
    handleRuntimeError(
      context,
      new errors.NotEnoughArguments(
        callExpression,
        procedureName,
        procedure.argumentPassingStyle.numCompulsoryParameters,
        numArgs
      )
    )
  }
}

export function* listOfArguments(
  expressions: SchemeExpression[],
  context: Context
): Generator<Context, ExpressibleValue[]> {
  const values: ExpressibleValue[] = []
  for (const expression of expressions) {
    values.push(yield* evaluate(expression, context))
  }
  return values
}

const extendProcedureEnvironment = (
  environment: Environment,
  procedureName: string,
  parameters: string[],
  args: ExpressibleValue[]
): Environment => {
  const frame = {}
  const newEnvironment: Environment = {
    name: procedureName,
    tail: environment,
    head: frame,
    procedureName: procedureName
  }
  parameters.forEach((param, index) => {
    frame[param] = args[index]
  })
  return newEnvironment
}

// TODO: let each proc remember its name
export function* apply(
  context: Context,
  procedure: EVProcedure,
  procedureName: string,
  suppliedArgs: ExpressibleValue[],
  node: SchemeExpression
): Generator<Context, NonTailCallExpressibleValue> {
  while (true) {
    checkNumberOfArguments(context, procedure, procedureName, suppliedArgs.length, node)

    let result: ExpressibleValue
    if (procedure.variant === 'CompoundProcedure') {
      result = yield* applyCompoundProcedure(context, procedure, procedureName, suppliedArgs)
    } else {
      result = yield* applyBuiltInProcedure(context, procedure, suppliedArgs, node)
    }

    if (result.type !== 'TailCall') {
      return result
    }

    procedure = result.procedure
    procedureName = result.procedureName
    suppliedArgs = result.args
    node = result.node
  }
}

function* applyCompoundProcedure(
  context: Context,
  procedure: EVCompoundProcedure,
  procedureName: string,
  suppliedArgs: ExpressibleValue[]
): ValueGenerator {
  const { parameters, args: argsToPass } = makeArguments(procedure, suppliedArgs)
  const environment = extendProcedureEnvironment(
    procedure.environment,
    procedureName,
    parameters,
    argsToPass
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
  // Enter tail context before evaluating the last expression
  const lastExpression = procedure.body[procedure.body.length - 1]
  enterTailContext(context)
  const result = yield* evaluate(lastExpression, context)
  exitTailContext(context)

  popEnvironment(context)
  return result
}

function* applyBuiltInProcedure(
  context: Context,
  procedure: EVBuiltInProcedure,
  suppliedArgs: ExpressibleValue[],
  node: SchemeExpression
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
const makeArguments = (
  procedure: EVCompoundProcedure,
  args: ExpressibleValue[]
): { parameters: string[]; args: ExpressibleValue[] } => {
  const argumentPassingStyle = procedure.argumentPassingStyle

  if (argumentPassingStyle.style === 'fixed-args') {
    return {
      parameters: argumentPassingStyle.parameters.map(param => param.name),
      args
    }
  } else {
    return {
      parameters: [
        ...argumentPassingStyle.compulsoryParameters.map(param => param.name),
        argumentPassingStyle.restParameters.name
      ],
      args: [
        ...args.slice(0, argumentPassingStyle.numCompulsoryParameters),
        makeList(...args.slice(argumentPassingStyle.numCompulsoryParameters))
      ]
    }
  }
}

/**
 * Check whether the parent node is in tail context
 */
export const isParentInTailContext = (context: Context): boolean => {
  return context.runtime.inTailContext.length >= 2 && context.runtime.inTailContext[1]
}

/**
 * Check whether the current node is in tail context
 */
export const isNodeInTailContext = (context: Context): boolean => {
  return context.runtime.inTailContext[0]
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
