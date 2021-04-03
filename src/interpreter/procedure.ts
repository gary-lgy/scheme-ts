import { Context } from '..'
import * as errors from '../errors/errors'
import { SchemeExpression, SchemeIdentifier, SchemeList } from '../lang/scheme'
import { Environment } from '../types'
import { EVCompoundProcedure, EVProcedure, ExpressibleValue, makeList } from './ExpressibleValue'
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
  callExpression: SchemeList
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

export function* apply(
  context: Context,
  procedure: EVProcedure,
  procedureName: string,
  suppliedArgs: ExpressibleValue[],
  node: SchemeList
): ValueGenerator {
  checkNumberOfArguments(context, procedure, procedureName, suppliedArgs.length, node)

  if (procedure.variant === 'CompoundProcedure') {
    // TODO: TCO
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

    let result: ExpressibleValue
    for (const expression of procedure.body) {
      result = yield* evaluate(expression, context)
    }

    popEnvironment(context)

    return result!
  } else {
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
}

// Organise the arguments according to the argument passing style.
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
