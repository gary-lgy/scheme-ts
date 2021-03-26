import { Context } from '..'
import * as errors from '../errors/errors'
import { SchemeExpression, SchemeList } from '../lang/scheme'
import { Environment } from '../types'
import { evaluate } from './interpreter'
import { EVProcedure, ExpressibleValue } from './runtime'
import { handleRuntimeError, popEnvironment, pushEnvironment } from './util'

const checkNumberOfArguments = (
  context: Context,
  procedure: EVProcedure,
  procedureName: string,
  callExpression: SchemeList
) => {
  const numArgs = callExpression.elements.length - 1
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
    numArgs < procedure.argumentPassingStyle.minNumParams
  ) {
    handleRuntimeError(
      context,
      new errors.NotEnoughArguments(
        callExpression,
        procedureName,
        procedure.argumentPassingStyle.minNumParams,
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
  parameters: string[],
  procedureName: string,
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
  args: ExpressibleValue[],
  node: SchemeList
) {
  // TODO: TCO
  checkNumberOfArguments(context, procedure, procedureName, node)
  if (procedure.variant === 'CompoundProcedure') {
    const environment = extendProcedureEnvironment(
      procedure.environment,
      procedure.parameters,
      procedureName,
      args
    )
    pushEnvironment(context, environment)
    const result = yield* evaluate(procedure.body, context)
    popEnvironment(context)
    return result
  } else {
    try {
      return procedure.body(args)
    } catch (e) {
      return handleRuntimeError(context, new errors.BuiltinProcedureError(e, node))
    }
  }
}
