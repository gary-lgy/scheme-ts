import { Context } from 'vm'
import * as errors from '../errors/errors'
import { RuntimeSourceError } from '../errors/runtimeSourceError'
import { Environment, Frame } from '../types'
import { ExpressibleValue } from './ExpressibleValue'

// ======================= Environment ===========================

export const currentEnvironment = (context: Context) => context.runtime.environments[0]
export const replaceEnvironment = (context: Context, environment: Environment) =>
  (context.runtime.environments[0] = environment)
export const popEnvironment = (context: Context) => context.runtime.environments.shift()
export const pushEnvironment = (context: Context, environment: Environment) =>
  context.runtime.environments.unshift(environment)

export const extendCurrentEnvironment = (
  context: Context,
  name: string,
  head: Frame = {}
): Environment => {
  return {
    name,
    tail: currentEnvironment(context),
    head
  }
}

export const getVariable = (context: Context, name: string) => {
  let environment: Environment | null = context.runtime.environments[0]
  while (environment) {
    if (environment.head.hasOwnProperty(name)) {
      return environment.head[name]
    } else {
      environment = environment.tail
    }
  }
  return undefined
}

export const setVariable = (context: Context, name: string, value: any) => {
  let environment: Environment | null = context.runtime.environments[0]
  while (environment) {
    if (environment.head.hasOwnProperty(name)) {
      environment.head[name] = value
      return
    } else {
      environment = environment.tail
    }
  }
  return handleRuntimeError(context, new errors.UndefinedVariable(name, context.runtime.nodes[0]))
}

export const isDefined = (context: Context, name: string): boolean => {
  return !!getVariable(context, name)
}

// ======================= Evaluation ===========================

export const isTruthy = (value: ExpressibleValue) => value.type !== 'EVBool' || value.value

export const handleRuntimeError = (context: Context, error: RuntimeSourceError): never => {
  context.errors.push(error)
  context.runtime.environments = context.runtime.environments.slice(
    -context.numberOfOuterEnvironments
  )
  throw error
}
