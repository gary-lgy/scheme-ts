// Variable determining chapter of Source is contained in this file.

import { GLOBAL } from './constants'
import { importNativeBuiltins } from './interpreter/BuiltIns'
import { EVProcedure, ExpressibleValue } from './interpreter/ExpressibleValue'
import * as misc from './stdlib/misc'
import { stdlibPrelude } from './stdlib/prelude'
import { Context, CustomBuiltIns, Value, Variant } from './types'
import { stringify } from './utils/stringify'

const createEmptyRuntime = () => ({
  break: false,
  debuggerOn: true,
  isRunning: false,
  environments: [],
  value: undefined,
  nodes: [],
  inTailContext: []
})

const createGlobalEnvironment = () => ({
  tail: null,
  name: 'global',
  head: {}
})

export const createEmptyContext = <T>(
  variant: Variant,
  externalSymbols: string[],
  externalContext?: T,
  moduleParams?: any
): Context<T> => {
  return {
    externalSymbols,
    errors: [],
    externalContext,
    moduleParams,
    runtime: createEmptyRuntime(),
    numberOfOuterEnvironments: 1,
    prelude: null,
    executionMethod: 'auto',
    variant
  }
}

export const ensureGlobalEnvironmentExist = (context: Context) => {
  if (!context.runtime) {
    context.runtime = createEmptyRuntime()
  }
  if (!context.runtime.environments) {
    context.runtime.environments = []
  }
  if (context.runtime.environments.length === 0) {
    context.runtime.environments.push(createGlobalEnvironment())
  }
}

const defineSymbol = (context: Context, name: string, value: ExpressibleValue) => {
  const globalEnvironment = context.runtime.environments[0]
  globalEnvironment.head[name] = value
}

export const importExternalSymbols = (context: Context, externalSymbols: string[]) => {
  ensureGlobalEnvironmentExist(context)

  externalSymbols.forEach(symbol => {
    defineSymbol(context, symbol, GLOBAL[symbol])
  })
}

const importExternalBuiltins = (context: Context, externalBuiltIns: CustomBuiltIns) => {
  const rawDisplay = (v: Value) =>
    externalBuiltIns.rawDisplay(
      v,
      undefined as any, // Workaround for rawDisplay hack on frontend
      context.externalContext
    )

  const displayProcedure: EVProcedure = {
    type: 'EVProcedure',
    name: 'display',
    parameterPassingStyle: {
      style: 'fixed-args',
      numParams: 1
    },
    variant: 'BuiltInProcedure',
    body: (args: ExpressibleValue[]): ExpressibleValue => {
      rawDisplay(stringify(args[0]))
      return args[0]
    }
  }

  const errorProcedure: EVProcedure = {
    type: 'EVProcedure',
    name: 'error',
    parameterPassingStyle: {
      style: 'var-args',
      numCompulsoryParameters: 1
    },
    variant: 'BuiltInProcedure',
    body: (args: ExpressibleValue[]): never => {
      if (args[0].type !== 'EVString') {
        throw new Error(`error expected the first argument to be a string, got ${args[0].type}`)
      }
      misc.error_message(args[0].value, args.slice(1))
    }
  }

  defineSymbol(context, 'display', displayProcedure)
  defineSymbol(context, 'error', errorProcedure)
}

/**
 * Imports builtins from standard and external libraries.
 */

const defaultBuiltIns: CustomBuiltIns = {
  rawDisplay: misc.rawDisplay,
  // See issue #5
  prompt: misc.rawDisplay,
  // See issue #11
  alert: misc.rawDisplay,
  visualiseList: (v: Value) => {
    throw new Error('List visualizer is not enabled')
  }
}

const importPrelude = (context: Context) => {
  context.prelude = stdlibPrelude
}

export const importBuiltins = (context: Context, externalBuiltIns: CustomBuiltIns) => {
  ensureGlobalEnvironmentExist(context)

  importExternalBuiltins(context, externalBuiltIns)
  importNativeBuiltins(context)
}

const createContext = <T>(
  variant: Variant = 'base',
  externalSymbols: string[] = [],
  externalContext?: T,
  externalBuiltIns: CustomBuiltIns = defaultBuiltIns,
  moduleParams?: any
) => {
  const context = createEmptyContext(variant, externalSymbols, externalContext, moduleParams)

  importPrelude(context)
  importBuiltins(context, externalBuiltIns)
  importExternalSymbols(context, externalSymbols)

  return context
}

export default createContext
