import { defaultVariant } from './constants'
import { importNativeBuiltins } from './interpreter/BuiltIns'
import { Procedure, Value } from './interpreter/Value'
import { stdlibMacros } from './stdlib/macros.prelude'
import * as misc from './stdlib/misc'
import { stdlibProcedures } from './stdlib/procedures.prelude'
import { Context, CustomBuiltIns, Variant } from './types'
import { stringify } from './utils/stringify'

const createEmptyRuntime = () => ({
  break: false,
  debuggerOn: true,
  isRunning: false,
  environments: [],
  value: undefined,
  nodes: [],
  inTailContext: [],
  nextUniqueSymbolNumber: 0
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

const defineSymbol = (context: Context, name: string, value: Value) => {
  const globalEnvironment = context.runtime.environments[0]
  globalEnvironment.head[name] = value
}

const importExternalBuiltins = (context: Context, externalBuiltIns: CustomBuiltIns) => {
  const externalDisplay = (v: Value) =>
    externalBuiltIns.rawDisplay(
      stringify(v),
      undefined as any, // Workaround for rawDisplay hack on frontend
      context.externalContext
    )

  const displayProcedure: Procedure = {
    type: 'procedure',
    name: 'display',
    callSignature: {
      style: 'fixed-args',
      numParams: 1
    },
    variant: 'BuiltInProcedure',
    body: (args: Value[]): Value => {
      externalDisplay(args[0])
      return args[0]
    }
  }

  defineSymbol(context, 'display', displayProcedure)
}

/**
 * Imports builtins from standard and external libraries.
 */

const defaultBuiltIns: CustomBuiltIns = {
  rawDisplay: misc.rawDisplay
}

const importPrelude = (context: Context) => {
  context.prelude = stdlibProcedures
  if (context.variant === 'macro') {
    context.prelude += stdlibMacros
  }
}

export const importBuiltins = (context: Context, externalBuiltIns: CustomBuiltIns) => {
  ensureGlobalEnvironmentExist(context)

  importExternalBuiltins(context, externalBuiltIns)
  importNativeBuiltins(context)
}

const createContext = <T>(
  variant: Variant = defaultVariant,
  externalSymbols: string[] = [],
  externalContext?: T,
  externalBuiltIns: CustomBuiltIns = defaultBuiltIns,
  moduleParams?: any
) => {
  const context = createEmptyContext(variant, externalSymbols, externalContext, moduleParams)

  importPrelude(context)
  importBuiltins(context, externalBuiltIns)

  return context
}

export default createContext
