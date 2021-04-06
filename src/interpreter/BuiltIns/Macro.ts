import { Context } from '../../types'
import { flattenPairToList } from '../../utils/listHelpers'
import { EVProcedure, EVSymbol, ExpressibleValue, makeSymbol } from '../ExpressibleValue'
import { ValueGenerator } from '../interpreter'
import { expandMacro } from '../macro'
import { getVariable, syntheticIdentifierPrefix } from '../util'

export const macroexpand: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'macroexpand',
  parameterPassingStyle: {
    style: 'fixed-args',
    numParams: 1
  },
  body: function* (args: ExpressibleValue[], context: Context): ValueGenerator {
    const form = args[0]
    if (form.type !== 'EVPair') {
      throw new Error(`macroexpand expected a list as the only argument, but got ${form.type}`)
    }

    const list = flattenPairToList(form)
    if (list.type !== 'List') {
      throw new Error(`macroexpand expected a list as the only argument, but got an improper list`)
    }

    if (list.value.length === 0) {
      throw new Error(
        `macroexpand expected a non-empty list as the only argument, but got an empty list`
      )
    }

    const macroName = list.value[0].value
    if (macroName.type !== 'EVSymbol') {
      throw new Error(`first element in the argument list must be a macro name`)
    }

    const macro = getVariable(context, macroName.value)
    if (!macro || macro.type !== 'EVMacro') {
      throw new Error(`no macro named ${macroName.value}`)
    }

    return yield* expandMacro(
      context,
      macro,
      list.value.map(element => element.value).slice(1),
      context.runtime.nodes[0]
    )
  }
}

export const genSym: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'gensym',
  parameterPassingStyle: {
    style: 'fixed-args',
    numParams: 0
  },
  body: (_args: ExpressibleValue[], context: Context): EVSymbol => {
    const seqNumber: number = context.runtime.nextUniqueSymbolNumber++
    const symbolName: string = syntheticIdentifierPrefix + seqNumber
    return makeSymbol(symbolName, false)
  }
}
