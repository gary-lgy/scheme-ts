import { Context } from '../../types'
import { flattenPairToList } from '../../utils/listHelpers'
import { ValueGenerator } from '../interpreter'
import { expandMacro } from '../macro'
import { makeSymbol, SSymbol } from '../SExpression'
import { getVariable, syntheticIdentifierPrefix } from '../util'
import { Procedure, Value } from '../Value'

export const macroexpand: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'macroexpand',
  callSignature: {
    style: 'fixed-args',
    numParams: 1
  },
  body: function* (args: Value[], context: Context): ValueGenerator {
    const form = args[0]
    if (form.type !== 'pair') {
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
    if (macroName.type !== 'symbol') {
      throw new Error(`first element in the argument list must be a macro name`)
    }

    const macro = getVariable(context, macroName.value)
    if (!macro || macro.type !== 'macro') {
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

export const genSym: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'gensym',
  callSignature: {
    style: 'fixed-args',
    numParams: 0
  },
  body: (_args: Value[], context: Context): SSymbol => {
    const seqNumber: number = context.runtime.nextUniqueSymbolNumber++
    const symbolName: string = syntheticIdentifierPrefix + seqNumber
    return makeSymbol(symbolName, false)
  }
}
