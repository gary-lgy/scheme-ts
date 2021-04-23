import { ValueGenerator } from '../interpreter/interpreter'
import { apply as applyProcedure, isParentInTailContext } from '../interpreter/procedure'
import { Procedure, Value } from '../interpreter/value'
import { Context } from '../types'
import { flattenPairToList } from '../utils/listHelpers'
import { stringify } from '../utils/stringify'

export const apply: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'apply',
  callSignature: {
    style: 'var-args',
    numCompulsoryParameters: 1
  },
  body: function* (args: Value[], context: Context): ValueGenerator {
    const proc = args[0]
    if (proc.type !== 'procedure') {
      throw new Error(`\`apply' expected a procedure as the first argument, got ${proc.type}`)
    }

    let actualArguments: Value[]
    if (args.length === 1) {
      // no arguments provided
      actualArguments = []
    } else {
      const lastSuppliedArgument = args[args.length - 1]
      if (lastSuppliedArgument.type !== 'pair' && lastSuppliedArgument.type !== 'empty list') {
        throw new Error(
          `\`apply' expected a list as the last argument, but got ${lastSuppliedArgument.type}`
        )
      }

      let tailArgList: Value[]
      if (lastSuppliedArgument.type === 'empty list') {
        tailArgList = []
      } else {
        const list = flattenPairToList(lastSuppliedArgument)
        if (list.type === 'ImproperList') {
          throw new Error(`\`apply' expected a list as the last argument, but got an improper list`)
        }
        tailArgList = list.value.map(listElement => listElement.value)
      }

      const spreadArgList = args.slice(1, -1)
      actualArguments = spreadArgList.concat(tailArgList)
    }

    if (isParentInTailContext(context)) {
      return {
        type: 'TailCall',
        procedure: proc,
        args: actualArguments,
        node: context.runtime.nodes[0]
      }
    } else {
      return yield* applyProcedure(context, proc, actualArguments, context.runtime.nodes[0])
    }
  }
}

export const error: Procedure = {
  type: 'procedure',
  name: 'error',
  callSignature: {
    style: 'var-args',
    numCompulsoryParameters: 1
  },
  variant: 'BuiltInProcedure',
  body: (args: Value[]): never => {
    const output = args.map(value => stringify(value)).join(' ')
    throw new Error(output)
  }
}
