import { Context } from '../../types'
import { flattenPairToList } from '../../utils/listHelpers'
import { stringify } from '../../utils/stringify'
import { EVProcedure, ExpressibleValue } from '../ExpressibleValue'
import { ValueGenerator } from '../interpreter'
import { apply as applyProcedure, isParentInTailContext } from '../procedure'

export const apply: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'apply',
  parameterPassingStyle: {
    style: 'var-args',
    numCompulsoryParameters: 1
  },
  body: function* (args: ExpressibleValue[], context: Context): ValueGenerator {
    const proc = args[0]
    if (proc.type !== 'EVProcedure') {
      throw new Error(`\`apply' expected a procedure as the first argument, got ${proc.type}`)
    }

    let actualArguments: ExpressibleValue[]
    if (args.length === 1) {
      // no arguments provided
      actualArguments = []
    } else {
      const lastSuppliedArgument = args[args.length - 1]
      if (lastSuppliedArgument.type !== 'EVPair' && lastSuppliedArgument.type !== 'EVEmptyList') {
        throw new Error(
          `\`apply' expected a list as the last argument, but got ${lastSuppliedArgument.type}`
        )
      }

      let tailArgList: ExpressibleValue[]
      if (lastSuppliedArgument.type === 'EVEmptyList') {
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

export const error: EVProcedure = {
  type: 'EVProcedure',
  name: 'error',
  parameterPassingStyle: {
    style: 'var-args',
    numCompulsoryParameters: 1
  },
  variant: 'BuiltInProcedure',
  body: (args: ExpressibleValue[]): never => {
    const output = args.map(value => stringify(value)).join(' ')
    throw new Error(output)
  }
}
