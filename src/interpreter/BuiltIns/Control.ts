import { SchemeList } from '../../lang/scheme'
import { Context } from '../../types'
import { flattenPairToList } from '../../utils/listHelpers'
import { EVProcedure, ExpressibleValue } from '../ExpressibleValue'
import { ValueGenerator } from '../interpreter'
import { apply as applyProcedure } from '../procedure'

export const apply: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  argumentPassingStyle: {
    style: 'var-args',
    numCompulsoryParameters: 2
  },
  body: function* (args: ExpressibleValue[], context: Context): ValueGenerator {
    const proc = args[0]
    if (proc.type !== 'EVProcedure') {
      throw new Error(`\`apply' expected a procedure as the first argument, got ${proc.type}`)
    }
    const lastSuppliedArgument = args[args.length - 1]
    if (lastSuppliedArgument.type !== 'EVPair') {
      throw new Error(`\`apply' expected a list as the last argument, but got ${proc.type}`)
    }
    const list = flattenPairToList(lastSuppliedArgument)
    if (list.type === 'ImproperList') {
      throw new Error(`\`apply' expected a list as the last argument, but got an improper list`)
    }

    const spreadArgList = args.slice(1, -1)
    const tailArgList = list.value.map(listElement => listElement.value)
    const actualArguments = spreadArgList.concat(tailArgList)

    return yield* applyProcedure(
      context,
      proc,
      'apply',
      actualArguments,
      context.runtime.nodes[0] as SchemeList
    )
  }
}
