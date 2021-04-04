import {
  EVEmptyList,
  EVPair,
  EVProcedure,
  ExpressibleValue,
  makeEmptyList,
  makeList,
  makePair
} from '../ExpressibleValue'

export const cons: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  argumentPassingStyle: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: ExpressibleValue[]) => makePair(args[0], args[1])
}

export const mustDoOnPair = <T>(
  opName: string,
  pair: ExpressibleValue,
  op: (pair: EVPair) => T
): T => {
  if (pair.type !== 'EVPair') {
    throw new Error(opName + ' expects a pair as the only argument, but encountered ' + pair.type)
  }
  return op(pair)
}

export const car: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  argumentPassingStyle: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: ExpressibleValue[]) =>
    mustDoOnPair('car', args[0], (pair: EVPair): ExpressibleValue => pair.head)
}

export const cdr: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  argumentPassingStyle: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: ExpressibleValue[]) =>
    mustDoOnPair('cdr', args[0], (pair: EVPair): ExpressibleValue => pair.tail)
}

export const setCar: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  argumentPassingStyle: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: ExpressibleValue[]) =>
    mustDoOnPair(
      'set-car!',
      args[0],
      (pair: EVPair): EVEmptyList => {
        pair.head = args[1]
        return makeEmptyList()
      }
    )
}

export const setCdr: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  argumentPassingStyle: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: ExpressibleValue[]) =>
    mustDoOnPair(
      'set-cdr!',
      args[0],
      (pair: EVPair): EVEmptyList => {
        pair.tail = args[1]
        return makeEmptyList()
      }
    )
}

export const list: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  argumentPassingStyle: {
    style: 'var-args',
    numCompulsoryParameters: 0
  },
  body: (args: ExpressibleValue[]) => makeList(...args)
}
