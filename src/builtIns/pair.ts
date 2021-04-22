import { makeList, makePair, Pair, Procedure, Value } from '../interpreter/value'
import { makeEmptyList, SEmptyList } from '../sExpression'

export const cons: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'cons',
  callSignature: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: Value[]) => makePair(args[0], args[1])
}

export const mustDoOnPair = <T>(opName: string, pair: Value, op: (pair: Pair) => T): T => {
  if (pair.type !== 'pair') {
    throw new Error(opName + ' expects a pair as the only argument, but encountered ' + pair.type)
  }
  return op(pair)
}

export const car: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'car',
  callSignature: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: Value[]) => mustDoOnPair('car', args[0], (pair: Pair): Value => pair.head)
}

export const cdr: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'cdr',
  callSignature: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: Value[]) => mustDoOnPair('cdr', args[0], (pair: Pair): Value => pair.tail)
}

export const setCar: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'set-car!',
  callSignature: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: Value[]) =>
    mustDoOnPair(
      'set-car!',
      args[0],
      (pair: Pair): SEmptyList => {
        pair.head = args[1]
        return makeEmptyList()
      }
    )
}

export const setCdr: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'set-cdr!',
  callSignature: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: Value[]) =>
    mustDoOnPair(
      'set-cdr!',
      args[0],
      (pair: Pair): SEmptyList => {
        pair.tail = args[1]
        return makeEmptyList()
      }
    )
}

export const list: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'list',
  callSignature: {
    style: 'var-args',
    numCompulsoryParameters: 0
  },
  body: (args: Value[]) => makeList(args)
}
