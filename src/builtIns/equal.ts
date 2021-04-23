import { Procedure, Value } from '../interpreter/value'
import { makeBool, SBool } from '../sExpression'

export const eq: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'eq?',
  callSignature: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: Value[]): SBool => {
    const lhs = args[0]
    const rhs = args[1]
    if (
      (lhs.type === 'number' && rhs.type === 'number') ||
      (lhs.type === 'boolean' && rhs.type === 'boolean') ||
      (lhs.type === 'string' && rhs.type === 'string') ||
      (lhs.type === 'symbol' && rhs.type === 'symbol')
    ) {
      return makeBool(lhs.value === rhs.value)
    } else if (lhs.type === 'empty list' && rhs.type === 'empty list') {
      return makeBool(true)
    } else if (
      (lhs.type === 'procedure' && rhs.type === 'procedure') ||
      (lhs.type === 'pair' && rhs.type === 'pair')
    ) {
      return makeBool(lhs === rhs)
    } else {
      return makeBool(false)
    }
  }
}

export const eqv: Procedure = {
  ...eq,
  name: 'eqv?'
}

export const areValuesEqual = (lhs: Value, rhs: Value): boolean => {
  if (
    (lhs.type === 'number' && rhs.type === 'number') ||
    (lhs.type === 'boolean' && rhs.type === 'boolean') ||
    (lhs.type === 'string' && rhs.type === 'string') ||
    (lhs.type === 'symbol' && rhs.type === 'symbol')
  ) {
    return lhs.value === rhs.value
  } else if (lhs.type === 'empty list' && rhs.type === 'empty list') {
    return true
  } else if (lhs.type === 'procedure' && rhs.type === 'procedure') {
    return lhs === rhs
  } else if (lhs.type === 'pair' && rhs.type === 'pair') {
    return areValuesEqual(lhs.head, rhs.head) && areValuesEqual(lhs.tail, rhs.tail)
  } else {
    return false
  }
}

export const equal: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'equal?',
  callSignature: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: Value[]): SBool => makeBool(areValuesEqual(args[0], args[1]))
}
