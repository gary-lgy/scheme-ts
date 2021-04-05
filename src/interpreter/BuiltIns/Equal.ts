import { EVBool, EVProcedure, ExpressibleValue, makeBool } from '../ExpressibleValue'

export const eq: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'eq?',
  parameterPassingStyle: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: ExpressibleValue[]): EVBool => {
    const lhs = args[0]
    const rhs = args[1]
    if (
      (lhs.type === 'EVNumber' && rhs.type === 'EVNumber') ||
      (lhs.type === 'EVBool' && rhs.type === 'EVBool') ||
      (lhs.type === 'EVString' && rhs.type === 'EVString') ||
      (lhs.type === 'EVSymbol' && rhs.type === 'EVSymbol')
    ) {
      return makeBool(lhs.value === rhs.value)
    } else if (lhs.type === 'EVEmptyList' && rhs.type === 'EVEmptyList') {
      return makeBool(true)
    } else if (
      (lhs.type === 'EVProcedure' && rhs.type === 'EVProcedure') ||
      (lhs.type === 'EVPair' && rhs.type === 'EVPair')
    ) {
      return makeBool(lhs === rhs)
    } else {
      return makeBool(false)
    }
  }
}

export const eqv: EVProcedure = {
  ...eq,
  name: 'eqv?'
}

const equalImpl = (lhs: ExpressibleValue, rhs: ExpressibleValue): boolean => {
  if (
    (lhs.type === 'EVNumber' && rhs.type === 'EVNumber') ||
    (lhs.type === 'EVBool' && rhs.type === 'EVBool') ||
    (lhs.type === 'EVString' && rhs.type === 'EVString') ||
    (lhs.type === 'EVSymbol' && rhs.type === 'EVSymbol')
  ) {
    return lhs.value === rhs.value
  } else if (lhs.type === 'EVEmptyList' && rhs.type === 'EVEmptyList') {
    return true
  } else if (lhs.type === 'EVProcedure' && rhs.type === 'EVProcedure') {
    return lhs === rhs
  } else if (lhs.type === 'EVPair' && rhs.type === 'EVPair') {
    return equalImpl(lhs.head, rhs.head) && equalImpl(lhs.tail, rhs.tail)
  } else {
    return false
  }
}

export const equal: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'equal?',
  parameterPassingStyle: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: ExpressibleValue[]): EVBool => makeBool(equalImpl(args[0], args[1]))
}
