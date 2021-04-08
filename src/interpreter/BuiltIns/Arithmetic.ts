import { EVNumber, EVProcedure, ExpressibleValue, makeNumber } from '../ExpressibleValue'

const mustMapToNumbers = (opName: string, args: ExpressibleValue[]): number[] => {
  const mapped: number[] = []
  for (const arg of args) {
    if (arg.type !== 'EVNumber') {
      throw new Error(opName + ' expects numbers as arguments, but encountered ' + arg.type)
    }
    mapped.push(arg.value)
  }
  return mapped
}

const reduceNumericalArgs = (
  op: (accumulate: number, next: number) => number,
  init: number,
  args: number[]
): EVNumber => {
  return makeNumber(args.reduce(op, init))
}

export const add: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: '+',
  callSignature: {
    style: 'var-args',
    numCompulsoryParameters: 0
  },
  body: (args: ExpressibleValue[]) =>
    reduceNumericalArgs((acc, next) => acc + next, 0, mustMapToNumbers('+', args))
}

export const subtract: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: '-',
  callSignature: {
    style: 'var-args',
    numCompulsoryParameters: 1
  },
  body: (args: ExpressibleValue[]) => {
    const mappedArgs = mustMapToNumbers('-', args)
    if (mappedArgs.length === 1) {
      return makeNumber(-mappedArgs[0])
    }
    return reduceNumericalArgs((acc, next) => acc - next, mappedArgs[0], mappedArgs.slice(1))
  }
}

export const multiply: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: '*',
  callSignature: {
    style: 'var-args',
    numCompulsoryParameters: 0
  },
  body: (args: ExpressibleValue[]) =>
    reduceNumericalArgs((acc, next) => acc * next, 1, mustMapToNumbers('*', args))
}

export const divide: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: '/',
  callSignature: {
    style: 'var-args',
    numCompulsoryParameters: 1
  },
  body: (args: ExpressibleValue[]) => {
    const mappedArgs = mustMapToNumbers('/', args)
    if (mappedArgs.length === 1) {
      if (mappedArgs[0] === 0) {
        throw new Error('division by zero')
      }
      return makeNumber(1 / mappedArgs[0])
    }
    return reduceNumericalArgs(
      (acc, next) => {
        if (next === 0) {
          throw new Error('division by zero')
        }
        return acc / next
      },
      mappedArgs[0],
      mappedArgs.slice(1)
    )
  }
}

export const quotient: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'quotient',
  callSignature: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: ExpressibleValue[]) => {
    const mappedArgs = mustMapToNumbers('quotient', args)
    if (mappedArgs[1] === 0) {
      throw new Error('division by zero')
    }
    return makeNumber(Math.floor(mappedArgs[0] / mappedArgs[1]))
  }
}

export const remainder: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'remainder',
  callSignature: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: ExpressibleValue[]) => {
    const mappedArgs = mustMapToNumbers('remainder', args)
    if (mappedArgs[1] === 0) {
      throw new Error('division by zero')
    }
    return makeNumber(mappedArgs[0] % mappedArgs[1])
  }
}

export const numberEqual: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: '=',
  callSignature: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: ExpressibleValue[]) => {
    const mappedArgs = mustMapToNumbers('=', args)
    return {
      type: 'EVBool',
      value: mappedArgs[0] === mappedArgs[1]
    }
  }
}

export const lessThan: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: '<',
  callSignature: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: ExpressibleValue[]) => {
    const mappedArgs = mustMapToNumbers('<', args)
    return {
      type: 'EVBool',
      value: mappedArgs[0] < mappedArgs[1]
    }
  }
}

export const lessThanOrEqual: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: '<=',
  callSignature: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: ExpressibleValue[]) => {
    const mappedArgs = mustMapToNumbers('<=', args)
    return {
      type: 'EVBool',
      value: mappedArgs[0] <= mappedArgs[1]
    }
  }
}

export const greaterThan: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: '>',
  callSignature: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: ExpressibleValue[]) => {
    const mappedArgs = mustMapToNumbers('>', args)
    return {
      type: 'EVBool',
      value: mappedArgs[0] > mappedArgs[1]
    }
  }
}

export const greaterThanOrEqual: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: '>=',
  callSignature: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: ExpressibleValue[]) => {
    const mappedArgs = mustMapToNumbers('>=', args)
    return {
      type: 'EVBool',
      value: mappedArgs[0] >= mappedArgs[1]
    }
  }
}
