import { makeNumber, SNumber } from '../SExpression'
import { Procedure, Value } from '../Value'

const mustMapToNumbers = (opName: string, args: Value[]): number[] => {
  const mapped: number[] = []
  for (const arg of args) {
    if (arg.type !== 'number') {
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
): SNumber => {
  return makeNumber(args.reduce(op, init))
}

export const add: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: '+',
  callSignature: {
    style: 'var-args',
    numCompulsoryParameters: 0
  },
  body: (args: Value[]) =>
    reduceNumericalArgs((acc, next) => acc + next, 0, mustMapToNumbers('+', args))
}

export const subtract: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: '-',
  callSignature: {
    style: 'var-args',
    numCompulsoryParameters: 1
  },
  body: (args: Value[]) => {
    const mappedArgs = mustMapToNumbers('-', args)
    if (mappedArgs.length === 1) {
      return makeNumber(-mappedArgs[0])
    }
    return reduceNumericalArgs((acc, next) => acc - next, mappedArgs[0], mappedArgs.slice(1))
  }
}

export const multiply: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: '*',
  callSignature: {
    style: 'var-args',
    numCompulsoryParameters: 0
  },
  body: (args: Value[]) =>
    reduceNumericalArgs((acc, next) => acc * next, 1, mustMapToNumbers('*', args))
}

export const divide: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: '/',
  callSignature: {
    style: 'var-args',
    numCompulsoryParameters: 1
  },
  body: (args: Value[]) => {
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

export const quotient: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'quotient',
  callSignature: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: Value[]) => {
    const mappedArgs = mustMapToNumbers('quotient', args)
    if (mappedArgs[1] === 0) {
      throw new Error('division by zero')
    }
    return makeNumber(Math.floor(mappedArgs[0] / mappedArgs[1]))
  }
}

export const remainder: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'remainder',
  callSignature: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: Value[]) => {
    const mappedArgs = mustMapToNumbers('remainder', args)
    if (mappedArgs[1] === 0) {
      throw new Error('division by zero')
    }
    return makeNumber(mappedArgs[0] % mappedArgs[1])
  }
}

export const modulo: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'modulo',
  callSignature: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: Value[]) => {
    const mappedArgs = mustMapToNumbers('modulo', args)
    if (mappedArgs[1] === 0) {
      throw new Error('division by zero')
    }
    return makeNumber(((mappedArgs[0] % mappedArgs[1]) + mappedArgs[1]) % mappedArgs[1])
  }
}

export const numberEqual: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: '=',
  callSignature: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: Value[]) => {
    const mappedArgs = mustMapToNumbers('=', args)
    return {
      type: 'boolean',
      value: mappedArgs[0] === mappedArgs[1]
    }
  }
}

export const lessThan: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: '<',
  callSignature: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: Value[]) => {
    const mappedArgs = mustMapToNumbers('<', args)
    return {
      type: 'boolean',
      value: mappedArgs[0] < mappedArgs[1]
    }
  }
}

export const lessThanOrEqual: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: '<=',
  callSignature: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: Value[]) => {
    const mappedArgs = mustMapToNumbers('<=', args)
    return {
      type: 'boolean',
      value: mappedArgs[0] <= mappedArgs[1]
    }
  }
}

export const greaterThan: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: '>',
  callSignature: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: Value[]) => {
    const mappedArgs = mustMapToNumbers('>', args)
    return {
      type: 'boolean',
      value: mappedArgs[0] > mappedArgs[1]
    }
  }
}

export const greaterThanOrEqual: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: '>=',
  callSignature: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: Value[]) => {
    const mappedArgs = mustMapToNumbers('>=', args)
    return {
      type: 'boolean',
      value: mappedArgs[0] >= mappedArgs[1]
    }
  }
}
