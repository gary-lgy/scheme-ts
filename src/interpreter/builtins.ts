import { EVNumber, EVPair, EVProcedure, ExpressibleValue, Frame } from './runtime'

const defineBuiltin = (frame: Frame, name: string, value: ExpressibleValue) => {
  frame.set(name, { value })
}

const mapNumericalArguments = (opName: string, args: ExpressibleValue[]): number[] => {
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
  return {
    type: 'EVNumber',
    value: args.reduce(op, init)
  }
}

const add: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  argumentPassingStyle: {
    style: 'var-args',
    minNumParams: 0
  },
  body: (args: ExpressibleValue[]) =>
    reduceNumericalArgs((acc, next) => acc + next, 0, mapNumericalArguments('+', args))
}

const subtract: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  argumentPassingStyle: {
    style: 'var-args',
    minNumParams: 1
  },
  body: (args: ExpressibleValue[]) => {
    const mappedArgs = mapNumericalArguments('-', args)
    return reduceNumericalArgs((acc, next) => acc - next, mappedArgs[0], mappedArgs.slice(1))
  }
}

const multiply: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  argumentPassingStyle: {
    style: 'var-args',
    minNumParams: 0
  },
  body: (args: ExpressibleValue[]) =>
    reduceNumericalArgs((acc, next) => acc * next, 1, mapNumericalArguments('*', args))
}

const divide: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  argumentPassingStyle: {
    style: 'var-args',
    minNumParams: 1
  },
  body: (args: ExpressibleValue[]) => {
    const mappedArgs = mapNumericalArguments('/', args)
    return reduceNumericalArgs((acc, next) => acc / next, mappedArgs[0], mappedArgs.slice(1))
  }
}

const cons: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  argumentPassingStyle: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: ExpressibleValue[]) => ({
    type: 'EVPair',
    head: args[0],
    tail: args[1]
  })
}

const doOnPair = <T>(opName: string, pair: ExpressibleValue, op: (pair: EVPair) => T) => {
  if (pair.type !== 'EVPair') {
    throw new Error(opName + ' expects a pair as the only argument, but encoundered ' + pair.type)
  }
  return op(pair)
}

const car: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  argumentPassingStyle: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: ExpressibleValue[]) =>
    doOnPair('car', args[0], (pair: EVPair): ExpressibleValue => pair.head)
}

const cdr: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  argumentPassingStyle: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: ExpressibleValue[]) =>
    doOnPair('cdr', args[0], (pair: EVPair): ExpressibleValue => pair.tail)
}

export const defineBuiltins = (frame: Frame) => {
  defineBuiltin(frame, '+', add)
  defineBuiltin(frame, '-', subtract)
  defineBuiltin(frame, '*', multiply)
  defineBuiltin(frame, '/', divide)
  defineBuiltin(frame, 'cons', cons)
  defineBuiltin(frame, 'car', car)
  defineBuiltin(frame, 'cdr', cdr)

  const comparisonOperatorSpecs: [string, (lhs: number, rhs: number) => boolean][] = [
    ['=', (lhs, rhs) => lhs === rhs],
    ['<', (lhs, rhs) => lhs < rhs],
    ['<=', (lhs, rhs) => lhs <= rhs],
    ['>', (lhs, rhs) => lhs > rhs],
    ['>=', (lhs, rhs) => lhs >= rhs]
  ]

  comparisonOperatorSpecs.forEach(([opName, op]) =>
    defineBuiltin(frame, opName, {
      type: 'EVProcedure',
      variant: 'BuiltInProcedure',
      argumentPassingStyle: {
        style: 'fixed-args',
        numParams: 2
      },
      body: (args: ExpressibleValue[]) => {
        const mappedArgs = mapNumericalArguments(opName, args)
        return {
          type: 'EVBool',
          value: op(mappedArgs[0], mappedArgs[1])
        }
      }
    })
  )
}
