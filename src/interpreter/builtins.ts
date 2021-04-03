import { Context, Frame } from '../types'
import {
  EVNumber,
  EVPair,
  EVProcedure,
  ExpressibleValue,
  makeNumber,
  makePair
} from './ExpressibleValue'

const defineBuiltin = (frame: Frame, name: string, value: ExpressibleValue) => {
  frame[name] = value
}

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

const add: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  argumentPassingStyle: {
    style: 'var-args'
  },
  body: (args: ExpressibleValue[]) =>
    reduceNumericalArgs((acc, next) => acc + next, 0, mustMapToNumbers('+', args))
}

const subtract: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  argumentPassingStyle: {
    style: 'rest-args',
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

const multiply: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  argumentPassingStyle: {
    style: 'var-args'
  },
  body: (args: ExpressibleValue[]) =>
    reduceNumericalArgs((acc, next) => acc * next, 1, mustMapToNumbers('*', args))
}

const divide: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  argumentPassingStyle: {
    style: 'rest-args',
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

const cons: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  argumentPassingStyle: {
    style: 'fixed-args',
    numParams: 2
  },
  body: (args: ExpressibleValue[]) => makePair(args[0], args[1])
}

const mustDoOnPair = <T>(opName: string, pair: ExpressibleValue, op: (pair: EVPair) => T): T => {
  if (pair.type !== 'EVPair') {
    throw new Error(opName + ' expects a pair as the only argument, but encountered ' + pair.type)
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
    mustDoOnPair('car', args[0], (pair: EVPair): ExpressibleValue => pair.head)
}

const cdr: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  argumentPassingStyle: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: ExpressibleValue[]) =>
    mustDoOnPair('cdr', args[0], (pair: EVPair): ExpressibleValue => pair.tail)
}

export const importNativeBuiltins = (context: Context) => {
  const frame = context.runtime.environments[0].head

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
        const mappedArgs = mustMapToNumbers(opName, args)
        return {
          type: 'EVBool',
          value: op(mappedArgs[0], mappedArgs[1])
        }
      }
    })
  )
}
