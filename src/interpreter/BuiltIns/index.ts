import { Context, Frame } from '../../types'
import { ExpressibleValue } from '../ExpressibleValue'
import {
  add,
  divide,
  greaterThan,
  greaterThanOrEqual,
  lessThan,
  lessThanOrEqual,
  multiply,
  numberEqual,
  quotient,
  remainder,
  subtract
} from './arithmetic'
import { car, cdr, cons } from './pair'

export const defineBuiltin = (frame: Frame, name: string, value: ExpressibleValue) => {
  frame[name] = value
}

export const importNativeBuiltins = (context: Context) => {
  const frame = context.runtime.environments[0].head

  defineBuiltin(frame, '+', add)
  defineBuiltin(frame, '-', subtract)
  defineBuiltin(frame, '*', multiply)
  defineBuiltin(frame, '/', divide)
  defineBuiltin(frame, 'quotient', quotient)
  defineBuiltin(frame, 'remainder', remainder)
  defineBuiltin(frame, '=', numberEqual)
  defineBuiltin(frame, '<', lessThan)
  defineBuiltin(frame, '<=', lessThanOrEqual)
  defineBuiltin(frame, '>', greaterThan)
  defineBuiltin(frame, '>=', greaterThanOrEqual)
  defineBuiltin(frame, 'cons', cons)
  defineBuiltin(frame, 'car', car)
  defineBuiltin(frame, 'cdr', cdr)
}
