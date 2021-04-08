import { Context, Frame } from '../../types'
import { ExpressibleValue } from '../ExpressibleValue'
import {
  add,
  divide,
  greaterThan,
  greaterThanOrEqual,
  lessThan,
  lessThanOrEqual,
  modulo,
  multiply,
  numberEqual,
  quotient,
  remainder,
  subtract
} from './Arithmetic'
import { apply, error } from './Control'
import { eq, equal, eqv } from './Equal'
import { genSym, macroexpand } from './Macro'
import { car, cdr, cons, list, setCar, setCdr } from './Pair'
import { isBool, isNull, isNumber, isPair, isProcedure, isString, isSymbol } from './TypePredicates'

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
  defineBuiltin(frame, 'modulo', modulo)

  defineBuiltin(frame, '=', numberEqual)
  defineBuiltin(frame, '<', lessThan)
  defineBuiltin(frame, '<=', lessThanOrEqual)
  defineBuiltin(frame, '>', greaterThan)
  defineBuiltin(frame, '>=', greaterThanOrEqual)

  defineBuiltin(frame, 'cons', cons)
  defineBuiltin(frame, 'car', car)
  defineBuiltin(frame, 'cdr', cdr)
  defineBuiltin(frame, 'set-car!', setCar)
  defineBuiltin(frame, 'set-cdr!', setCdr)
  defineBuiltin(frame, 'list', list)

  defineBuiltin(frame, 'number?', isNumber)
  defineBuiltin(frame, 'boolean?', isBool)
  defineBuiltin(frame, 'string?', isString)
  defineBuiltin(frame, 'symbol?', isSymbol)
  defineBuiltin(frame, 'procedure?', isProcedure)
  defineBuiltin(frame, 'pair?', isPair)
  defineBuiltin(frame, 'null?', isNull)

  defineBuiltin(frame, 'eq?', eq)
  defineBuiltin(frame, 'eqv?', eqv)
  defineBuiltin(frame, 'equal?', equal)

  defineBuiltin(frame, 'apply', apply)
  defineBuiltin(frame, 'error', error)

  if (context.variant === 'macro') {
    defineBuiltin(frame, 'macroexpand', macroexpand)
    defineBuiltin(frame, 'gensym', genSym)
  }
}
