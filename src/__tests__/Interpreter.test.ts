import { CallingNonFunctionValue, UnexpectedDottedList } from '../errors/errors'
import { makeEmptyList } from '../interpreter/SExpression'
import { Value } from '../interpreter/Value'
import { prepareContext, runUntilDone } from '../testHelpers'
import { Variant } from '../types'

describe.each<Variant>(['base', 'no-tco', 'macro'])('interpreter', variant => {
  function evaluateUntilDone(code: string): Value {
    const context = prepareContext(variant)
    return runUntilDone(code, context).value
  }

  describe('calling non-function value', () => {
    test('should throw', () => {
      expect(() => evaluateUntilDone('(1 2)')).toThrow(CallingNonFunctionValue)
      expect(() => evaluateUntilDone('(let ((x 1)) (x 2))')).toThrow(CallingNonFunctionValue)
    })
  })

  describe('empty list', () => {
    test('should return empty list', () => {
      expect(evaluateUntilDone('()')).toHaveMatchingValue(makeEmptyList())
    })
  })

  describe('dotted list', () => {
    test('should throw', () => {
      expect(() => evaluateUntilDone('(car . 1)')).toThrow(UnexpectedDottedList)
    })
  })
})
