import { CallingNonFunctionValue, UnexpectedDottedList } from '../errors/errors'
import { ExpressibleValue, makeEmptyList } from '../interpreter/ExpressibleValue'
import { prepareContext, runUntilDone } from '../testHelpers'
import { Variant } from '../types'

describe.each<Variant>(['base', 'no-tco', 'macro'])('interpreter', variant => {
  function evaluateUntilDone(code: string): ExpressibleValue {
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
      expect(evaluateUntilDone('()')).toEqual(makeEmptyList())
    })
  })

  describe('dotted list', () => {
    test('should throw', () => {
      expect(() => evaluateUntilDone('(car . 1)')).toThrow(UnexpectedDottedList)
    })
  })
})
