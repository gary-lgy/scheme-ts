import { CallingNonFunctionValue, UnexpectedDottedList } from '../errors/errors'
import { makeEmptyList } from '../interpreter/ExpressibleValue'
import { evaluateUntilDone } from '../testHelpers'

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
