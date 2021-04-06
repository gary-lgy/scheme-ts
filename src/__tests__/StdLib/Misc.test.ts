import { ExpressibleValue, makeBool, makeNumber } from '../../interpreter/ExpressibleValue'
import { prepareContext, runUntilDone } from '../../testHelpers'
import { Variant } from '../../types'

describe.each<Variant>(['base', 'no-tco', 'macro'])('miscellaneous library features', variant => {
  function evaluateUntilDone(code: string): ExpressibleValue {
    const context = prepareContext(variant)
    return runUntilDone(code, context).value
  }

  describe('logical operators', () => {
    describe('not', () => {
      describe('with truthy argument', () => {
        test('returns false', () => {
          expect(evaluateUntilDone('(not 2)')).toEqual(makeBool(false))
          expect(evaluateUntilDone('(not #t)')).toEqual(makeBool(false))
        })
      })

      describe('with false argument', () => {
        test('returns true', () => {
          expect(evaluateUntilDone('(not #f)')).toEqual(makeBool(true))
        })
      })
    })
  })

  describe('numeric', () => {
    describe('zero?', () => {
      test('returns whether argument is zero', () => {
        expect(evaluateUntilDone('(zero? 0)')).toEqual(makeBool(true))
        expect(evaluateUntilDone("(zero? '0)")).toEqual(makeBool(true))
        expect(evaluateUntilDone('(zero? 1)')).toEqual(makeBool(false))
        expect(evaluateUntilDone('(zero? "0")')).toEqual(makeBool(false))
      })
    })
  })

  describe('apply', () => {
    describe('called with no arguments', () => {
      test('should throw', () => {
        expect(() => evaluateUntilDone('(apply)')).toThrow()
      })
    })

    describe('calling nullary procedure', () => {
      test('should work correctly', () => {
        expect(evaluateUntilDone('(apply (lambda () (+ 1 1)))')).toEqual(makeNumber(2))
      })
    })

    describe('called with all arguments in tail list', () => {
      test('should work correctly', () => {
        expect(evaluateUntilDone("(apply + '(1 2 3))")).toEqual(makeNumber(6))
      })
    })

    describe('called with no arguments in tail list', () => {
      test('should work correctly', () => {
        expect(evaluateUntilDone("(apply + 1 2 3 '())")).toEqual(makeNumber(6))
      })
    })

    describe('called with arguments in both tail list and before it', () => {
      test('should work correctly', () => {
        expect(evaluateUntilDone("(apply + 1 '(2 3))")).toEqual(makeNumber(6))
      })
    })
  })
})
