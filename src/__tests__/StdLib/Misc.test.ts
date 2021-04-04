import { makeBool } from '../../interpreter/ExpressibleValue'
import { evaluateUntilDone } from '../../testHelpers'

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
