import { DisallowedIdentifier, UndefinedVariable } from '../errors/errors'
import { makeNumber } from '../interpreter/SExpression'
import { Value } from '../interpreter/Value'
import { prepareContext, runUntilDone } from '../testHelpers'
import { Variant } from '../types'

describe.each<Variant>(['base', 'no-tco', 'macro'])('names', variant => {
  function evaluateUntilDone(code: string): Value {
    const context = prepareContext(variant)
    return runUntilDone(code, context).value
  }

  describe('unbound name', () => {
    test('should throw', () => {
      expect(() => evaluateUntilDone('x')).toThrow(UndefinedVariable)
      expect(() => evaluateUntilDone('(x)')).toThrow(UndefinedVariable)
    })
  })

  describe('shadowing bindings', () => {
    test('should refer to the innermost binding', () => {
      expect(
        evaluateUntilDone(`
      (define x 10)
      ((lambda (x) x) 20)
    `)
      ).toHaveMatchingValue(makeNumber(20))
    })
  })

  describe('user identifier with synthetic prefix', () => {
    describe('using define', () => {
      test('should reject definition', () => {
        expect(() => evaluateUntilDone('(define $:my-name 29)')).toThrow(DisallowedIdentifier)
      })
    })
    describe('as parameter', () => {
      test('should reject binding', () => {
        expect(() => evaluateUntilDone('((lambda ($:29) (+ $:29 + 1)) 20)')).toThrow(
          DisallowedIdentifier
        )
      })
    })
  })
})
