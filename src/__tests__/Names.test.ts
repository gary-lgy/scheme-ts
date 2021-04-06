import { DisallowedIdentifier, UndefinedVariable } from '../errors/errors'
import { makeNumber } from '../interpreter/ExpressibleValue'
import { evaluateUntilDone } from '../testHelpers'

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
    ).toEqual(makeNumber(20))
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
