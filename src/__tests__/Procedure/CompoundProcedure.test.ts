import { Variant } from '../..'
import { InvalidNumberOfArguments, NotEnoughArguments } from '../../errors/errors'
import { makeNumber } from '../../interpreter/sExpression'
import { makeList, Value } from '../../interpreter/value'
import { prepareContext, runUntilDone } from '../../testHelpers'

describe.each<Variant>(['base', 'no-tco', 'macro'])('miscellaneous library features', variant => {
  function evaluateUntilDone(code: string): Value {
    const context = prepareContext(variant)
    return runUntilDone(code, context).value
  }

  describe('number of arguments', () => {
    describe('fixed number of arguments', () => {
      test('nullary lambda', () => {
        const program = '((lambda () 42))'
        expect(() => {
          evaluateUntilDone(program)
        }).not.toThrow(InvalidNumberOfArguments)
      })

      test('unary lambda', () => {
        const program = '((lambda (x) (+ x 1)) 1)'
        expect(() => {
          evaluateUntilDone(program)
        }).not.toThrow(InvalidNumberOfArguments)
      })

      test('binary lambda', () => {
        const program = '((lambda (x y) (+ x y)) 1 2)'
        expect(() => {
          evaluateUntilDone(program)
        }).not.toThrow(InvalidNumberOfArguments)
      })

      test('ternary lambda', () => {
        const program = '((lambda (x y z) (+ x y z)) 1 2 3)'
        expect(() => {
          evaluateUntilDone(program)
        }).not.toThrow(InvalidNumberOfArguments)
      })
    })

    describe('var args', () => {
      test('with no compulsory arguments', () => {
        expect(evaluateUntilDone('((lambda x x) 1 2 3)')).toHaveMatchingValue(
          makeList([makeNumber(1), makeNumber(2), makeNumber(3)])
        )
      })

      test('with one compulsory argument', () => {
        expect(evaluateUntilDone('((lambda (x . rest) (cons x rest)) 1 2 3)')).toHaveMatchingValue(
          makeList([makeNumber(1), makeNumber(2), makeNumber(3)])
        )
      })

      describe('with two compulsory arguments', () => {
        test('with enough arguments', () => {
          expect(
            evaluateUntilDone('((lambda (x y . rest) (cons y (cons x rest))) 1 2 3 4)')
          ).toHaveMatchingValue(
            makeList([makeNumber(2), makeNumber(1), makeNumber(3), makeNumber(4)])
          )
        })

        test('without enough arguments', () => {
          expect(() => evaluateUntilDone('((lambda (x y . rest) (cons y x rest)) 1)')).toThrow(
            NotEnoughArguments
          )
        })
      })
    })
  })

  describe('name binding', () => {
    test('lexical scope', () => {
      const program = `
  (define result '())
  (define x 10)
  (define my-function
      (lambda (y)
          (set! result (cons y result))
          (define x 20)
          (set! result (cons x result))
          (set! x 30)
          (set! result (cons x result))))

  (my-function x)
  (set! result (cons x result))
  result
  `

      expect(evaluateUntilDone(program)).toHaveMatchingValue(
        makeList([makeNumber(10), makeNumber(30), makeNumber(20), makeNumber(10)])
      )
    })

    describe('return result', () => {
      test('returns result of evaluating the last expression', () => {
        const program = `
      ((lambda ()
      (+ 1 2)
      (+ 1 3)
      35
      (* 2 3)))`

        expect(evaluateUntilDone(program)).toHaveMatchingValue(makeNumber(6))
      })
    })
  })
})
