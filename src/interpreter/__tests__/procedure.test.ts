import { InvalidNumberOfArguments } from '../../errors/errors'
import { evaluateUntilDone } from '../../testHelpers'
import { makeList, makeNumber } from '../runtime'

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

    expect(evaluateUntilDone(program)).toEqual(
      makeList(makeNumber(10), makeNumber(30), makeNumber(20), makeNumber(10))
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

      expect(evaluateUntilDone(program)).toEqual(makeNumber(6))
    })
  })
})
