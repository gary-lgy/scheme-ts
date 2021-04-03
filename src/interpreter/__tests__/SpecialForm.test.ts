import { DefineSyntaxError, LetSyntaxError } from '../../errors/errors'
import { evaluateUntilDone } from '../../testHelpers'
import { makeBool, makeNumber } from '../ExpressibleValue'

describe('binding constructs', () => {
  describe('define', () => {
    test('invalid syntax', () => {
      const programs: string[] = [
        '(define)',
        '(define x)',
        '(define 1 x)',
        '(define x 1 2)',
        '(define (x))',
        '(define (x y))',
        '(define (fn (x)) (+ x 1))',
        '(define ((fn) x) (+ x 1))'
      ]
      programs.forEach(program => {
        expect(() => evaluateUntilDone(program)).toThrow(DefineSyntaxError)
      })
    })

    test('basic variant', () => {
      const actual = evaluateUntilDone(`
        (define x (+ 5 5))
        (+ x 10)
      `)
      expect(actual).toEqual(makeNumber(20))
    })

    test('procedure variant', () => {
      const actual = evaluateUntilDone(`
        (define (fn x y)
          (+ x y))
        (fn 1 2)
      `)
      expect(actual).toEqual(makeNumber(3))
    })
  })

  describe('let', () => {
    test('basic', () => {
      const actual = evaluateUntilDone(`
        (let ((x 2) (y 3))
          (* x y))
      `)
      expect(actual).toEqual(makeNumber(6))
    })

    test('with shadowing binding', () => {
      const actual = evaluateUntilDone(`
        (let ((x 2) (y 3))
          (let ((x 7)
                (z (+ x y)))
            (* z x)))
      `)
      expect(actual).toEqual(makeNumber(35))
    })

    test('no body', () => {
      expect(() => evaluateUntilDone(`(let ((x 10)))`)).toThrow(LetSyntaxError)
    })

    test('no bindings', () => {
      expect(evaluateUntilDone(`(let () (+ 1 2))`)).toEqual(makeNumber(3))
    })
  })

  describe('let*', () => {
    test('basic', () => {
      const actual = evaluateUntilDone(`
        (let ((x 2) (y 3))
          (let* ((x 7)
                (z (+ x y)))
            (* z x)))
      `)
      expect(actual).toEqual(makeNumber(70))
    })

    test('no body', () => {
      expect(() => evaluateUntilDone(`(let* ((x 10)))`)).toThrow(LetSyntaxError)
    })

    test('no bindings', () => {
      expect(evaluateUntilDone(`(let* () (+ 1 2))`)).toEqual(makeNumber(3))
    })
  })

  describe('letrec', () => {
    test('basic', () => {
      const actual = evaluateUntilDone(`
        (letrec ((even?
                  (lambda (n)
                    (if (= n 0)
                        #t
                        (odd? (- n 1)))))
                (odd?
                  (lambda (n)
                    (if (= n 0)
                        #f
                        (even? (- n 1))))))
          (even? 88))
      `)
      expect(actual).toEqual(makeBool(true))
    })

    test('no body', () => {
      expect(() => evaluateUntilDone(`(letrec ((x 10)))`)).toThrow(LetSyntaxError)
    })

    test('no bindings', () => {
      expect(evaluateUntilDone(`(letrec () (+ 1 2))`)).toEqual(makeNumber(3))
    })
  })
})
