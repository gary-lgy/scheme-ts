import {
  BuiltinProcedureError,
  DefineSyntaxError,
  InvalidNumberOfArguments,
  LetSyntaxError,
  NotEnoughArguments
} from '../../errors/errors'
import { evaluateUntilDone } from '../../testHelpers'
import { makeBool, makeList, makeNumber, makePair } from '../ExpressibleValue'

describe('arithmetic procedures', () => {
  describe('+', () => {
    test('no arguments', () => {
      const actual = evaluateUntilDone('(+)')
      expect(actual).toEqual(makeNumber(0))
    })

    test('one argument', () => {
      const actual = evaluateUntilDone('(+ 5)')
      expect(actual).toEqual(makeNumber(5))
    })

    test('two arguments', () => {
      const actual = evaluateUntilDone('(+ 5 1)')
      expect(actual).toEqual(makeNumber(6))
    })

    test('more than two arguments', () => {
      const actual = evaluateUntilDone('(+ 1 2 3 4 5)')
      expect(actual).toEqual(makeNumber(15))
    })
  })

  describe('*', () => {
    test('no arguments', () => {
      const actual = evaluateUntilDone('(*)')
      expect(actual).toEqual(makeNumber(1))
    })

    test('one argument', () => {
      const actual = evaluateUntilDone('(* 5)')
      expect(actual).toEqual(makeNumber(5))
    })

    test('two arguments', () => {
      const actual = evaluateUntilDone('(* 5 2)')
      expect(actual).toEqual(makeNumber(10))
    })
  })

  describe('-', () => {
    test('no arguments', () => {
      const actual = () => evaluateUntilDone('(-)')
      expect(actual).toThrow(NotEnoughArguments)
    })

    test('one argument', () => {
      const actual = evaluateUntilDone('(- 5)')
      expect(actual).toEqual(makeNumber(-5))
    })

    test('two arguments', () => {
      const actual = evaluateUntilDone('(- 5 1)')
      expect(actual).toEqual(makeNumber(4))
    })
  })

  describe('/', () => {
    test('no arguments', () => {
      const actual = () => evaluateUntilDone('(/)')
      expect(actual).toThrow(NotEnoughArguments)
    })

    test('one argument', () => {
      const actual = evaluateUntilDone('(/ 5)')
      expect(actual).toEqual(makeNumber(0.2))
    })

    test('two arguments', () => {
      const actual = evaluateUntilDone('(/ 5 2)')
      expect(actual).toEqual(makeNumber(2.5))
    })

    test('division by zero', () => {
      const getBuiltinProcedureErrorString = (operation: () => void) => {
        try {
          operation()
        } catch (error) {
          expect(error).toBeInstanceOf(BuiltinProcedureError)
          return error.explain()
        }
      }
      expect(getBuiltinProcedureErrorString(() => evaluateUntilDone('(/ 0)'))).toEqual(
        expect.stringMatching(/division by zero/)
      )
      expect(getBuiltinProcedureErrorString(() => evaluateUntilDone('(/ 10 2 0)'))).toEqual(
        expect.stringMatching(/division by zero/)
      )
    })
  })
})

describe('pair procedures', () => {
  describe('cons', () => {
    test('must take two arguments', () => {
      expect(() => evaluateUntilDone('(cons)')).toThrow(InvalidNumberOfArguments)
      expect(() => evaluateUntilDone('(cons 1)')).toThrow(InvalidNumberOfArguments)
      expect(() => evaluateUntilDone('(cons 1 2 3)')).toThrow(InvalidNumberOfArguments)
    })

    test('returns a pair', () => {
      expect(evaluateUntilDone('(cons 1 2)')).toEqual(makePair(makeNumber(1), makeNumber(2)))
      expect(evaluateUntilDone('(cons (cons 1 2) (cons 3 4))')).toEqual(
        makePair(makePair(makeNumber(1), makeNumber(2)), makePair(makeNumber(3), makeNumber(4)))
      )
    })
  })

  describe('car', () => {
    test('must take one argument', () => {
      expect(() => evaluateUntilDone('(car)')).toThrow(InvalidNumberOfArguments)
      expect(() => evaluateUntilDone('(car (cons 1 2) (cons 3 4))')).toThrow(
        InvalidNumberOfArguments
      )
    })

    test('on non-pair argument', () => {
      expect(() => evaluateUntilDone('(car 1)')).toThrow(BuiltinProcedureError)
      expect(() => evaluateUntilDone('(car "string")')).toThrow(BuiltinProcedureError)
      expect(() => evaluateUntilDone("(car '())")).toThrow(BuiltinProcedureError)
    })

    test('on improper list', () => {
      expect(evaluateUntilDone('(car (cons 1 2))')).toEqual(makeNumber(1))
    })

    test('on proper list', () => {
      expect(evaluateUntilDone("(car '(1 2 3))")).toEqual(makeNumber(1))
    })
  })

  describe('cdr', () => {
    test('must take one argument', () => {
      expect(() => evaluateUntilDone('(cdr)')).toThrow(InvalidNumberOfArguments)
      expect(() => evaluateUntilDone('(cdr (cons 1 2) (cons 3 4))')).toThrow(
        InvalidNumberOfArguments
      )
    })

    test('on non-pair argument', () => {
      expect(() => evaluateUntilDone('(cdr 1)')).toThrow(BuiltinProcedureError)
      expect(() => evaluateUntilDone('(cdr "string")')).toThrow(BuiltinProcedureError)
      expect(() => evaluateUntilDone("(cdr '())")).toThrow(BuiltinProcedureError)
    })

    test('on improper list', () => {
      expect(evaluateUntilDone('(cdr (cons 1 2))')).toEqual(makeNumber(2))
    })

    test('on proper list', () => {
      expect(evaluateUntilDone("(cdr '(1 2 3))")).toEqual(makeList(makeNumber(2), makeNumber(3)))
    })
  })
})

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
