import { Variant } from '../..'
import {
  BuiltinProcedureError,
  InvalidNumberOfArguments,
  NotEnoughArguments
} from '../../errors/errors'
import { makeBool, makeEmptyList, makeNumber, makeString } from '../../interpreter/sExpression'
import { makeList, makePair, Value } from '../../interpreter/value'
import { prepareContext, runUntilDone } from '../../testHelpers'
import { stringify } from '../../utils/stringify'

describe.each<Variant>(['base', 'no-tco', 'macro'])('miscellaneous library features', variant => {
  function evaluateUntilDone(code: string): Value {
    const context = prepareContext(variant)
    return runUntilDone(code, context).value
  }

  describe('arithmetic procedures', () => {
    describe('+', () => {
      test('no arguments', () => {
        const actual = evaluateUntilDone('(+)')
        expect(actual).toHaveMatchingValue(makeNumber(0))
      })

      test('one argument', () => {
        const actual = evaluateUntilDone('(+ 5)')
        expect(actual).toHaveMatchingValue(makeNumber(5))
      })

      test('two arguments', () => {
        const actual = evaluateUntilDone('(+ 5 1)')
        expect(actual).toHaveMatchingValue(makeNumber(6))
      })

      test('more than two arguments', () => {
        const actual = evaluateUntilDone('(+ 1 2 3 4 5)')
        expect(actual).toHaveMatchingValue(makeNumber(15))
      })
    })

    describe('*', () => {
      test('no arguments', () => {
        const actual = evaluateUntilDone('(*)')
        expect(actual).toHaveMatchingValue(makeNumber(1))
      })

      test('one argument', () => {
        const actual = evaluateUntilDone('(* 5)')
        expect(actual).toHaveMatchingValue(makeNumber(5))
      })

      test('two arguments', () => {
        const actual = evaluateUntilDone('(* 5 2)')
        expect(actual).toHaveMatchingValue(makeNumber(10))
      })
    })

    describe('-', () => {
      test('no arguments', () => {
        const actual = () => evaluateUntilDone('(-)')
        expect(actual).toThrow(NotEnoughArguments)
      })

      test('one argument', () => {
        const actual = evaluateUntilDone('(- 5)')
        expect(actual).toHaveMatchingValue(makeNumber(-5))
      })

      test('two arguments', () => {
        const actual = evaluateUntilDone('(- 5 1)')
        expect(actual).toHaveMatchingValue(makeNumber(4))
      })
    })

    describe('/', () => {
      test('no arguments', () => {
        const actual = () => evaluateUntilDone('(/)')
        expect(actual).toThrow(NotEnoughArguments)
      })

      test('one argument', () => {
        const actual = evaluateUntilDone('(/ 5)')
        expect(actual).toHaveMatchingValue(makeNumber(0.2))
      })

      test('two arguments', () => {
        const actual = evaluateUntilDone('(/ 5 2)')
        expect(actual).toHaveMatchingValue(makeNumber(2.5))
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

    describe('remainder', () => {
      test('both positive', () => {
        expect(evaluateUntilDone('(remainder 13 4)')).toHaveMatchingValue(makeNumber(1))
      })

      test('negative + positive', () => {
        expect(evaluateUntilDone('(remainder -13 4)')).toHaveMatchingValue(makeNumber(-1))
      })

      test('positive + negative', () => {
        expect(evaluateUntilDone('(remainder 13 -4)')).toHaveMatchingValue(makeNumber(1))
      })

      test('both negative', () => {
        expect(evaluateUntilDone('(remainder -13 -4)')).toHaveMatchingValue(makeNumber(-1))
      })
    })

    describe('modulo', () => {
      test('both positive', () => {
        expect(evaluateUntilDone('(modulo 13 4)')).toHaveMatchingValue(makeNumber(1))
      })

      test('negative + positive', () => {
        expect(evaluateUntilDone('(modulo -13 4)')).toHaveMatchingValue(makeNumber(3))
      })

      test('positive + negative', () => {
        expect(evaluateUntilDone('(modulo 13 -4)')).toHaveMatchingValue(makeNumber(-3))
      })

      test('both negative', () => {
        expect(evaluateUntilDone('(modulo -13 -4)')).toHaveMatchingValue(makeNumber(-1))
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
        expect(evaluateUntilDone('(cons 1 2)')).toHaveMatchingValue(
          makePair(makeNumber(1), makeNumber(2))
        )
        expect(evaluateUntilDone('(cons (cons 1 2) (cons 3 4))')).toHaveMatchingValue(
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
        expect(evaluateUntilDone('(car (cons 1 2))')).toHaveMatchingValue(makeNumber(1))
      })

      test('on proper list', () => {
        expect(evaluateUntilDone("(car '(1 2 3))")).toHaveMatchingValue(makeNumber(1))
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
        expect(evaluateUntilDone('(cdr (cons 1 2))')).toHaveMatchingValue(makeNumber(2))
      })

      test('on proper list', () => {
        expect(evaluateUntilDone("(cdr '(1 2 3))")).toHaveMatchingValue(
          makeList([makeNumber(2), makeNumber(3)])
        )
      })
    })

    describe('set-car!', () => {
      test('basic', () => {
        expect(
          evaluateUntilDone(`
      (define my-pair (cons 1 2))
      (set-car! my-pair 3)
      my-pair
      `)
        ).toHaveMatchingValue(makePair(makeNumber(3), makeNumber(2)))
      })
    })

    describe('set-cdr!', () => {
      test('basic', () => {
        expect(
          evaluateUntilDone(`
      (define my-pair (cons 1 2))
      (set-cdr! my-pair 3)
      my-pair
      `)
        ).toHaveMatchingValue(makePair(makeNumber(1), makeNumber(3)))
      })

      test('circular', () => {
        expect(
          stringify(
            evaluateUntilDone(`
      (define my-pair (cons 1 2))
      (set-cdr! my-pair my-pair)
      my-pair
      `)
          )
        ).toEqual('(1 . ...<circular>)')
      })
    })

    describe('list', () => {
      test('no arguments', () => {
        expect(evaluateUntilDone('(list)')).toHaveMatchingValue(makeEmptyList())
      })

      test('with arguments', () => {
        expect(evaluateUntilDone('(list 1 2 (list 3 4) "str")')).toHaveMatchingValue(
          makeList([
            makeNumber(1),
            makeNumber(2),
            makeList([makeNumber(3), makeNumber(4)]),
            makeString('str')
          ])
        )
      })
    })
  })

  describe('equivalence predicates', () => {
    describe('eqv?', () => {
      test('bool', () => {
        expect(evaluateUntilDone('(eqv? #t #t)')).toHaveMatchingValue(makeBool(true))
        expect(evaluateUntilDone('(eqv? #f #f)')).toHaveMatchingValue(makeBool(true))
        expect(evaluateUntilDone('(eqv? #t #f)')).toHaveMatchingValue(makeBool(false))
        expect(evaluateUntilDone('(eqv? #f #t)')).toHaveMatchingValue(makeBool(false))
      })

      test('symbols', () => {
        expect(evaluateUntilDone("(eqv? 'a 'a)")).toHaveMatchingValue(makeBool(true))
        expect(evaluateUntilDone("(eqv? 'a 'b)")).toHaveMatchingValue(makeBool(false))
      })

      test('numbers', () => {
        expect(evaluateUntilDone('(eqv? 1 1)')).toHaveMatchingValue(makeBool(true))
        expect(evaluateUntilDone('(eqv? 1 2)')).toHaveMatchingValue(makeBool(false))
      })

      test('empty list', () => {
        expect(evaluateUntilDone("(eqv? '() '())")).toHaveMatchingValue(makeBool(true))
        expect(evaluateUntilDone("(eqv? '() '(1))")).toHaveMatchingValue(makeBool(false))
      })

      test('pair', () => {
        expect(evaluateUntilDone('(eqv? (cons 1 2) (cons 1 2))')).toHaveMatchingValue(
          makeBool(false)
        )
        expect(
          evaluateUntilDone('(let ((my-pair (cons 1 2))) (eqv? my-pair my-pair))')
        ).toHaveMatchingValue(makeBool(true))
      })

      test('procedure', () => {
        expect(evaluateUntilDone('(eqv? (lambda () 1) (lambda () 1))')).toHaveMatchingValue(
          makeBool(false)
        )
        expect(
          evaluateUntilDone('(let ((my-procedure (lambda () 1))) (eqv? my-procedure my-procedure))')
        ).toHaveMatchingValue(makeBool(true))
      })

      test('different types', () => {
        expect(evaluateUntilDone("(eqv? #f 'nil)")).toHaveMatchingValue(makeBool(false))
        expect(evaluateUntilDone('(eqv? (lambda () 1) 1)')).toHaveMatchingValue(makeBool(false))
        expect(evaluateUntilDone(`(eqv? "a" 'a)`)).toHaveMatchingValue(makeBool(false))
      })
    })

    describe('eq?', () => {
      test('bool', () => {
        expect(evaluateUntilDone('(eq? #t #t)')).toHaveMatchingValue(makeBool(true))
        expect(evaluateUntilDone('(eq? #f #f)')).toHaveMatchingValue(makeBool(true))
        expect(evaluateUntilDone('(eq? #t #f)')).toHaveMatchingValue(makeBool(false))
        expect(evaluateUntilDone('(eq? #f #t)')).toHaveMatchingValue(makeBool(false))
      })

      test('symbols', () => {
        expect(evaluateUntilDone("(eq? 'a 'a)")).toHaveMatchingValue(makeBool(true))
        expect(evaluateUntilDone("(eq? 'a 'b)")).toHaveMatchingValue(makeBool(false))
      })

      test('numbers', () => {
        expect(evaluateUntilDone('(eq? 1 1)')).toHaveMatchingValue(makeBool(true))
        expect(evaluateUntilDone('(eq? 1 2)')).toHaveMatchingValue(makeBool(false))
      })

      test('empty list', () => {
        expect(evaluateUntilDone("(eq? '() '())")).toHaveMatchingValue(makeBool(true))
        expect(evaluateUntilDone("(eq? '() '(1))")).toHaveMatchingValue(makeBool(false))
      })

      test('pair', () => {
        expect(evaluateUntilDone('(eq? (cons 1 2) (cons 1 2))')).toHaveMatchingValue(
          makeBool(false)
        )
        expect(
          evaluateUntilDone('(let ((my-pair (cons 1 2))) (eq? my-pair my-pair))')
        ).toHaveMatchingValue(makeBool(true))
      })

      test('procedure', () => {
        expect(evaluateUntilDone('(eq? (lambda () 1) (lambda () 1))')).toHaveMatchingValue(
          makeBool(false)
        )
        expect(
          evaluateUntilDone('(let ((my-procedure (lambda () 1))) (eq? my-procedure my-procedure))')
        ).toHaveMatchingValue(makeBool(true))
      })

      test('different types', () => {
        expect(evaluateUntilDone("(eq? #f 'nil)")).toHaveMatchingValue(makeBool(false))
        expect(evaluateUntilDone('(eq? (lambda () 1) 1)')).toHaveMatchingValue(makeBool(false))
        expect(evaluateUntilDone(`(eq? "a" 'a)`)).toHaveMatchingValue(makeBool(false))
      })
    })

    describe('equal?', () => {
      test('bool', () => {
        expect(evaluateUntilDone('(equal? #t #t)')).toHaveMatchingValue(makeBool(true))
        expect(evaluateUntilDone('(equal? #f #f)')).toHaveMatchingValue(makeBool(true))
        expect(evaluateUntilDone('(equal? #t #f)')).toHaveMatchingValue(makeBool(false))
        expect(evaluateUntilDone('(equal? #f #t)')).toHaveMatchingValue(makeBool(false))
      })

      test('symbols', () => {
        expect(evaluateUntilDone("(equal? 'a 'a)")).toHaveMatchingValue(makeBool(true))
        expect(evaluateUntilDone("(equal? 'a 'b)")).toHaveMatchingValue(makeBool(false))
      })

      test('numbers', () => {
        expect(evaluateUntilDone('(equal? 1 1)')).toHaveMatchingValue(makeBool(true))
        expect(evaluateUntilDone('(equal? 1 2)')).toHaveMatchingValue(makeBool(false))
      })

      test('empty list', () => {
        expect(evaluateUntilDone("(equal? '() '())")).toHaveMatchingValue(makeBool(true))
        expect(evaluateUntilDone("(equal? '() '(1))")).toHaveMatchingValue(makeBool(false))
      })

      test('pair', () => {
        expect(evaluateUntilDone('(equal? (cons 1 2) (cons 1 2))')).toHaveMatchingValue(
          makeBool(true)
        )
        expect(evaluateUntilDone(`(equal? '(a (b) c) '(a (b) c))`)).toHaveMatchingValue(
          makeBool(true)
        )
        expect(
          evaluateUntilDone('(let ((my-pair (cons 1 2))) (equal? my-pair my-pair))')
        ).toHaveMatchingValue(makeBool(true))
      })

      test('procedure', () => {
        expect(evaluateUntilDone('(equal? (lambda () 1) (lambda () 1))')).toHaveMatchingValue(
          makeBool(false)
        )
        expect(
          evaluateUntilDone(
            '(let ((my-procedure (lambda () 1))) (equal? my-procedure my-procedure))'
          )
        ).toHaveMatchingValue(makeBool(true))
      })

      test('different types', () => {
        expect(evaluateUntilDone("(equal? #f 'nil)")).toHaveMatchingValue(makeBool(false))
        expect(evaluateUntilDone('(equal? (lambda () 1) 1)')).toHaveMatchingValue(makeBool(false))
        expect(evaluateUntilDone(`(equal? "a" 'a)`)).toHaveMatchingValue(makeBool(false))
      })
    })
  })

  describe('apply', () => {
    test('basic', () => {
      expect(evaluateUntilDone('(apply + (list 3 4))')).toHaveMatchingValue(makeNumber(7))
    })

    test('without spreading arguments', () => {
      expect(
        evaluateUntilDone(`
        (define compose
          (lambda (f g)
            (lambda args
              (f (apply g args)))))

        ((compose / *) 2 5)
      `)
      ).toHaveMatchingValue(makeNumber(0.1))
    })

    test('with spreading arguments', () => {
      expect(evaluateUntilDone("(apply (lambda x x) 1 2 '(3 4 5))")).toHaveMatchingValue(
        makeList([makeNumber(1), makeNumber(2), makeNumber(3), makeNumber(4), makeNumber(5)])
      )
    })
  })
})
