import {
  BuiltinProcedureError,
  InvalidNumberOfArguments,
  NotEnoughArguments
} from '../../errors/errors'
import { evaluateUntilDone } from '../../testHelpers'
import { stringify } from '../../utils/stringify'
import {
  makeBool,
  makeEmptyList,
  makeList,
  makeNumber,
  makePair,
  makeString
} from '../ExpressibleValue'

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

  describe('set-car!', () => {
    test('basic', () => {
      expect(
        evaluateUntilDone(`
      (define my-pair (cons 1 2))
      (set-car! my-pair 3)
      my-pair
      `)
      ).toEqual(makePair(makeNumber(3), makeNumber(2)))
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
      ).toEqual(makePair(makeNumber(1), makeNumber(3)))
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
      expect(evaluateUntilDone('(list)')).toEqual(makeEmptyList())
    })

    test('with arguments', () => {
      expect(evaluateUntilDone('(list 1 2 (list 3 4) "str")')).toEqual(
        makeList(
          makeNumber(1),
          makeNumber(2),
          makeList(makeNumber(3), makeNumber(4)),
          makeString('str')
        )
      )
    })
  })
})

describe('equivalence predicates', () => {
  describe('eqv?', () => {
    test('bool', () => {
      expect(evaluateUntilDone('(eqv? #t #t)')).toEqual(makeBool(true))
      expect(evaluateUntilDone('(eqv? #f #f)')).toEqual(makeBool(true))
      expect(evaluateUntilDone('(eqv? #t #f)')).toEqual(makeBool(false))
      expect(evaluateUntilDone('(eqv? #f #t)')).toEqual(makeBool(false))
    })

    test('symbols', () => {
      expect(evaluateUntilDone("(eqv? 'a 'a)")).toEqual(makeBool(true))
      expect(evaluateUntilDone("(eqv? 'a 'b)")).toEqual(makeBool(false))
    })

    test('numbers', () => {
      expect(evaluateUntilDone('(eqv? 1 1)')).toEqual(makeBool(true))
      expect(evaluateUntilDone('(eqv? 1 2)')).toEqual(makeBool(false))
    })

    test('empty list', () => {
      expect(evaluateUntilDone("(eqv? '() '())")).toEqual(makeBool(true))
      expect(evaluateUntilDone("(eqv? '() '(1))")).toEqual(makeBool(false))
    })

    test('pair', () => {
      expect(evaluateUntilDone('(eqv? (cons 1 2) (cons 1 2))')).toEqual(makeBool(false))
      expect(evaluateUntilDone('(let ((my-pair (cons 1 2))) (eqv? my-pair my-pair))')).toEqual(
        makeBool(true)
      )
    })

    test('procedure', () => {
      expect(evaluateUntilDone('(eqv? (lambda () 1) (lambda () 1))')).toEqual(makeBool(false))
      expect(
        evaluateUntilDone('(let ((my-procedure (lambda () 1))) (eqv? my-procedure my-procedure))')
      ).toEqual(makeBool(true))
    })

    test('different types', () => {
      expect(evaluateUntilDone("(eqv? #f 'nil)")).toEqual(makeBool(false))
      expect(evaluateUntilDone('(eqv? (lambda () 1) 1)')).toEqual(makeBool(false))
      expect(evaluateUntilDone(`(eqv? "a" 'a)`)).toEqual(makeBool(false))
    })
  })

  describe('eq?', () => {
    test('bool', () => {
      expect(evaluateUntilDone('(eq? #t #t)')).toEqual(makeBool(true))
      expect(evaluateUntilDone('(eq? #f #f)')).toEqual(makeBool(true))
      expect(evaluateUntilDone('(eq? #t #f)')).toEqual(makeBool(false))
      expect(evaluateUntilDone('(eq? #f #t)')).toEqual(makeBool(false))
    })

    test('symbols', () => {
      expect(evaluateUntilDone("(eq? 'a 'a)")).toEqual(makeBool(true))
      expect(evaluateUntilDone("(eq? 'a 'b)")).toEqual(makeBool(false))
    })

    test('numbers', () => {
      expect(evaluateUntilDone('(eq? 1 1)')).toEqual(makeBool(true))
      expect(evaluateUntilDone('(eq? 1 2)')).toEqual(makeBool(false))
    })

    test('empty list', () => {
      expect(evaluateUntilDone("(eq? '() '())")).toEqual(makeBool(true))
      expect(evaluateUntilDone("(eq? '() '(1))")).toEqual(makeBool(false))
    })

    test('pair', () => {
      expect(evaluateUntilDone('(eq? (cons 1 2) (cons 1 2))')).toEqual(makeBool(false))
      expect(evaluateUntilDone('(let ((my-pair (cons 1 2))) (eq? my-pair my-pair))')).toEqual(
        makeBool(true)
      )
    })

    test('procedure', () => {
      expect(evaluateUntilDone('(eq? (lambda () 1) (lambda () 1))')).toEqual(makeBool(false))
      expect(
        evaluateUntilDone('(let ((my-procedure (lambda () 1))) (eq? my-procedure my-procedure))')
      ).toEqual(makeBool(true))
    })

    test('different types', () => {
      expect(evaluateUntilDone("(eq? #f 'nil)")).toEqual(makeBool(false))
      expect(evaluateUntilDone('(eq? (lambda () 1) 1)')).toEqual(makeBool(false))
      expect(evaluateUntilDone(`(eq? "a" 'a)`)).toEqual(makeBool(false))
    })
  })

  describe('equal?', () => {
    test('bool', () => {
      expect(evaluateUntilDone('(equal? #t #t)')).toEqual(makeBool(true))
      expect(evaluateUntilDone('(equal? #f #f)')).toEqual(makeBool(true))
      expect(evaluateUntilDone('(equal? #t #f)')).toEqual(makeBool(false))
      expect(evaluateUntilDone('(equal? #f #t)')).toEqual(makeBool(false))
    })

    test('symbols', () => {
      expect(evaluateUntilDone("(equal? 'a 'a)")).toEqual(makeBool(true))
      expect(evaluateUntilDone("(equal? 'a 'b)")).toEqual(makeBool(false))
    })

    test('numbers', () => {
      expect(evaluateUntilDone('(equal? 1 1)')).toEqual(makeBool(true))
      expect(evaluateUntilDone('(equal? 1 2)')).toEqual(makeBool(false))
    })

    test('empty list', () => {
      expect(evaluateUntilDone("(equal? '() '())")).toEqual(makeBool(true))
      expect(evaluateUntilDone("(equal? '() '(1))")).toEqual(makeBool(false))
    })

    test('pair', () => {
      expect(evaluateUntilDone('(equal? (cons 1 2) (cons 1 2))')).toEqual(makeBool(true))
      expect(evaluateUntilDone(`(equal? '(a (b) c) '(a (b) c))`)).toEqual(makeBool(true))
      expect(evaluateUntilDone('(let ((my-pair (cons 1 2))) (equal? my-pair my-pair))')).toEqual(
        makeBool(true)
      )
    })

    test('procedure', () => {
      expect(evaluateUntilDone('(equal? (lambda () 1) (lambda () 1))')).toEqual(makeBool(false))
      expect(
        evaluateUntilDone('(let ((my-procedure (lambda () 1))) (equal? my-procedure my-procedure))')
      ).toEqual(makeBool(true))
    })

    test('different types', () => {
      expect(evaluateUntilDone("(equal? #f 'nil)")).toEqual(makeBool(false))
      expect(evaluateUntilDone('(equal? (lambda () 1) 1)')).toEqual(makeBool(false))
      expect(evaluateUntilDone(`(equal? "a" 'a)`)).toEqual(makeBool(false))
    })
  })
})
