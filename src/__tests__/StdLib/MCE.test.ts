import {
  makeBool,
  makeEmptyList,
  makeList,
  makeNumber,
  makePair,
  makeString,
  makeSymbol
} from '../../interpreter/ExpressibleValue'
import { evaluateInMce } from '../../testHelpers'

describe('self-evaluating', () => {
  test('number', () => {
    expect(evaluateInMce('1')).toEqual(makeNumber(1))
  })

  test('string', () => {
    expect(evaluateInMce('"MCE is awesome"')).toEqual(makeString('MCE is awesome'))
  })
})

describe('variable', () => {
  test('definition', () => {
    expect(evaluateInMce('(define x 10) x')).toEqual(makeNumber(10))
  })

  test('assignment', () => {
    expect(evaluateInMce('(define x 10) (set! x 20) x')).toEqual(makeNumber(20))
  })
})

test('quoted', () => {
  expect(evaluateInMce(`'(1 "my-string" (3 (4)))`)).toEqual(
    makeList(
      makeNumber(1),
      makeString('my-string'),
      makeList(makeNumber(3), makeList(makeNumber(4)))
    )
  )
})

describe('if', () => {
  describe('with truthy predicate', () => {
    test('returns consequent', () => {
      expect(evaluateInMce("(if (< 0 1) 'consequent 'alternative)")).toEqual(
        makeSymbol('consequent')
      )
    })

    test('does not evaluate the alternative', () => {
      expect(evaluateInMce(`(if (< 0 1) 'consequent (error "alternative evaluated"))`)).toEqual(
        makeSymbol('consequent')
      )
    })
  })

  describe('with false predicate', () => {
    test('returns alternative', () => {
      expect(evaluateInMce("(if (> 0 1) 'consequent 'alternative)")).toEqual(
        makeSymbol('alternative')
      )
    })

    test('does not evaluate the alternative', () => {
      expect(evaluateInMce(`(if (> 0 1) (error "consequent evaluated") 'alternative)`)).toEqual(
        makeSymbol('alternative')
      )
    })
  })

  describe('without alternative', () => {
    test('should not throw', () => {
      expect(() => evaluateInMce("(if (> 0 1) 'consequent)")).not.toThrow()
      expect(() => evaluateInMce("(if (< 0 1) 'consequent)")).not.toThrow()
    })
  })

  describe('procedure', () => {
    test('built-in procedures', () => {
      expect(evaluateInMce('(+ 1 2)')).toEqual(makeNumber(3))
      expect(evaluateInMce('(- 1 2)')).toEqual(makeNumber(-1))
      expect(evaluateInMce('(* 2 3)')).toEqual(makeNumber(6))
      expect(evaluateInMce('(/ 3 2)')).toEqual(makeNumber(1.5))

      expect(evaluateInMce('(= 1 1)')).toEqual(makeBool(true))
      expect(evaluateInMce('(= 1 2)')).toEqual(makeBool(false))
      expect(evaluateInMce('(< 1 2)')).toEqual(makeBool(true))
      expect(evaluateInMce('(< 2 1)')).toEqual(makeBool(false))
      expect(evaluateInMce('(> 1 2)')).toEqual(makeBool(false))
      expect(evaluateInMce('(> 2 1)')).toEqual(makeBool(true))

      expect(evaluateInMce('(cons 2 1)')).toEqual(makePair(makeNumber(2), makeNumber(1)))
      expect(evaluateInMce('(car (cons 2 1))')).toEqual(makeNumber(2))
      expect(evaluateInMce('(cdr (cons 2 1))')).toEqual(makeNumber(1))
      expect(
        evaluateInMce('(define my-pair (cons 2 1)) (set-car! my-pair 3) (car my-pair)')
      ).toEqual(makeNumber(3))
      expect(
        evaluateInMce('(define my-pair (cons 2 1)) (set-cdr! my-pair 3) (cdr my-pair)')
      ).toEqual(makeNumber(3))
      expect(evaluateInMce("(null? '())")).toEqual(makeBool(true))
      expect(evaluateInMce("(null? '(1))")).toEqual(makeBool(false))
      expect(evaluateInMce('(list)')).toEqual(makeEmptyList())
      expect(evaluateInMce('(list 1 2 3)')).toEqual(
        makeList(makeNumber(1), makeNumber(2), makeNumber(3))
      )
    })

    describe('compound procedures', () => {
      test('definition with define', () => {
        expect(() => evaluateInMce('(define (fn x) (+ x 1))')).not.toThrow()
      })

      test('definition with lambda', () => {
        expect(() => evaluateInMce('(lambda (x) (+ x 1))')).not.toThrow()
      })

      test('application', () => {
        expect(evaluateInMce('(define (fn x) (+ x 1)) (fn 2)')).toEqual(makeNumber(3))
        expect(evaluateInMce('((lambda (x) (+ x 1)) 2)')).toEqual(makeNumber(3))
      })

      test('lexical scope', () => {
        expect(
          evaluateInMce(`
        (define fn
          (lambda (x y)
            ((lambda (x z)
              (list x y z))
             3 4)))
        (fn 1 2)
        `)
        ).toEqual(makeList(makeNumber(3), makeNumber(2), makeNumber(4)))
      })
    })

    test('begin', () => {
      expect(
        evaluateInMce(`
      (begin
        (+ 1 2)
        'ok
        'not-ok
        (* 2 3)
        "lambda")
        `)
      ).toEqual(makeString('lambda'))
    })

    describe('cond', () => {
      test('empty basic clause', () => {
        const actual = evaluateInMce(`
      (cond ((+ 1 2)))
    `)
        expect(actual).toEqual(makeNumber(3))
      })

      test('basic clause with one body expression', () => {
        const actual = evaluateInMce(`
      (cond ((> 3 2) 'greater))
    `)
        expect(actual).toEqual(makeSymbol('greater'))
      })

      test('basic clause with more than one body expression', () => {
        const actual = evaluateInMce(`
      (cond ((> 3 2) (+ 1 2) - (*) 'greater))
    `)
        expect(actual).toEqual(makeSymbol('greater'))
      })
    })

    test('else clause reached', () => {
      const actual = evaluateInMce(`
      (cond ((> 3 3) 'greater)
            ((< 3 3) 'less)
            (else 'equal))
    `)
      expect(actual).toEqual(makeSymbol('equal'))
    })

    test('else clause not reached', () => {
      const actual = evaluateInMce(`
      (cond ((> 3 3) 'greater)
            ((< 3 4) 'less)
            (else 'equal))
    `)
      expect(actual).toEqual(makeSymbol('less'))
    })
  })

  describe('integration', () => {
    test('does not evaluate falsy clauses', () => {
      const actual = evaluateInMce(`
        (define x 10)
        (cond (false (set! x (+ x 1)))
              (false (set! x (+ x 1)))
              (true (set! x (+ x 1)))
              (false (set! x (+ x 1))))
        x
      `)
      expect(actual).toEqual(makeNumber(11))
    })

    test('evaluates exactly one truthy clause', () => {
      const actual = evaluateInMce(`
        (define x 10)
        (cond (false (set! x (+ x 1)))
              (false (set! x (+ x 1)))
              (true (set! x (+ x 1)))
              (true (set! x (+ x 1)))
              (true (set! x (+ x 1)))
              (false (set! x (+ x 1))))
        x
      `)
      expect(actual).toEqual(makeNumber(11))
    })
  })
})
