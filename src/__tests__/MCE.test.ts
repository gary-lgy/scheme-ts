import { makeList, makePair, Value } from '../interpreter/value'
import { makeBool, makeEmptyList, makeNumber, makeString, makeSymbol } from '../sExpression'
import { sicpMce } from '../stdlib/sicpMce'
import { prepareContext, runUntilDone } from '../testHelpers'
import { Variant } from '../types'

describe.each<Variant>(['base', 'no-tco', 'macro'])('MCE', variant => {
  function evaluateInMce(code: string): Value {
    const context = prepareContext(variant, sicpMce)
    return runUntilDone(`(eval '(begin ${code}) the-global-environment)`, context).value
  }

  describe('self-evaluating', () => {
    test('number', () => {
      expect(evaluateInMce('1')).toHaveMatchingValue(makeNumber(1))
    })

    test('string', () => {
      expect(evaluateInMce('"MCE is awesome"')).toHaveMatchingValue(makeString('MCE is awesome'))
    })
  })

  describe('variable', () => {
    test('definition', () => {
      expect(evaluateInMce('(define x 10) x')).toHaveMatchingValue(makeNumber(10))
    })

    test('assignment', () => {
      expect(evaluateInMce('(define x 10) (set! x 20) x')).toHaveMatchingValue(makeNumber(20))
    })
  })

  test('quoted', () => {
    expect(evaluateInMce(`'(1 "my-string" (3 (4)))`)).toHaveMatchingValue(
      makeList([
        makeNumber(1),
        makeString('my-string'),
        makeList([makeNumber(3), makeList([makeNumber(4)])])
      ])
    )
  })

  describe('if', () => {
    describe('with truthy predicate', () => {
      test('returns consequent', () => {
        expect(evaluateInMce("(if (< 0 1) 'consequent 'alternative)")).toHaveMatchingValue(
          makeSymbol('consequent', true)
        )
      })

      test('does not evaluate the alternative', () => {
        expect(
          evaluateInMce(`(if (< 0 1) 'consequent (error "alternative evaluated"))`)
        ).toHaveMatchingValue(makeSymbol('consequent', true))
      })
    })

    describe('with false predicate', () => {
      test('returns alternative', () => {
        expect(evaluateInMce("(if (> 0 1) 'consequent 'alternative)")).toHaveMatchingValue(
          makeSymbol('alternative', true)
        )
      })

      test('does not evaluate the alternative', () => {
        expect(
          evaluateInMce(`(if (> 0 1) (error "consequent evaluated") 'alternative)`)
        ).toHaveMatchingValue(makeSymbol('alternative', true))
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
        expect(evaluateInMce('(+ 1 2)')).toHaveMatchingValue(makeNumber(3))
        expect(evaluateInMce('(- 1 2)')).toHaveMatchingValue(makeNumber(-1))
        expect(evaluateInMce('(* 2 3)')).toHaveMatchingValue(makeNumber(6))
        expect(evaluateInMce('(/ 3 2)')).toHaveMatchingValue(makeNumber(1.5))

        expect(evaluateInMce('(= 1 1)')).toHaveMatchingValue(makeBool(true))
        expect(evaluateInMce('(= 1 2)')).toHaveMatchingValue(makeBool(false))
        expect(evaluateInMce('(< 1 2)')).toHaveMatchingValue(makeBool(true))
        expect(evaluateInMce('(< 2 1)')).toHaveMatchingValue(makeBool(false))
        expect(evaluateInMce('(> 1 2)')).toHaveMatchingValue(makeBool(false))
        expect(evaluateInMce('(> 2 1)')).toHaveMatchingValue(makeBool(true))

        expect(evaluateInMce('(cons 2 1)')).toHaveMatchingValue(
          makePair(makeNumber(2), makeNumber(1))
        )
        expect(evaluateInMce('(car (cons 2 1))')).toHaveMatchingValue(makeNumber(2))
        expect(evaluateInMce('(cdr (cons 2 1))')).toHaveMatchingValue(makeNumber(1))
        expect(
          evaluateInMce('(define my-pair (cons 2 1)) (set-car! my-pair 3) (car my-pair)')
        ).toHaveMatchingValue(makeNumber(3))
        expect(
          evaluateInMce('(define my-pair (cons 2 1)) (set-cdr! my-pair 3) (cdr my-pair)')
        ).toHaveMatchingValue(makeNumber(3))
        expect(evaluateInMce("(null? '())")).toHaveMatchingValue(makeBool(true))
        expect(evaluateInMce("(null? '(1))")).toHaveMatchingValue(makeBool(false))
        expect(evaluateInMce('(list)')).toHaveMatchingValue(makeEmptyList())
        expect(evaluateInMce('(list 1 2 3)')).toHaveMatchingValue(
          makeList([makeNumber(1), makeNumber(2), makeNumber(3)])
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
          expect(evaluateInMce('(define (fn x) (+ x 1)) (fn 2)')).toHaveMatchingValue(makeNumber(3))
          expect(evaluateInMce('((lambda (x) (+ x 1)) 2)')).toHaveMatchingValue(makeNumber(3))
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
          ).toHaveMatchingValue(makeList([makeNumber(3), makeNumber(2), makeNumber(4)]))
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
        ).toHaveMatchingValue(makeString('lambda'))
      })

      describe('cond', () => {
        test('basic clause with one body expression', () => {
          const actual = evaluateInMce(`
      (cond ((> 3 2) 'greater))
    `)
          expect(actual).toHaveMatchingValue(makeSymbol('greater', true))
        })

        test('basic clause with more than one body expression', () => {
          const actual = evaluateInMce(`
      (cond ((> 3 2) (+ 1 2) - (*) 'greater))
    `)
          expect(actual).toHaveMatchingValue(makeSymbol('greater', true))
        })
      })

      test('else clause reached', () => {
        const actual = evaluateInMce(`
      (cond ((> 3 3) 'greater)
            ((< 3 3) 'less)
            (else 'equal))
    `)
        expect(actual).toHaveMatchingValue(makeSymbol('equal', true))
      })

      test('else clause not reached', () => {
        const actual = evaluateInMce(`
      (cond ((> 3 3) 'greater)
            ((< 3 4) 'less)
            (else 'equal))
    `)
        expect(actual).toHaveMatchingValue(makeSymbol('less', true))
      })

      test('else clause with more than one expresions', () => {
        const actual = evaluateInMce(`
      (cond ((> 3 3) 'greater)
            ((< 3 3) 'less)
            (else 'equal 'for-real 'equal!))
    `)
        expect(actual).toHaveMatchingValue(makeSymbol('equal!', true))
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
        expect(actual).toHaveMatchingValue(makeNumber(11))
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
        expect(actual).toHaveMatchingValue(makeNumber(11))
      })
    })
  })
})
