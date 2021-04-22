import { DefineSyntaxError } from '../errors/errors'
import { makeBool, makeNumber, makeString, makeSymbol } from '../interpreter/sExpression'
import { makeList, Value } from '../interpreter/value'
import { prepareContext, runUntilDone } from '../testHelpers'
import { Variant } from '../types'

describe.each<Variant>(['base', 'no-tco', 'macro'])('special forms', variant => {
  const evaluateUntilDone: (code: string) => Value = code => {
    const context = prepareContext(variant)
    return runUntilDone(code, context).value
  }

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
        expect(actual).toHaveMatchingValue(makeNumber(20))
      })

      describe('procedure variant', () => {
        test('fixed number of arguments', () => {
          const actual = evaluateUntilDone(`
        (define (fn x y)
          (+ x y))
        (fn 1 2)
      `)
          expect(actual).toHaveMatchingValue(makeNumber(3))
        })

        test('var args with one compulsory argument', () => {
          const actual = evaluateUntilDone(`
        (define (fn x . y)
          (cons x y))
        (fn 1 2 3)
      `)
          expect(actual).toHaveMatchingValue(
            makeList([makeNumber(1), makeNumber(2), makeNumber(3)])
          )
        })

        test('var args with no compulsory arguments', () => {
          const actual = evaluateUntilDone(`
        (define (fn . y)
          y)
        (fn 1 2 3)
      `)
          expect(actual).toHaveMatchingValue(
            makeList([makeNumber(1), makeNumber(2), makeNumber(3)])
          )
        })
      })
    })

    describe('let', () => {
      test('basic', () => {
        const actual = evaluateUntilDone(`
        (let ((x 2) (y 3))
          (* x y))
      `)
        expect(actual).toHaveMatchingValue(makeNumber(6))
      })

      test('with shadowing binding', () => {
        const actual = evaluateUntilDone(`
        (let ((x 2) (y 3))
          (let ((x 7)
                (z (+ x y)))
            (* z x)))
      `)
        expect(actual).toHaveMatchingValue(makeNumber(35))
      })

      test('no body', () => {
        expect(() => evaluateUntilDone(`(let ((x 10)))`)).toThrow()
      })

      test('no bindings', () => {
        expect(evaluateUntilDone(`(let () (+ 1 2))`)).toHaveMatchingValue(makeNumber(3))
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
        expect(actual).toHaveMatchingValue(makeNumber(70))
      })

      test('no body', () => {
        expect(() => evaluateUntilDone(`(let* ((x 10)))`)).toThrow()
      })

      test('no bindings', () => {
        expect(evaluateUntilDone(`(let* () (+ 1 2))`)).toHaveMatchingValue(makeNumber(3))
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
        expect(actual).toHaveMatchingValue(makeBool(true))
      })

      test('no body', () => {
        expect(() => evaluateUntilDone(`(letrec ((x 10)))`)).toThrow()
      })

      test('no bindings', () => {
        expect(evaluateUntilDone(`(letrec () (+ 1 2))`)).toHaveMatchingValue(makeNumber(3))
      })
    })
  })

  describe('cond', () => {
    describe('incorrect usage', () => {
      test('invalid syntax', () => {
        expect(() => evaluateUntilDone('(cond)')).toThrow()
        expect(() => evaluateUntilDone('(cond 1)')).toThrow()
        expect(() => evaluateUntilDone('(cond ())')).toThrow()
        expect(() => evaluateUntilDone('(cond (#t) 2)')).toThrow()
        expect(() => evaluateUntilDone('(cond ((+ 1 1) =>))')).toThrow()
        expect(() => evaluateUntilDone('(cond ((+ 1 1) => * /))')).toThrow()
        expect(() => evaluateUntilDone('(cond (#t 1) (else 2) (#f 3))')).toThrow()
        expect(() => evaluateUntilDone('(cond (#t 1) (else))')).toThrow()
      })

      test('body of procedure clause evaluated to non-procedure value', () => {
        expect(() => evaluateUntilDone('(cond (1 => 2))')).toThrow()
      })
    })

    describe('basic clause', () => {
      test('empty basic clause', () => {
        const actual = evaluateUntilDone(`
      (cond ((+ 1 2)))
    `)
        expect(actual).toHaveMatchingValue(makeNumber(3))
      })

      test('basic clause with one body expression', () => {
        const actual = evaluateUntilDone(`
      (cond ((> 3 2) 'greater))
    `)
        expect(actual).toHaveMatchingValue(makeSymbol('greater', true))
      })

      test('basic clause with more than one body expression', () => {
        const actual = evaluateUntilDone(`
      (cond ((> 3 2) (+ 1 2) - (*) 'greater))
    `)
        expect(actual).toHaveMatchingValue(makeSymbol('greater', true))
      })
    })

    describe('procedure clause', () => {
      test('calls the procedure with the test result', () => {
        const actual = evaluateUntilDone(`
      (cond ((cons 1 2) => car))
    `)
        expect(actual).toHaveMatchingValue(makeNumber(1))
      })
    })

    describe('else clause', () => {
      test('else clause reached', () => {
        const actual = evaluateUntilDone(`
      (cond ((> 3 3) 'greater)
            ((< 3 3) 'less)
            (else 'equal))
    `)
        expect(actual).toHaveMatchingValue(makeSymbol('equal', true))
      })

      test('else clause not reached', () => {
        const actual = evaluateUntilDone(`
      (cond ((> 3 3) 'greater)
            ((< 3 4) 'less)
            (else 'equal))
    `)
        expect(actual).toHaveMatchingValue(makeSymbol('less', true))
      })

      test('else clause with more than one expresions', () => {
        const actual = evaluateUntilDone(`
      (cond ((> 3 3) 'greater)
            ((< 3 3) 'less)
            (else 'equal 'for-real 'equal!))
    `)
        expect(actual).toHaveMatchingValue(makeSymbol('equal!', true))
      })
    })

    describe('integration', () => {
      test('does not evaluate falsy clauses', () => {
        const actual = evaluateUntilDone(`
        (define x 10)
        (cond (#f (set! x (+ x 1)))
              (#f (set! x (+ x 1)))
              (#t (set! x (+ x 1)))
              (#f (set! x (+ x 1))))
        x
      `)
        expect(actual).toHaveMatchingValue(makeNumber(11))
      })

      test('evaluates exactly one truthy clause', () => {
        const actual = evaluateUntilDone(`
        (define x 10)
        (cond (#f (set! x (+ x 1)))
              (#f (set! x (+ x 1)))
              (#t (set! x (+ x 1)))
              (#t (set! x (+ x 1)))
              (#t (set! x (+ x 1)))
              (#f (set! x (+ x 1))))
        x
      `)
        expect(actual).toHaveMatchingValue(makeNumber(11))
      })
    })
  })

  describe('begin', () => {
    test('invalid syntax', () => {
      expect(() => evaluateUntilDone('(begin)')).toThrow()
    })

    test('basic', () => {
      expect(
        evaluateUntilDone(`
      (define x 0)
      (begin (set! x 5)
            (+ x 1))
    `)
      ).toHaveMatchingValue(makeNumber(6))
    })
  })

  describe('logical', () => {
    describe('and', () => {
      describe('no argument', () => {
        test('return true', () => {
          expect(evaluateUntilDone('(and)')).toHaveMatchingValue(makeBool(true))
        })
      })

      describe('one argument', () => {
        test('returns argument', () => {
          expect(evaluateUntilDone('(and 1)')).toHaveMatchingValue(makeNumber(1))
          expect(evaluateUntilDone('(and "str")')).toHaveMatchingValue(makeString('str'))
        })
      })

      describe('more than one arguments', () => {
        describe('all arguments are truthy', () => {
          test('returns last argument', () => {
            expect(evaluateUntilDone(`(and "1" 'two '(3) 4)`)).toHaveMatchingValue(makeNumber(4))
          })
        })

        describe('at least one argument is false', () => {
          test('returns false', () => {
            expect(evaluateUntilDone(`(and "1" #f '(3) 4)`)).toHaveMatchingValue(makeBool(false))
          })
        })
      })
    })

    describe('or', () => {
      describe('no argument', () => {
        test('returns false', () => {
          expect(evaluateUntilDone('(or)')).toHaveMatchingValue(makeBool(false))
        })
      })

      describe('one argument', () => {
        test('returns argument', () => {
          expect(evaluateUntilDone('(or 1)')).toHaveMatchingValue(makeNumber(1))
          expect(evaluateUntilDone('(or "str")')).toHaveMatchingValue(makeString('str'))
        })
      })

      describe('more than one arguments', () => {
        describe('all arguments are false', () => {
          test('returns false', () => {
            expect(evaluateUntilDone(`(or #f #f #f #f)`)).toHaveMatchingValue(makeBool(false))
          })
        })

        describe('at least one argument is truthy', () => {
          test('returns first truthy argument', () => {
            expect(evaluateUntilDone(`(or #f #f 1 #f 2)`)).toHaveMatchingValue(makeNumber(1))
          })
        })
      })
    })
  })
})
