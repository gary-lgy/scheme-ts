import { DefineSyntaxError } from '../errors/errors'
import {
  ExpressibleValue,
  makeBool,
  makeList,
  makeNumber,
  makeString,
  makeSymbol
} from '../interpreter/ExpressibleValue'
import { prepareContext, runUntilDone } from '../testHelpers'
import { Variant } from '../types'

describe.each<Variant>(['base', 'no-tco', 'macro'])('special forms', variant => {
  const evaluateUntilDone: (code: string) => ExpressibleValue = code => {
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
        expect(actual).toEqual(makeNumber(20))
      })

      describe('procedure variant', () => {
        test('fixed number of arguments', () => {
          const actual = evaluateUntilDone(`
        (define (fn x y)
          (+ x y))
        (fn 1 2)
      `)
          expect(actual).toEqual(makeNumber(3))
        })

        test('var args with one compulsory argument', () => {
          const actual = evaluateUntilDone(`
        (define (fn x . y)
          (cons x y))
        (fn 1 2 3)
      `)
          expect(actual).toEqual(makeList(makeNumber(1), makeNumber(2), makeNumber(3)))
        })

        test('var args with no compulsory arguments', () => {
          const actual = evaluateUntilDone(`
        (define (fn . y)
          y)
        (fn 1 2 3)
      `)
          expect(actual).toEqual(makeList(makeNumber(1), makeNumber(2), makeNumber(3)))
        })
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
        expect(() => evaluateUntilDone(`(let ((x 10)))`)).toThrow()
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
        expect(() => evaluateUntilDone(`(let* ((x 10)))`)).toThrow()
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
        expect(() => evaluateUntilDone(`(letrec ((x 10)))`)).toThrow()
      })

      test('no bindings', () => {
        expect(evaluateUntilDone(`(letrec () (+ 1 2))`)).toEqual(makeNumber(3))
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
        expect(actual).toEqual(makeNumber(3))
      })

      test('basic clause with one body expression', () => {
        const actual = evaluateUntilDone(`
      (cond ((> 3 2) 'greater))
    `)
        expect(actual).toEqual(makeSymbol('greater'))
      })

      test('basic clause with more than one body expression', () => {
        const actual = evaluateUntilDone(`
      (cond ((> 3 2) (+ 1 2) - (*) 'greater))
    `)
        expect(actual).toEqual(makeSymbol('greater'))
      })
    })

    describe('procedure clause', () => {
      test('calls the procedure with the test result', () => {
        const actual = evaluateUntilDone(`
      (cond ((cons 1 2) => car))
    `)
        expect(actual).toEqual(makeNumber(1))
      })
    })

    describe('else clause', () => {
      test('else clause reached', () => {
        const actual = evaluateUntilDone(`
      (cond ((> 3 3) 'greater)
            ((< 3 3) 'less)
            (else 'equal))
    `)
        expect(actual).toEqual(makeSymbol('equal'))
      })

      test('else clause not reached', () => {
        const actual = evaluateUntilDone(`
      (cond ((> 3 3) 'greater)
            ((< 3 4) 'less)
            (else 'equal))
    `)
        expect(actual).toEqual(makeSymbol('less'))
      })

      test('else clause with more than one expresions', () => {
        const actual = evaluateUntilDone(`
      (cond ((> 3 3) 'greater)
            ((< 3 3) 'less)
            (else 'equal 'for-real 'equal!))
    `)
        expect(actual).toEqual(makeSymbol('equal!'))
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
        expect(actual).toEqual(makeNumber(11))
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
        expect(actual).toEqual(makeNumber(11))
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
      ).toEqual(makeNumber(6))
    })
  })

  describe('logical', () => {
    describe('and', () => {
      describe('no argument', () => {
        test('return true', () => {
          expect(evaluateUntilDone('(and)')).toEqual(makeBool(true))
        })
      })

      describe('one argument', () => {
        test('returns argument', () => {
          expect(evaluateUntilDone('(and 1)')).toEqual(makeNumber(1))
          expect(evaluateUntilDone('(and "str")')).toEqual(makeString('str'))
        })
      })

      describe('more than one arguments', () => {
        describe('all arguments are truthy', () => {
          test('returns last argument', () => {
            expect(evaluateUntilDone(`(and "1" 'two '(3) 4)`)).toEqual(makeNumber(4))
          })
        })

        describe('at least one argument is false', () => {
          test('returns false', () => {
            expect(evaluateUntilDone(`(and "1" #f '(3) 4)`)).toEqual(makeBool(false))
          })
        })
      })
    })

    describe('or', () => {
      describe('no argument', () => {
        test('returns false', () => {
          expect(evaluateUntilDone('(or)')).toEqual(makeBool(false))
        })
      })

      describe('one argument', () => {
        test('returns argument', () => {
          expect(evaluateUntilDone('(or 1)')).toEqual(makeNumber(1))
          expect(evaluateUntilDone('(or "str")')).toEqual(makeString('str'))
        })
      })

      describe('more than one arguments', () => {
        describe('all arguments are false', () => {
          test('returns false', () => {
            expect(evaluateUntilDone(`(or #f #f #f #f)`)).toEqual(makeBool(false))
          })
        })

        describe('at least one argument is truthy', () => {
          test('returns first truthy argument', () => {
            expect(evaluateUntilDone(`(or #f #f 1 #f 2)`)).toEqual(makeNumber(1))
          })
        })
      })
    })
  })
})
