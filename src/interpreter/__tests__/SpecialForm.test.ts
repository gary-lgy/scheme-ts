import {
  CondProcedureClauseError,
  CondSyntaxError,
  DefineSyntaxError,
  LetSyntaxError
} from '../../errors/errors'
import { evaluateUntilDone } from '../../testHelpers'
import { makeBool, makeNumber, makeSymbol } from '../ExpressibleValue'

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

describe('cond', () => {
  describe('incorrect usage', () => {
    test('invalid syntax', () => {
      const programs = [
        '(cond)',
        '(cond 1)',
        '(cond ())',
        '(cond (#t) 2)',
        '(cond ((+ 1 1) =>))',
        '(cond ((+ 1 1) => * /))',
        '(cond (#t 1) (else 2) (#f 3))',
        '(cond (#t 1) (else))'
      ]
      programs.forEach(program => expect(() => evaluateUntilDone(program)).toThrow(CondSyntaxError))
    })

    test('body of procedure clause evaluated to non-procedure value', () => {
      expect(() => evaluateUntilDone('(cond (1 => 2))')).toThrow(CondProcedureClauseError)
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
