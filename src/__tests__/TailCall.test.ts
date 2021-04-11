import { makeNumber } from '../interpreter/SExpression'
import { prepareContext, runUntilDone } from '../testHelpers'
import { Variant } from '../types'

// The expected number of environments created is a loose estimate
// As long as it is way smaller than the number of recursive calls made, it is fine

describe.each<Variant>(['base', 'macro'])('tail call', variant => {
  const evaluateAndCountEnvironmentsUntilDone: (
    code: string
  ) => ReturnType<typeof runUntilDone> = code => {
    const context = prepareContext(variant)
    return runUntilDone(code, context)
  }

  // Sanity check: a non-tail-recursive function should create many environments
  describe('sanity check: recursion in non-tail context', () => {
    test('should create environments for each call', () => {
      const result = evaluateAndCountEnvironmentsUntilDone(`
    (define (fn x sum)
      (if (> x 0)
          (let ((new-value (fn (- x 1) (+ sum 1))))
            new-value)
          sum))
    (fn 100 0)
    `)
      expect(result.value).toHaveMatchingValue(makeNumber(100))
      expect(result.maxNumEnvironment).toBeGreaterThan(100)
    })
  })

  describe('should not create environments for each call', () => {
    describe('tail recursion in if form', () => {
      test('tail recursion in consequent', () => {
        const result = evaluateAndCountEnvironmentsUntilDone(`
    (define (fn x sum)
      (if (> x 0) (fn (- x 1) (+ sum 1)) sum))
    (fn 100 0)
    `)
        expect(result.value).toHaveMatchingValue(makeNumber(100))
        expect(result.maxNumEnvironment).toBeLessThan(20)
      })

      test('tail recursion in alternative', () => {
        const result = evaluateAndCountEnvironmentsUntilDone(`
    (define (fn x sum)
      (if (zero? x) sum (fn (- x 1) (+ sum 1))))
    (fn 100 0)
    `)
        expect(result.value).toHaveMatchingValue(makeNumber(100))
        expect(result.maxNumEnvironment).toBeLessThan(20)
      })
    })

    describe('tail recursion in cond form', () => {
      test('tail recursion in basic clause', () => {
        const result = evaluateAndCountEnvironmentsUntilDone(`
    (define (fn x sum)
      (cond
        ((> x 0) (+ 1 2) (fn (- x 1) (+ sum 1)))
        (else sum)))
    (fn 100 0)
    `)
        expect(result.value).toHaveMatchingValue(makeNumber(100))
        expect(result.maxNumEnvironment).toBeLessThan(20)
      })

      test('tail recursion in procedure clause', () => {
        const result = evaluateAndCountEnvironmentsUntilDone(`
    (define (fn x sum)
      (cond
        ((> x 0) => (lambda (ignored) (fn (- x 1) (+ sum 1))))
        (else sum)))
    (fn 100 0)
    `)
        expect(result.value).toHaveMatchingValue(makeNumber(100))
        expect(result.maxNumEnvironment).toBeLessThan(20)
      })

      test('tail recursion in else clause', () => {
        const result = evaluateAndCountEnvironmentsUntilDone(`
    (define (fn x sum)
      (cond
        ((<= x 0) sum)
        (else (+ 1 2) (fn (- x 1) (+ sum 1)))))
    (fn 100 0)
    `)
        expect(result.value).toHaveMatchingValue(makeNumber(100))
        expect(result.maxNumEnvironment).toBeLessThan(20)
      })
    })

    describe('tail recursion in and form', () => {
      test('tail recursion in last expression', () => {
        const result = evaluateAndCountEnvironmentsUntilDone(`
    (define (fn x sum)
      (and #t (+ 1 1) ((lambda () "string")) #t (if (> x 0) (fn (- x 1) (+ sum 1)) sum)))
    (fn 100 0)
    `)
        expect(result.value).toHaveMatchingValue(makeNumber(100))
        expect(result.maxNumEnvironment).toBeLessThan(20)
      })
    })

    describe('tail recursion in or form', () => {
      test('tail recursion in last expression', () => {
        const result = evaluateAndCountEnvironmentsUntilDone(`
    (define (fn x sum)
      (or #f #f ((lambda () #f)) #f (if (> x 0) (fn (- x 1) (+ sum 1)) sum)))
    (fn 100 0)
    `)
        expect(result.value).toHaveMatchingValue(makeNumber(100))
        expect(result.maxNumEnvironment).toBeLessThan(20)
      })
    })

    describe('tail recursion in let form', () => {
      test('tail recursion in last expression', () => {
        const result = evaluateAndCountEnvironmentsUntilDone(`
    (define (fn x sum)
      (let ((new-x (- x 1))
            (new-sum (+ sum 1)))
        "do something funny"
        (+ 1 2)
        (if (> x 0) (fn new-x new-sum) sum)))
    (fn 100 0)
    `)
        expect(result.value).toHaveMatchingValue(makeNumber(100))
        expect(result.maxNumEnvironment).toBeLessThan(20)
      })
    })

    describe('tail recursion in let* form', () => {
      test('tail recursion in last expression', () => {
        const result = evaluateAndCountEnvironmentsUntilDone(`
    (define (fn x sum)
      (let* ((dummy (+ x 1))
             (new-x (- dummy 2))
             (new-sum (+ sum 1)))
        "do something funny"
        (+ 1 2)
        (if (> x 0) (fn new-x new-sum) sum)))
    (fn 100 0)
    `)
        expect(result.value).toHaveMatchingValue(makeNumber(100))
        expect(result.maxNumEnvironment).toBeLessThan(20)
      })
    })

    describe('tail recursion in letrec form', () => {
      test('tail recursion in last expression', () => {
        const result = evaluateAndCountEnvironmentsUntilDone(`
    (define (fn x sum)
      (letrec ((new-x (- x 1))
               (new-sum (+ sum 1)))
        "do something funny"
        (+ 1 2)
        (if (> x 0) (fn new-x new-sum) sum)))
    (fn 100 0)
    `)
        expect(result.value).toHaveMatchingValue(makeNumber(100))
        expect(result.maxNumEnvironment).toBeLessThan(20)
      })
    })

    describe('tail recursion in begin form', () => {
      test('tail recursion in last expression', () => {
        const result = evaluateAndCountEnvironmentsUntilDone(`
    (define (fn x sum)
      (begin
        (+ 1 2)
        "do something funny"
        (if (> x 0) (fn (- x 1) (+ sum 1)) sum)))
    (fn 100 0)
    `)
        expect(result.value).toHaveMatchingValue(makeNumber(100))
        expect(result.maxNumEnvironment).toBeLessThan(20)
      })
    })

    test('non-recursive tail call', () => {
      const result = evaluateAndCountEnvironmentsUntilDone(`
    (define (f x sum)
      (if (> x 0) (g (- x 1) (+ sum 1)) sum))
    (define (g x sum)
      (if (> x 0) (f (- x 1) (+ sum 1)) sum))
    (f 100 0)
    `)
      expect(result.value).toHaveMatchingValue(makeNumber(100))
      expect(result.maxNumEnvironment).toBeLessThan(20)
    })

    describe('recursion by calling apply', () => {
      test('recursive', () => {
        const result = evaluateAndCountEnvironmentsUntilDone(`
    (define (fn x sum)
      (if (> x 0) (apply fn (list (- x 1) (+ sum 1))) sum))
    (fn 100 0)
    `)
        expect(result.value).toHaveMatchingValue(makeNumber(100))
        expect(result.maxNumEnvironment).toBeLessThan(20)
      })

      test('non recursive', () => {
        const result = evaluateAndCountEnvironmentsUntilDone(`
    (define (f x sum)
      (if (> x 0) (apply g (list (- x 1) (+ sum 1))) sum))
    (define (g x sum)
      (if (> x 0) (apply f (list (- x 1) (+ sum 1))) sum))
    (f 100 0)
    `)
        expect(result.value).toHaveMatchingValue(makeNumber(100))
        expect(result.maxNumEnvironment).toBeLessThan(20)
      })
    })
  })
})
