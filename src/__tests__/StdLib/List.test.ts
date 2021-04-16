import { Variant } from '../..'
import { makeBool, makeNumber, makeSymbol } from '../../interpreter/SExpression'
import { makeList, Value } from '../../interpreter/Value'
import { prepareContext, runUntilDone } from '../../testHelpers'

describe.each<Variant>(['base', 'no-tco', 'macro'])('miscellaneous library features', variant => {
  function evaluateUntilDone(code: string): Value {
    const context = prepareContext(variant)
    return runUntilDone(code, context).value
  }

  describe('list?', () => {
    test('returns whether the argument is a list', () => {
      expect(evaluateUntilDone('(list? 0)')).toHaveMatchingValue(makeBool(false))
      expect(evaluateUntilDone("(list? 'id)")).toHaveMatchingValue(makeBool(false))
      expect(evaluateUntilDone("(list? '())")).toHaveMatchingValue(makeBool(true))
      expect(evaluateUntilDone("(list? '(1))")).toHaveMatchingValue(makeBool(true))
      expect(evaluateUntilDone("(list? '(1 2))")).toHaveMatchingValue(makeBool(true))
      expect(evaluateUntilDone('(list? (cons 1 2))')).toHaveMatchingValue(makeBool(false))
      expect(evaluateUntilDone("(list? '(1 2 . 3))")).toHaveMatchingValue(makeBool(false))
    })
  })

  describe('length', () => {
    test('returns the correct list length', () => {
      expect(evaluateUntilDone("(length '())")).toHaveMatchingValue(makeNumber(0))
      expect(evaluateUntilDone("(length '(1))")).toHaveMatchingValue(makeNumber(1))
      expect(evaluateUntilDone("(length '(1 2))")).toHaveMatchingValue(makeNumber(2))
      expect(evaluateUntilDone("(length '(1 2 3))")).toHaveMatchingValue(makeNumber(3))
      expect(evaluateUntilDone("(length '(1 (2) (3 4)))")).toHaveMatchingValue(makeNumber(3))
    })
  })

  describe('list-tail', () => {
    test('returns the correct list tail', () => {
      expect(evaluateUntilDone("(list-tail '(1 2 3) 0)")).toHaveMatchingValue(
        makeList([makeNumber(1), makeNumber(2), makeNumber(3)])
      )
      expect(evaluateUntilDone("(list-tail '(1 2 3) 1)")).toHaveMatchingValue(
        makeList([makeNumber(2), makeNumber(3)])
      )
      expect(evaluateUntilDone("(list-tail '(1 2 3) 2)")).toHaveMatchingValue(
        makeList([makeNumber(3)])
      )
      expect(evaluateUntilDone("(list-tail '(1 2 3) 3)")).toHaveMatchingValue(makeList([]))
    })
  })

  describe('list-ref', () => {
    test('returns the correct list head', () => {
      expect(evaluateUntilDone("(list-ref '(1 2 3) 0)")).toHaveMatchingValue(makeNumber(1))
      expect(evaluateUntilDone("(list-ref '(1 2 3) 1)")).toHaveMatchingValue(makeNumber(2))
      expect(evaluateUntilDone("(list-ref '(1 2 3) 2)")).toHaveMatchingValue(makeNumber(3))
    })
  })

  describe('append', () => {
    test('appends', () => {
      expect(evaluateUntilDone("(append '(x) '(y))")).toHaveMatchingValue(
        makeList([makeSymbol('x', true), makeSymbol('y', true)])
      )
      expect(evaluateUntilDone("(append '(a) '(b c d))")).toHaveMatchingValue(
        makeList([
          makeSymbol('a', true),
          makeSymbol('b', true),
          makeSymbol('c', true),
          makeSymbol('d', true)
        ])
      )
      expect(evaluateUntilDone("(append '() '(b c d))")).toHaveMatchingValue(
        makeList([makeSymbol('b', true), makeSymbol('c', true), makeSymbol('d', true)])
      )
    })

    test("last list's identity is preserved", () => {
      expect(
        evaluateUntilDone(`
      (let ((xs '(a))
            (ys '(b c d)))
         (eq? (cdr (append xs ys)) ys))
         `)
      ).toHaveMatchingValue(makeBool(true))
    })
  })

  describe('reverse', () => {
    test('reverses the list', () => {
      expect(evaluateUntilDone("(reverse '())")).toHaveMatchingValue(makeList([]))
      expect(evaluateUntilDone("(reverse '(1))")).toHaveMatchingValue(makeList([makeNumber(1)]))
      expect(evaluateUntilDone("(reverse '(1 2 3))")).toHaveMatchingValue(
        makeList([makeNumber(3), makeNumber(2), makeNumber(1)])
      )
      expect(evaluateUntilDone("(reverse '(1 (2 3) 4 (5 (6))))")).toHaveMatchingValue(
        makeList([
          makeList([makeNumber(5), makeList([makeNumber(6)])]),
          makeNumber(4),
          makeList([makeNumber(2), makeNumber(3)]),
          makeNumber(1)
        ])
      )
    })
  })

  describe('map', () => {
    test('maps each element in the list', () => {
      expect(evaluateUntilDone("(map cadr '((a b) (c d) (e f)))")).toHaveMatchingValue(
        makeList([makeSymbol('b', true), makeSymbol('d', true), makeSymbol('f', true)])
      )
    })

    test('procedure has side effects', () => {
      expect(
        evaluateUntilDone(`
        (let ((count 0))
          (map (lambda (ignored)
                (set! count (+ count 1))
                count)
               '(a b)))
      `)
      ).toHaveMatchingValue(makeList([makeNumber(1), makeNumber(2)]))
    })
  })

  describe('for-each', () => {
    test('side effects are observed', () => {
      expect(
        evaluateUntilDone(`
      (let ((count 0))
        (for-each
          (lambda (i) (set! count (+ count i)))
          '(0 1 2 3 4 5))
        count)
      `)
      ).toHaveMatchingValue(makeNumber(15))
    })
  })
})
