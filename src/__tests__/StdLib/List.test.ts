import { makeBool, makeList, makeNumber, makeSymbol } from '../../interpreter/ExpressibleValue'
import { evaluateUntilDone } from '../../testHelpers'

describe('list?', () => {
  test('returns whether the argument is a list', () => {
    expect(evaluateUntilDone('(list? 0)')).toEqual(makeBool(false))
    expect(evaluateUntilDone("(list? 'id)")).toEqual(makeBool(false))
    expect(evaluateUntilDone("(list? '())")).toEqual(makeBool(true))
    expect(evaluateUntilDone("(list? '(1))")).toEqual(makeBool(true))
    expect(evaluateUntilDone("(list? '(1 2))")).toEqual(makeBool(true))
    expect(evaluateUntilDone('(list? (cons 1 2))')).toEqual(makeBool(false))
    expect(evaluateUntilDone("(list? '(1 2 . 3))")).toEqual(makeBool(false))
  })
})

describe('length', () => {
  test('returns the correct list length', () => {
    expect(evaluateUntilDone("(length '())")).toEqual(makeNumber(0))
    expect(evaluateUntilDone("(length '(1))")).toEqual(makeNumber(1))
    expect(evaluateUntilDone("(length '(1 2))")).toEqual(makeNumber(2))
    expect(evaluateUntilDone("(length '(1 2 3))")).toEqual(makeNumber(3))
    expect(evaluateUntilDone("(length '(1 (2) (3 4)))")).toEqual(makeNumber(3))
  })
})

describe('list-tail', () => {
  test('returns the correct list tail', () => {
    expect(evaluateUntilDone("(list-tail '(1 2 3) 0)")).toEqual(
      makeList(makeNumber(1), makeNumber(2), makeNumber(3))
    )
    expect(evaluateUntilDone("(list-tail '(1 2 3) 1)")).toEqual(
      makeList(makeNumber(2), makeNumber(3))
    )
    expect(evaluateUntilDone("(list-tail '(1 2 3) 2)")).toEqual(makeList(makeNumber(3)))
    expect(evaluateUntilDone("(list-tail '(1 2 3) 3)")).toEqual(makeList())
  })
})

describe('list-ref', () => {
  test('returns the correct list head', () => {
    expect(evaluateUntilDone("(list-ref '(1 2 3) 0)")).toEqual(makeNumber(1))
    expect(evaluateUntilDone("(list-ref '(1 2 3) 1)")).toEqual(makeNumber(2))
    expect(evaluateUntilDone("(list-ref '(1 2 3) 2)")).toEqual(makeNumber(3))
  })
})

describe('append', () => {
  test('appends', () => {
    expect(evaluateUntilDone("(append '(x) '(y))")).toEqual(
      makeList(makeSymbol('x'), makeSymbol('y'))
    )
    expect(evaluateUntilDone("(append '(a) '(b c d))")).toEqual(
      makeList(makeSymbol('a'), makeSymbol('b'), makeSymbol('c'), makeSymbol('d'))
    )
    expect(evaluateUntilDone("(append '() '(b c d))")).toEqual(
      makeList(makeSymbol('b'), makeSymbol('c'), makeSymbol('d'))
    )
  })

  test("last list's identity is preserved", () => {
    expect(
      evaluateUntilDone(`
      (let ((xs '(a))
            (ys '(b c d)))
         (eq? (cdr (append xs ys)) ys))
         `)
    ).toEqual(makeBool(true))
  })
})

describe('reverse', () => {
  test('reverses the list', () => {
    expect(evaluateUntilDone("(reverse '())")).toEqual(makeList())
    expect(evaluateUntilDone("(reverse '(1))")).toEqual(makeList(makeNumber(1)))
    expect(evaluateUntilDone("(reverse '(1 2 3))")).toEqual(
      makeList(makeNumber(3), makeNumber(2), makeNumber(1))
    )
    expect(evaluateUntilDone("(reverse '(1 (2 3) 4 (5 (6))))")).toEqual(
      makeList(
        makeList(makeNumber(5), makeList(makeNumber(6))),
        makeNumber(4),
        makeList(makeNumber(2), makeNumber(3)),
        makeNumber(1)
      )
    )
  })
})

describe('map', () => {
  test('maps each element in the list', () => {
    expect(evaluateUntilDone("(map cadr '((a b) (c d) (e f)))")).toEqual(
      makeList(makeSymbol('b'), makeSymbol('d'), makeSymbol('f'))
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
    ).toEqual(makeList(makeNumber(1), makeNumber(2)))
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
    ).toEqual(makeNumber(15))
  })
})
