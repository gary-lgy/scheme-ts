import { makeEmptyList, makeNumber } from '../../interpreter/SExpression'
import { makePair, Value } from '../../interpreter/Value'
import { prepareContext, runUntilDone } from '../../testHelpers'
import { Variant } from '../../types'

const list = "'((((1 2) 3 4) (5 6) 7 8) ((9 10) 11 12) (13) 14 15)"

describe.each<Variant>(['base', 'no-tco', 'macro'])('miscellaneous library features', variant => {
  function evaluateUntilDone(code: string): Value {
    const context = prepareContext(variant)
    return runUntilDone(code, context).value
  }

  test('caar', () => {
    expect(evaluateUntilDone(`(caar  ${list})`)).toHaveMatchingValue(
      makePair(
        makePair(makeNumber(1), makePair(makeNumber(2), makeEmptyList())),
        makePair(makeNumber(3), makePair(makeNumber(4), makeEmptyList()))
      )
    )
  })

  test('cadr', () => {
    expect(evaluateUntilDone(`(cadr  ${list})`)).toHaveMatchingValue(
      makePair(
        makePair(makeNumber(9), makePair(makeNumber(10), makeEmptyList())),
        makePair(makeNumber(11), makePair(makeNumber(12), makeEmptyList()))
      )
    )
  })

  test('cdar', () => {
    expect(evaluateUntilDone(`(cdar  ${list})`)).toHaveMatchingValue(
      makePair(
        makePair(makeNumber(5), makePair(makeNumber(6), makeEmptyList())),
        makePair(makeNumber(7), makePair(makeNumber(8), makeEmptyList()))
      )
    )
  })

  test('cddr', () => {
    expect(evaluateUntilDone(`(cddr  ${list})`)).toHaveMatchingValue(
      makePair(
        makePair(makeNumber(13), makeEmptyList()),
        makePair(makeNumber(14), makePair(makeNumber(15), makeEmptyList()))
      )
    )
  })

  test('caaar', () => {
    expect(evaluateUntilDone(`(caaar  ${list})`)).toHaveMatchingValue(
      makePair(makeNumber(1), makePair(makeNumber(2), makeEmptyList()))
    )
  })

  test('caadr', () => {
    expect(evaluateUntilDone(`(caadr  ${list})`)).toHaveMatchingValue(
      makePair(makeNumber(9), makePair(makeNumber(10), makeEmptyList()))
    )
  })

  test('cadar', () => {
    expect(evaluateUntilDone(`(cadar  ${list})`)).toHaveMatchingValue(
      makePair(makeNumber(5), makePair(makeNumber(6), makeEmptyList()))
    )
  })

  test('caddr', () => {
    expect(evaluateUntilDone(`(caddr  ${list})`)).toHaveMatchingValue(
      makePair(makeNumber(13), makeEmptyList())
    )
  })

  test('cdaar', () => {
    expect(evaluateUntilDone(`(cdaar  ${list})`)).toHaveMatchingValue(
      makePair(makeNumber(3), makePair(makeNumber(4), makeEmptyList()))
    )
  })

  test('cdadr', () => {
    expect(evaluateUntilDone(`(cdadr  ${list})`)).toHaveMatchingValue(
      makePair(makeNumber(11), makePair(makeNumber(12), makeEmptyList()))
    )
  })

  test('cddar', () => {
    expect(evaluateUntilDone(`(cddar  ${list})`)).toHaveMatchingValue(
      makePair(makeNumber(7), makePair(makeNumber(8), makeEmptyList()))
    )
  })

  test('cdddr', () => {
    expect(evaluateUntilDone(`(cdddr  ${list})`)).toHaveMatchingValue(
      makePair(makeNumber(14), makePair(makeNumber(15), makeEmptyList()))
    )
  })

  test('caaaar', () => {
    expect(evaluateUntilDone(`(caaaar  ${list})`)).toHaveMatchingValue(makeNumber(1))
  })

  test('caaadr', () => {
    expect(evaluateUntilDone(`(caaadr  ${list})`)).toHaveMatchingValue(makeNumber(9))
  })

  test('caadar', () => {
    expect(evaluateUntilDone(`(caadar  ${list})`)).toHaveMatchingValue(makeNumber(5))
  })

  test('caaddr', () => {
    expect(evaluateUntilDone(`(caaddr  ${list})`)).toHaveMatchingValue(makeNumber(13))
  })

  test('cadaar', () => {
    expect(evaluateUntilDone(`(cadaar  ${list})`)).toHaveMatchingValue(makeNumber(3))
  })

  test('cadadr', () => {
    expect(evaluateUntilDone(`(cadadr  ${list})`)).toHaveMatchingValue(makeNumber(11))
  })

  test('caddar', () => {
    expect(evaluateUntilDone(`(caddar  ${list})`)).toHaveMatchingValue(makeNumber(7))
  })

  test('cadddr', () => {
    expect(evaluateUntilDone(`(cadddr  ${list})`)).toHaveMatchingValue(makeNumber(14))
  })

  test('cdaaar', () => {
    expect(evaluateUntilDone(`(cdaaar  ${list})`)).toHaveMatchingValue(
      makePair(makeNumber(2), makeEmptyList())
    )
  })

  test('cdaadr', () => {
    expect(evaluateUntilDone(`(cdaadr  ${list})`)).toHaveMatchingValue(
      makePair(makeNumber(10), makeEmptyList())
    )
  })

  test('cdadar', () => {
    expect(evaluateUntilDone(`(cdadar  ${list})`)).toHaveMatchingValue(
      makePair(makeNumber(6), makeEmptyList())
    )
  })

  test('cdaddr', () => {
    expect(evaluateUntilDone(`(cdaddr  ${list})`)).toHaveMatchingValue(makeEmptyList())
  })

  test('cddaar', () => {
    expect(evaluateUntilDone(`(cddaar  ${list})`)).toHaveMatchingValue(
      makePair(makeNumber(4), makeEmptyList())
    )
  })

  test('cddadr', () => {
    expect(evaluateUntilDone(`(cddadr  ${list})`)).toHaveMatchingValue(
      makePair(makeNumber(12), makeEmptyList())
    )
  })

  test('cdddar', () => {
    expect(evaluateUntilDone(`(cdddar  ${list})`)).toHaveMatchingValue(
      makePair(makeNumber(8), makeEmptyList())
    )
  })

  test('cddddr', () => {
    expect(evaluateUntilDone(`(cddddr  ${list})`)).toHaveMatchingValue(
      makePair(makeNumber(15), makeEmptyList())
    )
  })
})
