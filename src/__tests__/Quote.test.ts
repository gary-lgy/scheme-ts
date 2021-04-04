import {
  QuoteSyntaxError,
  UnquoteInWrongContext,
  UnquoteSplicingEvaluatedToNonList,
  UnquoteSplicingInNonListContext
} from '../errors/errors'
import {
  EVBool,
  EVNumber,
  EVString,
  EVSymbol,
  makeBool,
  makeList,
  makeNumber,
  makePair,
  makeString,
  makeSymbol
} from '../interpreter/ExpressibleValue'
import { evaluateUntilDone } from '../testHelpers'
import { stringify } from '../utils/stringify'

describe('synatx', () => {
  test('must take exactly one argument', () => {
    expect(() => evaluateUntilDone('(quote 1 2)')).toThrow(QuoteSyntaxError)
    expect(() => evaluateUntilDone('(quasiquote 1 2)')).toThrow(QuoteSyntaxError)
    expect(() => evaluateUntilDone('(unquote 1 2)')).toThrow(QuoteSyntaxError)
    expect(() => evaluateUntilDone('(unquote-splicing 1 2)')).toThrow(QuoteSyntaxError)
  })

  test('unquote must appear within quasiquotion', () => {
    expect(() => evaluateUntilDone(',(+ 1 2)')).toThrow(UnquoteInWrongContext)
    expect(() => evaluateUntilDone('`(1 2 ,(cons 1 ,(+ 1 2)))')).toThrow(UnquoteInWrongContext)
  })

  test('unquote-splicing must appear within list context', () => {
    expect(() => evaluateUntilDone('`,@(+ 1 2)')).toThrow(UnquoteSplicingInNonListContext)
  })

  test('unquote-splicing must evaluate to a list', () => {
    expect(() => evaluateUntilDone('`(1 2 ,@(+ 1 2))')).toThrow(UnquoteSplicingEvaluatedToNonList)
  })
})

describe('redefined', () => {
  test('redefined', () => {
    const programs: [string, string][] = [
      ['(define quote car) (quote (cons 1 2))', '1'],
      ["(define quote car) '(cons 1 2)", '1'],
      ['(define quasiquote car) (quasiquote (cons 1 2))', '1'],
      ['(define quasiquote car) `(cons 1 2)', '1'],
      ['(define unquote car) `(1 2 (unquote (cons 3 4)))', '(1 2 (unquote (cons 3 4)))'],
      ['(define unquote car) `(1 2 ,(cons 3 4))', '(1 2 (unquote (cons 3 4)))'],
      [
        '(define unquote-splicing car) `(1 2 (unquote-splicing (cons 3 4)))',
        '(1 2 (unquote-splicing (cons 3 4)))'
      ],
      ['(define unquote-splicing car) `(1 2 ,@(cons 3 4))', '(1 2 (unquote-splicing (cons 3 4)))']
    ]
    programs.forEach(program =>
      expect(stringify(evaluateUntilDone(program[0]))).toEqual(program[1])
    )
  })
})

describe('quote', () => {
  test('quote number', () => {
    const actual = evaluateUntilDone("'1")
    const expected: EVNumber = makeNumber(1)
    expect(actual).toEqual(expected)
  })

  test('quote string', () => {
    const actual = evaluateUntilDone(`'"hello world"`)
    const expected: EVString = makeString('hello world')
    expect(actual).toEqual(expected)
  })

  test('quote bool', () => {
    const actual = evaluateUntilDone("'#t")
    const expected: EVBool = makeBool(true)
    expect(actual).toEqual(expected)
  })

  test('quote identifier', () => {
    const actual = evaluateUntilDone("'my-pair")
    const expected: EVSymbol = makeSymbol('my-pair')
    expect(actual).toEqual(expected)
  })

  test('quote quoted', () => {
    const actual = evaluateUntilDone(`''a`)
    const expected = makeList(makeSymbol('quote'), makeSymbol('a'))
    expect(actual).toEqual(expected)
  })

  test('quote simple list', () => {
    const actual = evaluateUntilDone(`'(1 2 "my-string" #f my-symbol)`)
    const expected = makeList(
      makeNumber(1),
      makeNumber(2),
      makeString('my-string'),
      makeBool(false),
      makeSymbol('my-symbol')
    )
    expect(actual).toEqual(expected)
  })

  test('quote empty list', () => {
    const actual = evaluateUntilDone(`'()`)
    const expected = makeList()
    expect(actual).toEqual(expected)
  })

  test('quote nested list', () => {
    const actual = evaluateUntilDone(`'(1 2 '(3 4 '(5 6)))`)
    const expected = makeList(
      makeNumber(1),
      makeNumber(2),
      makeList(
        makeSymbol('quote'),
        makeList(
          makeNumber(3),
          makeNumber(4),
          makeList(makeSymbol('quote'), makeList(makeNumber(5), makeNumber(6)))
        )
      )
    )
    expect(actual).toEqual(expected)
  })

  test('quote using list special form', () => {
    const actual = evaluateUntilDone(`(quote (+ 1 2))`)
    const expected = makeList(makeSymbol('+'), makeNumber(1), makeNumber(2))
    expect(actual).toEqual(expected)
  })

  test('quote with mixed shorthand and special form', () => {
    const actual = evaluateUntilDone(`'(quote (+ 1 2))`)
    const expected = makeList(
      makeSymbol('quote'),
      makeList(makeSymbol('+'), makeNumber(1), makeNumber(2))
    )
    expect(actual).toEqual(expected)
  })

  test('quote with quasiquote and unquote inside quoted expression', () => {
    const actual = stringify(evaluateUntilDone("'(1 2 `(3 4 ,(+ 1 2) ,@(list a b)))"))
    const expected = '(1 2 (quasiquote (3 4 (unquote (+ 1 2)) (unquote-splicing (list a b)))))'
    expect(actual).toEqual(expected)
  })

  test('quoted quasiquote and unquote', () => {
    const actual = stringify(evaluateUntilDone(`'(quasiquote (list (unquote (+ 1 2)) 4))`))
    const expected = '(quasiquote (list (unquote (+ 1 2)) 4))'
    expect(actual).toEqual(expected)
  })

  test('quote dotted list', () => {
    const actual = evaluateUntilDone(`
      '(1 2 . 3)
    `)
    const expected = makePair(makeNumber(1), makePair(makeNumber(2), makeNumber(3)))
    expect(actual).toEqual(expected)
  })
})

describe('quasiquote', () => {
  test('no unquote', () => {
    const actual = evaluateUntilDone('`(1 2 "my-string" #f my-symbol)')
    const expected = makeList(
      makeNumber(1),
      makeNumber(2),
      makeString('my-string'),
      makeBool(false),
      makeSymbol('my-symbol')
    )
    expect(actual).toEqual(expected)
  })

  test('simple unquote', () => {
    const actual = evaluateUntilDone('`(list ,(+ 1 2) 4)')
    const expected = makeList(makeSymbol('list'), makeNumber(3), makeNumber(4))
    expect(actual).toEqual(expected)
  })

  test('unquote expanding identifier', () => {
    const actual = evaluateUntilDone("(define name 'a) `(list ,name ',name)")
    const expected = makeList(
      makeSymbol('list'),
      makeSymbol('a'),
      makeList(makeSymbol('quote'), makeSymbol('a'))
    )
    expect(actual).toEqual(expected)
  })

  test('unquote-splicing', () => {
    const actual = evaluateUntilDone("`(a ,(+ 1 2) ,@(cons 3 (cons 4 '())) b)")
    const expected = makeList(
      makeSymbol('a'),
      makeNumber(3),
      makeNumber(3),
      makeNumber(4),
      makeSymbol('b')
    )
    expect(actual).toEqual(expected)
  })

  test('unquote & unquote-splicing', () => {
    const actual = evaluateUntilDone("`(( foo ,(- 10 3)) ,@(cdr '(c)) ,(car '(cons)))")
    const expected = makeList(makeList(makeSymbol('foo'), makeNumber(7)), {
      type: 'EVSymbol',
      value: 'cons'
    })
    expect(actual).toEqual(expected)
  })

  test('multilevel unquote', () => {
    const actual = stringify(evaluateUntilDone('`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)'))
    const expected = '(a (quasiquote (b (unquote (+ 1 2)) (unquote (foo 4 d)) e)) f)'
    expect(actual).toEqual(expected)
  })

  test('multilevel unquote with identifiers', () => {
    const actual = stringify(
      evaluateUntilDone(`
      (define name1 'x)
      (define name2 'y)
      \`(a \`(b ,,name1 ,',name2 d) e)`)
    )
    const expected = '(a (quasiquote (b (unquote x) (unquote (quote y)) d)) e)'
    expect(actual).toEqual(expected)
  })

  test('multilevel unquote with splicing', () => {
    const actual = stringify(evaluateUntilDone('`(a `(b ,@(c ,(+ 1 2))))'))
    const expected = '(a (quasiquote (b (unquote-splicing (c 3)))))'
    expect(actual).toEqual(expected)
  })

  test('quasiquote using list special form', () => {
    const actual = stringify(
      evaluateUntilDone(
        `(quasiquote (list (unquote (+ 1 2)) 4 (unquote-splicing (cons 5 (cons 6 '())))))`
      )
    )
    const expected = '(list 3 4 5 6)'
    expect(actual).toEqual(expected)
  })
})
