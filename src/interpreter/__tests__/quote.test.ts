import {
  QuoteSyntaxError,
  UnquoteInWrongContext,
  UnquoteSplicingEvaluatedToNonList,
  UnquoteSplicingInNonListContext
} from '../../errors/errors'
import { evaluateUntilDone } from '../../testHelpers'
import { stringify } from '../../utils/stringify'
import { EVBool, EVNumber, EVString, EVSymbol } from '../runtime'
import { listOfValues } from '../util'

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
    const expected: EVNumber = {
      type: 'EVNumber',
      value: 1
    }
    expect(actual).toEqual(expected)
  })

  test('quote string', () => {
    const actual = evaluateUntilDone('"hello world"')
    const expected: EVString = {
      type: 'EVString',
      value: 'hello world'
    }
    expect(actual).toEqual(expected)
  })

  test('quote bool', () => {
    const actual = evaluateUntilDone('#t')
    const expected: EVBool = {
      type: 'EVBool',
      value: true
    }
    expect(actual).toEqual(expected)
  })

  test('quote identifier', () => {
    const actual = evaluateUntilDone("'my-pair")
    const expected: EVSymbol = {
      type: 'EVSymbol',
      value: 'my-pair'
    }
    expect(actual).toEqual(expected)
  })

  test('quote quoted', () => {
    const actual = evaluateUntilDone(`''a`)
    const expected = listOfValues(
      { type: 'EVSymbol', value: 'quote' },
      { type: 'EVSymbol', value: 'a' }
    )
    expect(actual).toEqual(expected)
  })

  test('quote simple list', () => {
    const actual = evaluateUntilDone(`'(1 2 "my-string" #f my-symbol)`)
    const expected = listOfValues(
      { type: 'EVNumber', value: 1 },
      { type: 'EVNumber', value: 2 },
      { type: 'EVString', value: 'my-string' },
      { type: 'EVBool', value: false },
      { type: 'EVSymbol', value: 'my-symbol' }
    )
    expect(actual).toEqual(expected)
  })

  test('quote empty list', () => {
    const actual = evaluateUntilDone(`'()`)
    const expected = listOfValues()
    expect(actual).toEqual(expected)
  })

  test('quote nested list', () => {
    const actual = evaluateUntilDone(`'(1 2 '(3 4 '(5 6)))`)
    const expected = listOfValues(
      { type: 'EVNumber', value: 1 },
      { type: 'EVNumber', value: 2 },
      listOfValues(
        { type: 'EVSymbol', value: 'quote' },
        listOfValues(
          { type: 'EVNumber', value: 3 },
          { type: 'EVNumber', value: 4 },
          listOfValues(
            { type: 'EVSymbol', value: 'quote' },
            listOfValues({ type: 'EVNumber', value: 5 }, { type: 'EVNumber', value: 6 })
          )
        )
      )
    )
    expect(actual).toEqual(expected)
  })

  test('quote using list special form', () => {
    const actual = evaluateUntilDone(`(quote (+ 1 2))`)
    const expected = listOfValues(
      { type: 'EVSymbol', value: '+' },
      { type: 'EVNumber', value: 1 },
      { type: 'EVNumber', value: 2 }
    )
    expect(actual).toEqual(expected)
  })

  test('quote with mixed shorthand and special form', () => {
    const actual = evaluateUntilDone(`'(quote (+ 1 2))`)
    const expected = listOfValues(
      { type: 'EVSymbol', value: 'quote' },
      listOfValues(
        { type: 'EVSymbol', value: '+' },
        { type: 'EVNumber', value: 1 },
        { type: 'EVNumber', value: 2 }
      )
    )
    expect(actual).toEqual(expected)
  })

  test('quote with quasiquote and unquote inside quoted expression', () => {
    const actual = evaluateUntilDone(`'(1 2 \`(3 4 ,(+ 1 2) ,@(list a b)))`)
    const expected = listOfValues(
      { type: 'EVNumber', value: 1 },
      { type: 'EVNumber', value: 2 },
      listOfValues(
        { type: 'EVSymbol', value: 'quasiquote' },
        listOfValues(
          { type: 'EVNumber', value: 3 },
          { type: 'EVNumber', value: 4 },
          listOfValues(
            { type: 'EVSymbol', value: 'unquote' },
            listOfValues(
              { type: 'EVSymbol', value: '+' },
              { type: 'EVNumber', value: 1 },
              { type: 'EVNumber', value: 2 }
            )
          ),
          listOfValues(
            { type: 'EVSymbol', value: 'unquote-splicing' },
            listOfValues(
              { type: 'EVSymbol', value: 'list' },
              { type: 'EVSymbol', value: 'a' },
              { type: 'EVSymbol', value: 'b' }
            )
          )
        )
      )
    )
    expect(actual).toEqual(expected)
  })

  test('quoted quasiquote and unquote', () => {
    const actual = stringify(evaluateUntilDone(`'(quasiquote (list (unquote (+ 1 2)) 4))`))
    const expected = '(quasiquote (list (unquote (+ 1 2)) 4))'
    expect(actual).toEqual(expected)
  })
})

describe('quasiquote', () => {
  test('no unquote', () => {
    const actual = evaluateUntilDone('`(1 2 "my-string" #f my-symbol)')
    const expected = listOfValues(
      { type: 'EVNumber', value: 1 },
      { type: 'EVNumber', value: 2 },
      { type: 'EVString', value: 'my-string' },
      { type: 'EVBool', value: false },
      { type: 'EVSymbol', value: 'my-symbol' }
    )
    expect(actual).toEqual(expected)
  })

  test('simple unquote', () => {
    const actual = evaluateUntilDone('`(list ,(+ 1 2) 4)')
    const expected = listOfValues(
      { type: 'EVSymbol', value: 'list' },
      { type: 'EVNumber', value: 3 },
      { type: 'EVNumber', value: 4 }
    )
    expect(actual).toEqual(expected)
  })

  test('unquote expanding identifier', () => {
    const actual = evaluateUntilDone("(define name 'a) `(list ,name ',name)")
    const expected = listOfValues(
      { type: 'EVSymbol', value: 'list' },
      { type: 'EVSymbol', value: 'a' },
      listOfValues({ type: 'EVSymbol', value: 'quote' }, { type: 'EVSymbol', value: 'a' })
    )
    expect(actual).toEqual(expected)
  })

  test('unquote-splicing', () => {
    const actual = evaluateUntilDone("`(a ,(+ 1 2) ,@(cons 3 (cons 4 '())) b)")
    const expected = listOfValues(
      { type: 'EVSymbol', value: 'a' },
      { type: 'EVNumber', value: 3 },
      { type: 'EVNumber', value: 3 },
      { type: 'EVNumber', value: 4 },
      { type: 'EVSymbol', value: 'b' }
    )
    expect(actual).toEqual(expected)
  })

  test('unquote & unquote-splicing', () => {
    const actual = evaluateUntilDone("`(( foo ,(- 10 3)) ,@(cdr '(c)) ,(car '(cons)))")
    const expected = listOfValues(
      listOfValues({ type: 'EVSymbol', value: 'foo' }, { type: 'EVNumber', value: 7 }),
      { type: 'EVSymbol', value: 'cons' }
    )
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
