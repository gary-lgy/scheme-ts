import { ExpressibleValue, Macro, makeList, Pair } from '../interpreter/ExpressibleValue'
import { FixedArgsWithParameterNames, VarArgsWithParameterNames } from '../interpreter/procedure'
import { makeNumber, makeSymbol, SSymbol } from '../interpreter/SExpression'
import { prepareContext, runUntilDone } from '../testHelpers'
import { flattenPairToList, List } from '../utils/listHelpers'

function evaluateUntilDone(code: string): ExpressibleValue {
  const context = prepareContext('macro')
  return runUntilDone(code, context).value
}

describe('defitition', () => {
  test('fixed-args', () => {
    const macro: Macro = evaluateUntilDone(`
  (defmacro swap! (a b)
    \`(let ((temp ,a))
          (set! ,a ,b)
          (set! ,b temp)))
  swap!
  `) as Macro
    expect(macro.type).toEqual('macro')
    expect(macro.name).toEqual('swap!')
    expect(macro.callSignature.style).toEqual('fixed-args')
    const style = macro.callSignature as FixedArgsWithParameterNames
    expect(style.numParams).toEqual(2)
    expect(style.parameters.map(param => param.value)).toEqual(['a', 'b'])
  })

  describe('var-args', () => {
    test('with compulsory args', () => {
      const macro: Macro = evaluateUntilDone(`
      (defmacro when (test first-body . rest-body)
          \`(if ,test (begin ,first-body ,@rest-body)))
      when
  `) as Macro
      expect(macro.type).toEqual('macro')
      expect(macro.name).toEqual('when')
      expect(macro.callSignature.style).toEqual('var-args')
      const style = macro.callSignature as VarArgsWithParameterNames
      expect(style.numCompulsoryParameters).toEqual(2)
      expect(style.compulsoryParameters.map(param => param.value)).toEqual(['test', 'first-body'])
      expect(style.restParameters.value).toEqual('rest-body')
    })

    test('without compulsory args', () => {
      const macro: Macro = evaluateUntilDone(`
      (defmacro and args
        \`(cond
            ((null? args) #t)
            ((null? (cdr args)) (car args))
            ((car args) (and (cdr args)))
            (else #f)))
      and
  `) as Macro
      expect(macro.type).toEqual('macro')
      expect(macro.name).toEqual('and')
      expect(macro.callSignature.style).toEqual('var-args')
      const style = macro.callSignature as VarArgsWithParameterNames
      expect(style.numCompulsoryParameters).toEqual(0)
      expect(style.compulsoryParameters.map(param => param.value)).toEqual([])
      expect(style.restParameters.value).toEqual('args')
    })
  })
})

describe('gensym', () => {
  test('returns a synthetic identifier', () => {
    expect((evaluateUntilDone('(gensym)') as any).isFromSource).toBe(false)
  })

  test('returns a different identifier for each invocation', () => {
    const result = evaluateUntilDone(
      '(list (gensym) (gensym) (gensym) (gensym) (gensym) (gensym) (gensym) (gensym))'
    ) as Pair

    const symbols = (flattenPairToList(result).value as List).map(
      element => element.value
    ) as SSymbol[]

    const symbolsSet = new Set(symbols)
    expect(symbolsSet.size).toEqual(symbols.length)
  })
})

describe('use', () => {
  describe('no name conflicts', () => {
    test('should work correctly', () => {
      const result = evaluateUntilDone(`
        (define x 10)
        (define y 20)
        (defmacro swap! (a b)
          \`(let ((temp ,a))
                (set! ,a ,b)
                (set! ,b temp)))
        (swap! x y)
        (list x y)
  `)
      expect(result).toHaveMatchingValue(makeList([makeNumber(20), makeNumber(10)]))
    })
  })

  describe('with name conflicts', () => {
    test('should not work correctly', () => {
      const result = evaluateUntilDone(`
        (define x 10)
        (define y 20)
        (defmacro swap! (a b)
          \`(let ((x ,a))
                (set! ,a ,b)
                (set! ,b x)))
        (swap! x y)
        (list x y)
      `)
      expect(result).toHaveMatchingValue(makeList([makeNumber(10), makeNumber(20)]))
    })
  })

  describe('macro body has references to free variables', () => {
    test('should refer to the environment in which the macro was defined', () => {
      const result = evaluateUntilDone(`
        (let ((the-answer 42))
          (defmacro find-the-answer (question)
            \`(set! ,question ,the-answer))
          (let ((the-answer "unknown"))
            (define my-answer "???")
            (find-the-answer my-answer)
            my-answer))
        `)
      expect(result).toHaveMatchingValue(makeNumber(42))
    })
  })

  describe('preventing name conflicts with gensym', () => {
    test('should work correctly', () => {
      const result = evaluateUntilDone(`
        (define x 10)
        (define y 20)

        (defmacro swap! (a b)
          (let ((x (gensym)))
            \`(let ((,x ,a))
                  (set! ,a ,b)
                  (set! ,b ,x))))

        (swap! x y)
        (list x y)
      `)
      expect(result).toHaveMatchingValue(makeList([makeNumber(20), makeNumber(10)]))
    })

    describe('passsing synthetic symbol to a nested macro', () => {
      test('should work correctly', () => {
        const result = evaluateUntilDone(`
        (defmacro let-and-get-10 (x)
          \`(let ((,x 10)) ,x))

        (defmacro test ()
          (let ((temp (gensym)))
            \`(let ((,temp 20))
                (let-and-get-10 ,temp))))
        (test)
      `)
        expect(result).toHaveMatchingValue(makeNumber(10))
      })
    })
  })
})

describe('macroexpand', () => {
  test('expands correctly', () => {
    const result = evaluateUntilDone(`
      (defmacro when (test first-body . rest-body)
          \`(if ,test (begin ,first-body ,@rest-body)))
      (macroexpand '(when (< 0 1) (display 1) (display 2) (display 3)))
      `)
    expect(result).toHaveMatchingValue(
      makeList([
        makeSymbol('if', true),
        makeList([makeSymbol('<', true), makeNumber(0), makeNumber(1)]),
        makeList([
          makeSymbol('begin', true),
          makeList([makeSymbol('display', true), makeNumber(1)]),
          makeList([makeSymbol('display', true), makeNumber(2)]),
          makeList([makeSymbol('display', true), makeNumber(3)])
        ])
      ])
    )
  })
})
