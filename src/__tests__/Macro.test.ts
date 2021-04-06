import { EVMacro, makeList, makeNumber, makeSymbol } from '../interpreter/ExpressibleValue'
import { FixedArgsWithParameterNames, VarArgsWithParameterNames } from '../interpreter/procedure'
import { evaluateUntilDone } from '../testHelpers'

describe('defitition', () => {
  test('fixed-args', () => {
    const macro: EVMacro = evaluateUntilDone(`
  (defmacro swap! (a b)
    \`(let ((temp ,a))
          (set! ,a ,b)
          (set! ,b temp)))
  swap!
  `) as EVMacro
    expect(macro.type).toEqual('EVMacro')
    expect(macro.name).toEqual('swap!')
    expect(macro.parameterPasssingStyle.style).toEqual('fixed-args')
    const style = macro.parameterPasssingStyle as FixedArgsWithParameterNames
    expect(style.numParams).toEqual(2)
    expect(style.parameters.map(param => param.name)).toEqual(['a', 'b'])
  })

  describe('var-args', () => {
    test('with compulsory args', () => {
      const macro: EVMacro = evaluateUntilDone(`
      (defmacro when (test first-body . rest-body)
          \`(if ,test (begin ,first-body ,@rest-body)))
      when
  `) as EVMacro
      expect(macro.type).toEqual('EVMacro')
      expect(macro.name).toEqual('when')
      expect(macro.parameterPasssingStyle.style).toEqual('var-args')
      const style = macro.parameterPasssingStyle as VarArgsWithParameterNames
      expect(style.numCompulsoryParameters).toEqual(2)
      expect(style.compulsoryParameters.map(param => param.name)).toEqual(['test', 'first-body'])
      expect(style.restParameters.name).toEqual('rest-body')
    })

    test('without compulsory args', () => {
      const macro: EVMacro = evaluateUntilDone(`
      (defmacro and args
        \`(cond
            ((null? args) #t)
            ((null? (cdr args)) (car args))
            ((car args) (and (cdr args)))
            (else #f)))
      and
  `) as EVMacro
      expect(macro.type).toEqual('EVMacro')
      expect(macro.name).toEqual('and')
      expect(macro.parameterPasssingStyle.style).toEqual('var-args')
      const style = macro.parameterPasssingStyle as VarArgsWithParameterNames
      expect(style.numCompulsoryParameters).toEqual(0)
      expect(style.compulsoryParameters.map(param => param.name)).toEqual([])
      expect(style.restParameters.name).toEqual('args')
    })
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
      expect(result).toEqual(makeList(makeNumber(20), makeNumber(10)))
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
      expect(result).toEqual(makeList(makeNumber(10), makeNumber(20)))
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
      expect(result).toEqual(makeNumber(42))
    })
  })

  // TODO: gensym
})

describe('macroexpand', () => {
  test('expands correctly', () => {
    const result = evaluateUntilDone(`
      (defmacro when (test first-body . rest-body)
          \`(if ,test (begin ,first-body ,@rest-body)))
      (macroexpand '(when (< 0 1) (display 1) (display 2) (display 3)))
      `)
    expect(result).toEqual(
      makeList(
        makeSymbol('if'),
        makeList(makeSymbol('<'), makeNumber(0), makeNumber(1)),
        makeList(
          makeSymbol('begin'),
          makeList(makeSymbol('display'), makeNumber(1)),
          makeList(makeSymbol('display'), makeNumber(2)),
          makeList(makeSymbol('display'), makeNumber(3))
        )
      )
    )
  })
})
