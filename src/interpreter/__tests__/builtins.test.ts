import { BuiltinProcedureError, NotEnoughArguments } from '../../errors/errors'
import { evaluateUntilDone } from '../../testHelpers'
import { makeNumber } from '../runtime'

describe('arithmetic procedures', () => {
  describe('+', () => {
    test('no arguments', () => {
      const actual = evaluateUntilDone('(+)')
      expect(actual).toEqual(makeNumber(0))
    })

    test('one argument', () => {
      const actual = evaluateUntilDone('(+ 5)')
      expect(actual).toEqual(makeNumber(5))
    })

    test('two arguments', () => {
      const actual = evaluateUntilDone('(+ 5 1)')
      expect(actual).toEqual(makeNumber(6))
    })

    test('more than two arguments', () => {
      const actual = evaluateUntilDone('(+ 1 2 3 4 5)')
      expect(actual).toEqual(makeNumber(15))
    })
  })

  describe('*', () => {
    test('no arguments', () => {
      const actual = evaluateUntilDone('(*)')
      expect(actual).toEqual(makeNumber(1))
    })

    test('one argument', () => {
      const actual = evaluateUntilDone('(* 5)')
      expect(actual).toEqual(makeNumber(5))
    })

    test('two arguments', () => {
      const actual = evaluateUntilDone('(* 5 2)')
      expect(actual).toEqual(makeNumber(10))
    })
  })

  describe('-', () => {
    test('no arguments', () => {
      const actual = () => evaluateUntilDone('(-)')
      expect(actual).toThrow(NotEnoughArguments)
    })

    test('one argument', () => {
      const actual = evaluateUntilDone('(- 5)')
      expect(actual).toEqual(makeNumber(-5))
    })

    test('two arguments', () => {
      const actual = evaluateUntilDone('(- 5 1)')
      expect(actual).toEqual(makeNumber(4))
    })
  })

  describe('/', () => {
    test('no arguments', () => {
      const actual = () => evaluateUntilDone('(/)')
      expect(actual).toThrow(NotEnoughArguments)
    })

    test('one argument', () => {
      const actual = evaluateUntilDone('(/ 5)')
      expect(actual).toEqual(makeNumber(0.2))
    })

    test('two arguments', () => {
      const actual = evaluateUntilDone('(/ 5 2)')
      expect(actual).toEqual(makeNumber(2.5))
    })

    test('division by zero', () => {
      const getBuiltinProcedureErrorString = (operation: () => void) => {
        try {
          operation()
        } catch (error) {
          expect(error).toBeInstanceOf(BuiltinProcedureError)
          return error.explain()
        }
      }
      expect(getBuiltinProcedureErrorString(() => evaluateUntilDone('(/ 0)'))).toEqual(
        expect.stringMatching(/division by zero/)
      )
      expect(getBuiltinProcedureErrorString(() => evaluateUntilDone('(/ 10 2 0)'))).toEqual(
        expect.stringMatching(/division by zero/)
      )
    })
  })
})
