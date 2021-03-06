import { areValuesEqual } from './builtIns/equal'
import { Value } from './interpreter/value'

expect.extend({
  toHaveMatchingValue(received: Value, expected: Value) {
    const pass = areValuesEqual(received, expected)
    if (pass) {
      return {
        message: () => `expected values not to be equal`,
        pass: true
      }
    } else {
      return {
        message: () => `expected values to be equal`,
        pass: false
      }
    }
  }
})

declare global {
  // eslint-disable-next-line @typescript-eslint/no-namespace
  namespace jest {
    interface Matchers<R> {
      /** Expect an expressible value to be equal to another */
      toHaveMatchingValue(expected: Value): R
    }
  }
}
