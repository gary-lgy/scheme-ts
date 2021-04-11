import { areValuesEqual } from './interpreter/BuiltIns/Equal'
import { ExpressibleValue } from './interpreter/ExpressibleValue'

expect.extend({
  toHaveMatchingValue(received: ExpressibleValue, expected: ExpressibleValue) {
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
      toHaveMatchingValue(expected: ExpressibleValue): R
    }
  }
}
