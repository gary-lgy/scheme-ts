import { ExpressibleValue } from '../interpreter/ExpressibleValue'
import { Value } from '../types'
import { stringify } from '../utils/stringify'

/**
 * A function that displays to console.log by default (for a REPL).
 *
 * @param value the value to be represented and displayed.
 * @param externalContext a property of Context that can hold
 *   any information required for external use (optional).
 */
export function rawDisplay(value: Value, str: string, externalContext: any) {
  // tslint:disable-next-line:no-console
  console.log((str === undefined ? '' : str + ' ') + value.toString())
  return value
}

export function error_message(str: string, values: ExpressibleValue[]): never {
  const output = str + ' ' + values.map(value => stringify(value)).join(' ')
  throw new Error(output)
}
