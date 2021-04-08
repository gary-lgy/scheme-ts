import { Value } from '../types'

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
