/**
 * A function that displays to console.log by default (for a REPL).
 *
 * @param value the value to be represented and displayed.
 * @param externalContext a property of Context that can hold
 *   any information required for external use (optional).
 */
export function rawDisplay(str: string, prepend: string) {
  // tslint:disable-next-line:no-console
  console.log((prepend === undefined ? '' : prepend + ' ') + str)
}
