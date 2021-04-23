export interface SourceLocation {
  start: Position
  end: Position
}

export interface Position {
  /** >= 1 */
  line: number
  /** >= 0 */
  column: number
}

export interface SNumber {
  type: 'number'
  value: number
  loc?: SourceLocation
}

export const makeNumber = (value: number, loc?: SourceLocation): SNumber => {
  return { type: 'number', value, loc }
}

export interface SBool {
  type: 'boolean'
  value: boolean
  loc?: SourceLocation
}

export const makeBool = (value: boolean, loc?: SourceLocation): SBool => {
  return { type: 'boolean', value, loc }
}

export interface SString {
  type: 'string'
  value: string
  loc?: SourceLocation
}

export const makeString = (value: string, loc?: SourceLocation): SString => {
  return { type: 'string', value, loc }
}

export interface SSymbol {
  type: 'symbol'
  value: string
  // Whether the identifier came from the source code or macro expansion.
  // Used to disallow the user from using certain identifiers so that symbols from gensym cannot collide with identifiers from the source code.
  isFromSource: boolean
  loc?: SourceLocation
}

export const makeSymbol = (value: string, isFromSource: boolean, loc?: SourceLocation): SSymbol => {
  return { type: 'symbol', value, isFromSource, loc }
}

export interface SEmptyList {
  type: 'empty list'
  loc?: SourceLocation
}

export const makeEmptyList = (): SEmptyList => {
  return { type: 'empty list' }
}
