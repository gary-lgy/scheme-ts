// Source location

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

// Syntax tree

export type SchemeExpressionType =
  | 'Program'
  | 'Sequence'
  | 'List'
  | 'Quote'
  | 'Quasiquote'
  | 'Unquote'
  | 'UnquoteSplicing'
  | 'StringLiteral'
  | 'NumberLiteral'
  | 'BoolLiteral'
  | 'Identifier'

/**
 * Represents a language construct in Scheme.
 */
export type SchemeExpression =
  | SchemeProgram
  | SchemeSequence
  | SchemeList
  | SchemeQuote
  | SchemeQuasiquote
  | SchemeUnquote
  | SchemeUnquoteSplicing
  | SchemeStringLiteral
  | SchemeNumberLiteral
  | SchemeBoolLiteral
  | SchemeIdentifier

export interface SchemeProgram {
  type: 'Program'
  body: SchemeSequence
  loc: SourceLocation
}

export interface SchemeSequence {
  type: 'Sequence'
  expressions: SchemeExpression[]
  loc: SourceLocation
}

export interface SchemeList {
  type: 'List'
  elements: SchemeExpression[]
  loc: SourceLocation
}

export interface SchemeQuote {
  type: 'Quote'
  expression: SchemeExpression
  loc: SourceLocation
}

export interface SchemeQuasiquote {
  type: 'Quasiquote'
  expression: SchemeExpression
  loc: SourceLocation
}

export interface SchemeUnquote {
  type: 'Unquote'
  expression: SchemeExpression
  loc: SourceLocation
}

export interface SchemeUnquoteSplicing {
  type: 'UnquoteSplicing'
  expression: SchemeExpression
  loc: SourceLocation
}

export interface SchemeStringLiteral {
  type: 'StringLiteral'
  value: string
  loc: SourceLocation
}

export interface SchemeNumberLiteral {
  type: 'NumberLiteral'
  value: number
  loc: SourceLocation
}

export interface SchemeBoolLiteral {
  type: 'BoolLiteral'
  value: boolean
  loc: SourceLocation
}

export interface SchemeIdentifier {
  type: 'Identifier'
  name: string
  loc: SourceLocation
}
