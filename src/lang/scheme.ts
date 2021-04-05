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

export type SchemeExpressionType = SchemeExpression['type']

/**
 * Represents a syntax fragment in Scheme.
 */
export type SchemeExpression =
  | SchemeList
  | SchemeDottedList
  | SchemeStringLiteral
  | SchemeNumberLiteral
  | SchemeBoolLiteral
  | SchemeIdentifier

export interface SchemeProgram {
  body: SchemeExpression[]
}

export interface SchemeList {
  type: 'List'
  elements: SchemeExpression[]
  loc: SourceLocation
}

export interface SchemeDottedList {
  type: 'DottedList'
  pre: SchemeExpression[]
  post: SchemeExpression
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
