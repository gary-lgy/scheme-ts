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
  | 'StringLiteral'
  | 'NumberLiteral'
  | 'BoolLiteral'
  | 'Identifier'

/**
 * Represents a language construct in Scheme.
 */
export interface SchemeExpression {
  type: SchemeExpressionType
  loc: SourceLocation
}

export interface SchemeProgram extends SchemeExpression {
  type: 'Program'
  body: SchemeSequence
}

export interface SchemeSequence extends SchemeExpression {
  type: 'Sequence'
  expressions: SchemeExpression[]
}

export interface SchemeList extends SchemeExpression {
  type: 'List'
  elements: SchemeExpression[]
}

export interface SchemeStringLiteral extends SchemeExpression {
  type: 'StringLiteral'
  value: string
}

export interface SchemeNumberLiteral extends SchemeExpression {
  type: 'NumberLiteral'
  value: number
}

export interface SchemeBoolLiteral extends SchemeExpression {
  type: 'BoolLiteral'
  value: boolean
}

export interface SchemeIdentifier extends SchemeExpression {
  type: 'Identifier'
  name: string
}
