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

export type SyntaxNodeType = SyntaxNode['type']

/**
 * Represents a syntax fragment in Scheme.
 */
export type SyntaxNode =
  | SyntaxList
  | SyntaxDottedList
  | SyntaxString
  | SyntaxNumber
  | SyntaxBool
  | SyntaxIdentifier

export interface SchemeProgram {
  body: SyntaxNode[]
}

export interface SyntaxList {
  type: 'List'
  elements: SyntaxNode[]
  loc: SourceLocation
}

export interface SyntaxDottedList {
  type: 'DottedList'
  pre: SyntaxNode[]
  post: SyntaxNode
  loc: SourceLocation
}

export interface SyntaxString {
  type: 'StringLiteral'
  value: string
  loc: SourceLocation
}

export interface SyntaxNumber {
  type: 'NumberLiteral'
  value: number
  loc: SourceLocation
}

export interface SyntaxBool {
  type: 'BoolLiteral'
  value: boolean
  loc: SourceLocation
}

export interface SyntaxIdentifier {
  type: 'Identifier'
  name: string
  // Whether the identifier came from the source code or macro expansion.
  // Used to disallow the user from using certain identifiers so that symbols from gensym cannot collide with identifiers from the source code.
  isFromSource: boolean
  loc: SourceLocation
}
