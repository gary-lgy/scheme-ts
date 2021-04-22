import { SBool, SNumber, SourceLocation, SString, SSymbol } from './sExpression'

export type SyntaxNodeType = SyntaxNode['type']

/**
 * Represents a syntax fragment in Scheme.
 */
export type SyntaxNode = SyntaxList | SyntaxDottedList | SString | SNumber | SBool | SSymbol

export interface SchemeProgram {
  body: SyntaxNode[]
}

export interface SyntaxList {
  type: 'list'
  elements: SyntaxNode[]
  loc: SourceLocation
}

export interface SyntaxDottedList {
  type: 'dotted list'
  pre: SyntaxNode[]
  post: SyntaxNode
  loc: SourceLocation
}
