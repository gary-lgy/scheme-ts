import { SyntaxIdentifier, SyntaxList, SyntaxNode } from '../../lang/SchemeSyntax'
import { CompoundProcedureArgumentPassingStyle } from '../procedure'

export type SpecialForm =
  | DefineForm
  | SetBangForm
  | LambdaForm
  | IfForm
  | LetForm
  | LetStarForm
  | LetRecForm
  | CondForm
  | BeginForm
  | QuoteForm
  | QuasiquoteForm
  | UnquoteForm
  | UnquoteSplicingForm
  | AndForm
  | OrForm

export type DefineForm = {
  tag: 'define'
} & (
  | {
      variant: 'basic'
      name: SyntaxIdentifier
      value: SyntaxNode
    }
  | {
      variant: 'procedure'
      name: SyntaxIdentifier
      argumentPassingStyle: CompoundProcedureArgumentPassingStyle
      body: SyntaxNode[]
    }
)

export type SetBangForm = {
  tag: 'set!'
  name: SyntaxIdentifier
  value: SyntaxNode
}

export type LambdaForm = {
  tag: 'lambda'
  body: SyntaxNode[]
  argumentPassingStyle: CompoundProcedureArgumentPassingStyle
}

export type IfForm = {
  tag: 'if'
  test: SyntaxNode
  consequent: SyntaxNode
  alternative?: SyntaxNode
}

export type LetBinding = {
  name: SyntaxIdentifier
  value: SyntaxNode
}

export type LetForm = {
  tag: 'let'
  bindings: LetBinding[]
  body: SyntaxNode[]
}

export type LetStarForm = {
  tag: 'let*'
  bindings: LetBinding[]
  body: SyntaxNode[]
}

export type LetRecForm = {
  tag: 'letrec'
  bindings: LetBinding[]
  body: SyntaxNode[]
}

export type CondClause = { node: SyntaxList } & (
  | { type: 'basic'; test: SyntaxNode; body: SyntaxNode[] }
  | { type: 'procedure'; test: SyntaxNode; body: SyntaxNode }
)
export type CondElseClause = { type: 'else'; node: SyntaxList; body: SyntaxNode[] }

export type CondForm = {
  tag: 'cond'
  clauses: CondClause[]
  elseClause?: CondElseClause
}

export type BeginForm = {
  tag: 'begin'
  body: SyntaxNode[]
}

export type QuoteForm = {
  tag: 'quote'
  expression: SyntaxNode
}

export type QuasiquoteForm = {
  tag: 'quasiquote'
  expression: SyntaxNode
}

export type UnquoteForm = {
  tag: 'unquote'
  expression: SyntaxNode
}

export type UnquoteSplicingForm = {
  tag: 'unquote-splicing'
  expression: SyntaxNode
}

export type AndForm = {
  tag: 'and'
  arguments: SyntaxNode[]
}

export type OrForm = {
  tag: 'or'
  arguments: SyntaxNode[]
}
