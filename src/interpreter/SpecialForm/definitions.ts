import { SchemeExpression, SchemeIdentifier, SchemeList, SchemeSequence } from '../../lang/scheme'
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
      name: SchemeIdentifier
      value: SchemeExpression
    }
  | {
      variant: 'procedure'
      name: SchemeIdentifier
      argumentPassingStyle: CompoundProcedureArgumentPassingStyle
      body: SchemeExpression[]
    }
)

export type SetBangForm = {
  tag: 'set!'
  name: SchemeIdentifier
  value: SchemeExpression
}

export type LambdaForm = {
  tag: 'lambda'
  body: SchemeExpression[]
  argumentPassingStyle: CompoundProcedureArgumentPassingStyle
}

export type IfForm = {
  tag: 'if'
  test: SchemeExpression
  consequent: SchemeExpression
  alternative?: SchemeExpression
}

export type LetBinding = {
  name: SchemeIdentifier
  value: SchemeExpression
}

export type LetForm = {
  tag: 'let'
  bindings: LetBinding[]
  body: SchemeSequence
}

export type LetStarForm = {
  tag: 'let*'
  bindings: LetBinding[]
  body: SchemeSequence
}

export type LetRecForm = {
  tag: 'letrec'
  bindings: LetBinding[]
  body: SchemeSequence
}

export type CondClause = { node: SchemeList } & (
  | { type: 'basic'; test: SchemeExpression; body: SchemeExpression[] }
  | { type: 'procedure'; test: SchemeExpression; body: SchemeExpression }
)
export type CondElseClause = { type: 'else'; node: SchemeList; body: SchemeExpression[] }

export type CondForm = {
  tag: 'cond'
  clauses: CondClause[]
  elseClause?: CondElseClause
}

export type BeginForm = {
  tag: 'begin'
  body: SchemeSequence
}

export type QuoteForm = {
  tag: 'quote'
  expression: SchemeExpression
}

export type QuasiquoteForm = {
  tag: 'quasiquote'
  expression: SchemeExpression
}

export type UnquoteForm = {
  tag: 'unquote'
  expression: SchemeExpression
}

export type UnquoteSplicingForm = {
  tag: 'unquote-splicing'
  expression: SchemeExpression
}

export type AndForm = {
  tag: 'and'
  arguments: SchemeExpression[]
}

export type OrForm = {
  tag: 'or'
  arguments: SchemeExpression[]
}
