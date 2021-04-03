import { SchemeExpression, SchemeIdentifier, SchemeSequence } from '../../lang/scheme'

export type SpecialForm =
  | DefineForm
  | SetBangForm
  | LambdaForm
  | IfForm
  | LetForm
  | LetStarForm
  | LetRecForm
  | QuoteForm
  | QuasiquoteForm
  | UnquoteForm
  | UnquoteSplicingForm

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
      parameters: SchemeIdentifier[]
      argumentPassingStyle: LambdaArgumentPassingStyle
      body: SchemeExpression[]
    }
)

export type SetBangForm = {
  tag: 'set!'
  name: SchemeIdentifier
  value: SchemeExpression
}

export type LambdaArgumentPassingStyle =
  | { style: 'fixed-args'; numParams: number }
  | { style: 'var-args'; minNumParams: number }
  | { style: 'rest-args' }

export type LambdaForm = {
  tag: 'lambda'
  parameters: SchemeIdentifier[]
  body: SchemeExpression[]
  argumentPassingStyle: LambdaArgumentPassingStyle
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
