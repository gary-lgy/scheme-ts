import { SchemeExpression, SchemeSequence } from '../lang/scheme'
import { Environment } from '../types'

// An expressible value is a value that can be the result of an evaluation
export type ExpressibleValue =
  | EVNumber
  | EVString
  | EVSymbol
  | EVBool
  | EVProcedure
  | EVPair
  | EVEmptyList

export type EVNumber = {
  type: 'EVNumber'
  value: number
}

export const makeNumber = (value: number): EVNumber => {
  return {
    type: 'EVNumber',
    value
  }
}

export type EVString = {
  type: 'EVString'
  value: string
}

export const makeString = (value: string): EVString => {
  return {
    type: 'EVString',
    value
  }
}

export type EVSymbol = {
  type: 'EVSymbol'
  value: string
}

export const makeSymbol = (value: string): EVSymbol => {
  return {
    type: 'EVSymbol',
    value
  }
}

export type EVBool = {
  type: 'EVBool'
  value: boolean
}

export const makeBool = (value: boolean): EVBool => {
  return {
    type: 'EVBool',
    value
  }
}

export type EVProcedure = {
  type: 'EVProcedure'
  argumentPassingStyle: LambdaArgumentPassingStyle
} & (EVCompoundProcedure | EVBuiltInProcedure)

export type EVCompoundProcedure = {
  variant: 'CompoundProcedure'
  parameters: string[]
  body: SchemeSequence
  environment: Environment
}

export type EVBuiltInProcedure = {
  variant: 'BuiltInProcedure'
  body: (args: ExpressibleValue[]) => ExpressibleValue
}

export type EVEmptyList = {
  type: 'EVEmptyList'
}

export const makeEmptyList = (): EVEmptyList => {
  return { type: 'EVEmptyList' }
}

export type EVPair = {
  type: 'EVPair'
  head: ExpressibleValue
  tail: ExpressibleValue
}

export const makePair = (head: ExpressibleValue, tail: ExpressibleValue): EVPair => {
  return { type: 'EVPair', head, tail }
}

export const makeList = (...values: ExpressibleValue[]): EVPair | EVEmptyList => {
  const sentinel: { tail: EVPair | EVEmptyList } = { tail: makeEmptyList() }
  let prev: typeof sentinel | EVPair = sentinel
  for (const value of values) {
    const newTail = makePair(value, makeEmptyList())
    prev.tail = newTail
    prev = newTail
  }
  return sentinel.tail
}

// Special syntax forms

export type SpecialForm =
  | DefineForm
  | SetForm
  | LambdaForm
  | ConditionalForm
  | QuoteSpecialForm
  | QuasiquoteSpecialForm
  | UnquoteSpecialForm
  | UnquoteSplicingSpecialForm

export type DefineForm = {
  tag: 'define'
  name: string
  value: SchemeExpression
}

export type SetForm = {
  tag: 'set!'
  name: string
  value: SchemeExpression
}

export type LambdaArgumentPassingStyle =
  | { style: 'fixed-args'; numParams: number }
  | { style: 'var-args'; minNumParams: number }
  | { style: 'rest-args' }

export type LambdaForm = {
  tag: 'lambda'
  parameters: string[]
  body: SchemeSequence
  argumentPassingStyle: LambdaArgumentPassingStyle
}

export type ConditionalForm = {
  tag: 'if'
  test: SchemeExpression
  consequent: SchemeExpression
  alternative?: SchemeExpression
}

export type QuoteSpecialForm = {
  tag: 'quote'
  expression: SchemeExpression
}

export type QuasiquoteSpecialForm = {
  tag: 'quasiquote'
  expression: SchemeExpression
}

export type UnquoteSpecialForm = {
  tag: 'unquote'
  expression: SchemeExpression
}

export type UnquoteSplicingSpecialForm = {
  tag: 'unquote-splicing'
  expression: SchemeExpression
}
