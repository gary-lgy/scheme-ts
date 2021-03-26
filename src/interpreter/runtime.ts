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

export type EVString = {
  type: 'EVString'
  value: string
}

export type EVSymbol = {
  type: 'EVSymbol'
  value: string
}

export type EVBool = {
  type: 'EVBool'
  value: boolean
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

export type EVPair = {
  type: 'EVPair'
  head: ExpressibleValue
  tail: ExpressibleValue
}

// Special syntax forms

export type SpecialForm = DefineForm | SetForm | LambdaForm | ConditionalForm

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
