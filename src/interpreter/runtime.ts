import { SchemeExpression, SchemeSequence } from '../lang/scheme'
import { Environment } from '../types'

// An expressible value is a value that can be the result of an evaluation
export type ExpressibleValue = EVNumber | EVString | EVBool | EVProcedure | EVList

export type EVNumber = {
  type: 'EVNumber'
  value: number
}

export type EVString = {
  type: 'EVString'
  value: string
}

export type EVBool = {
  type: 'EVBool'
  value: boolean
}

export type EVProcedure = {
  type: 'EVProcedure'
  parameters: string[]
  body: SchemeSequence
  environment: Environment
} & LambdaArgumentStyle

export type EVList = {
  type: 'EVList'
  value: ExpressibleValue[]
}

// Special syntax forms

export type SpecialForm = DefineForm | SetForm | LambdaForm

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

export type LambdaArgumentStyle =
  | { style: 'fixed-args'; numParams: number }
  | { style: 'var-args' | 'rest-args' }

export type LambdaForm = {
  tag: 'lambda'
  parameters: string[]
  body: SchemeSequence
} & LambdaArgumentStyle

// Runtime data structures

export interface FrameBinding {
  value: ExpressibleValue
}

export class Frame {
  private bindings: Map<string, FrameBinding> = new Map()

  get(name: string): FrameBinding | undefined {
    return this.bindings.get(name)
  }

  set(name: string, newBinding: FrameBinding) {
    this.bindings.set(name, newBinding)
  }
}

export const EmptyList: EVList = {
  type: 'EVList',
  value: []
}
