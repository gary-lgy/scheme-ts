import { SyntaxNode } from '../lang/SchemeSyntax'
import { Context, Environment } from '../types'
import { ValueGenerator } from './interpreter'
import { NamedParameterPassingStyle, ParameterPassingStyle } from './procedure'

// An expressible value is a value that can be the result of an evaluation
export type ExpressibleValue = NonTailCallExpressibleValue | TailCall

export type NonTailCallExpressibleValue =
  | EVNumber
  | EVString
  | EVSymbol
  | EVBool
  | EVProcedure
  | EVMacro
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
} & (EVCompoundProcedure | EVBuiltInProcedure)

export type EVCompoundProcedure = {
  variant: 'CompoundProcedure'
  parameterPassingStyle: NamedParameterPassingStyle
  body: SyntaxNode[]
  environment: Environment
  name: string
}

export type EVBuiltInProcedure = {
  variant: 'BuiltInProcedure'
  parameterPassingStyle: ParameterPassingStyle
  name: string
  body:
    | ((args: ExpressibleValue[], context: Context) => ExpressibleValue)
    | ((args: ExpressibleValue[], context: Context) => ValueGenerator)
}

export type EVMacro = {
  type: 'EVMacro'
  name: string
  parameterPasssingStyle: NamedParameterPassingStyle
  body: SyntaxNode[]
  environment: Environment
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

/** Create an improper list where the values before the dot are `beforeDot` and the value after the dot is `afterDot`. */
export const makeImproperList = (
  beforeDot: ExpressibleValue[],
  afterDot: ExpressibleValue
): EVPair => {
  const preList = makeList(...beforeDot) as EVPair
  let curr = preList
  while (curr.tail.type !== 'EVEmptyList') {
    curr = curr.tail as EVPair
  }
  curr.tail = afterDot
  return preList
}

export type TailCall = {
  type: 'TailCall'
  procedure: EVProcedure
  args: ExpressibleValue[]
  node: SyntaxNode
}
