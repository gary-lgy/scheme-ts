import { SchemeExpression } from '../lang/scheme'
import { Environment } from '../types'
import {
  BuiltInProcedureArgumentPassingStyle,
  CompoundProcedureArgumentPassingStyle
} from './procedure'

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
} & (EVCompoundProcedure | EVBuiltInProcedure)

export type EVCompoundProcedure = {
  variant: 'CompoundProcedure'
  argumentPassingStyle: CompoundProcedureArgumentPassingStyle
  body: SchemeExpression[]
  environment: Environment
}

export type EVBuiltInProcedure = {
  variant: 'BuiltInProcedure'
  argumentPassingStyle: BuiltInProcedureArgumentPassingStyle
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
