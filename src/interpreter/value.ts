import { Context, Environment } from '../types'
import { ValueGenerator } from './interpreter'
import { CallSignature, NamedCallSignature } from './procedure'
import {
  makeEmptyList,
  SBool,
  SEmptyList,
  SNumber,
  SourceLocation,
  SString,
  SSymbol
} from './sExpression'
import { SyntaxNode } from './syntax'

// An expressible value is a value that can be the result of an evaluation
export type Value = NonTailCallValue | TailCall

export type NonTailCallValue =
  | SNumber
  | SString
  | SSymbol
  | SBool
  | Pair
  | SEmptyList
  | Procedure
  | Macro

export type Procedure = {
  type: 'procedure'
} & (CompoundProcedure | BuiltInProcedure)

export type CompoundProcedure = {
  variant: 'CompoundProcedure'
  callSignature: NamedCallSignature
  body: SyntaxNode[]
  environment: Environment
  name: string
}

export type BuiltInProcedure = {
  variant: 'BuiltInProcedure'
  callSignature: CallSignature
  name: string
  body:
    | ((args: Value[], context: Context) => Value)
    | ((args: Value[], context: Context) => ValueGenerator)
}

export type Macro = {
  type: 'macro'
  name: string
  callSignature: NamedCallSignature
  body: SyntaxNode[]
  environment: Environment
}

export type Pair = {
  type: 'pair'
  head: Value
  tail: Value
  loc?: SourceLocation
}

export const makePair = (head: Value, tail: Value, loc?: SourceLocation): Pair => {
  const location =
    loc ?? ('loc' in head && head.loc ? head.loc : 'loc' in tail && tail.loc ? tail.loc : undefined)
  return { type: 'pair', head, tail, loc: location }
}

export const makeList = (values: Value[], loc?: SourceLocation): Pair | SEmptyList => {
  const sentinel: { tail: Pair | SEmptyList } = { tail: makeEmptyList() }
  let prev: typeof sentinel | Pair = sentinel
  for (const value of values) {
    const newTail = makePair(value, makeEmptyList())
    prev.tail = newTail
    prev = newTail
  }
  sentinel.tail.loc = loc
  return sentinel.tail
}

/** Create an improper list where the values before the dot are `beforeDot` and the value after the dot is `afterDot`. */
export const makeImproperList = (
  beforeDot: Value[],
  afterDot: Value,
  loc?: SourceLocation
): Pair => {
  const preList = makeList(beforeDot) as Pair
  let curr = preList
  while (curr.tail.type !== 'empty list') {
    curr = curr.tail as Pair
  }
  curr.tail = afterDot
  preList.loc = loc
  return preList
}

export type TailCall = {
  type: 'TailCall'
  procedure: Procedure
  args: Value[]
  node: SyntaxNode
}
