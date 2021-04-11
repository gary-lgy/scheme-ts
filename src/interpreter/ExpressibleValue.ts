import { Context, Environment } from '../types'
import { ValueGenerator } from './interpreter'
import { CallSignature, NamedCallSignature } from './procedure'
import { SyntaxNode } from './SchemeSyntax'
import {
  makeEmptyList,
  SBool,
  SEmptyList,
  SNumber,
  SourceLocation,
  SString,
  SSymbol
} from './SExpression'

// An expressible value is a value that can be the result of an evaluation
export type ExpressibleValue = NonTailCallExpressibleValue | TailCall

export type NonTailCallExpressibleValue =
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
    | ((args: ExpressibleValue[], context: Context) => ExpressibleValue)
    | ((args: ExpressibleValue[], context: Context) => ValueGenerator)
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
  head: ExpressibleValue
  tail: ExpressibleValue
  loc?: SourceLocation
}

export const makePair = (
  head: ExpressibleValue,
  tail: ExpressibleValue,
  loc?: SourceLocation
): Pair => {
  const location =
    loc ?? ('loc' in head && head.loc ? head.loc : 'loc' in tail && tail.loc ? tail.loc : undefined)
  return { type: 'pair', head, tail, loc: location }
}

export const makeList = (values: ExpressibleValue[], loc?: SourceLocation): Pair | SEmptyList => {
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
  beforeDot: ExpressibleValue[],
  afterDot: ExpressibleValue,
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
  args: ExpressibleValue[]
  node: SyntaxNode
}
