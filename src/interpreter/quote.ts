import * as errors from '../errors/errors'
import {
  SchemeBoolLiteral,
  SchemeExpression,
  SchemeIdentifier,
  SchemeNumberLiteral,
  SchemeStringLiteral
} from '../lang/scheme'
import { Context } from '../types'
import { List, tryConvertToList } from '../utils/listHelpers'
import { evaluate, ValueGenerator } from './interpreter'
import { ExpressibleValue } from './runtime'
import { handleRuntimeError, listOfValues } from './util'

const quoteLiteral = (
  literal: SchemeBoolLiteral | SchemeNumberLiteral | SchemeStringLiteral | SchemeIdentifier
): ExpressibleValue => {
  switch (literal.type) {
    case 'BoolLiteral':
      return {
        type: 'EVBool',
        value: literal.value
      }
    case 'NumberLiteral':
      return {
        type: 'EVNumber',
        value: literal.value
      }
    case 'StringLiteral':
      return {
        type: 'EVString',
        value: literal.value
      }
    case 'Identifier':
      return {
        type: 'EVSymbol',
        value: literal.name
      }
  }
}

export const quoteExpression = (
  expression: SchemeExpression,
  context: Context
): ExpressibleValue => {
  switch (expression.type) {
    case 'BoolLiteral':
    case 'NumberLiteral':
    case 'StringLiteral':
    case 'Identifier':
      return quoteLiteral(expression)
    case 'List':
      return listOfValues(...expression.elements.map(elem => quoteExpression(elem, context)))
    case 'Quote':
      return listOfValues(
        { type: 'EVSymbol', value: 'quote' },
        quoteExpression(expression.expression, context)
      )
    case 'Quasiquote':
      return listOfValues(
        { type: 'EVSymbol', value: 'quosiquote' },
        quoteExpression(expression.expression, context)
      )
    case 'Unquote':
      return listOfValues(
        { type: 'EVSymbol', value: 'unquote' },
        quoteExpression(expression.expression, context)
      )
    case 'UnquoteSplicing':
      return listOfValues(
        { type: 'EVSymbol', value: 'unquote-splicing' },
        quoteExpression(expression.expression, context)
      )
    case 'Program':
    case 'Sequence':
      return handleRuntimeError(
        context,
        new errors.UnexpectedQuotationError(context.runtime.nodes[0])
      )
  }
}

// Quasiquote an expression in list context.
// In a list context, unquote-splicing is allowed.
// Therefore, this method returns an array instead of a single value.
function* quasiquoteExpressionInListContext(
  expression: SchemeExpression,
  context: Context,
  quoteLevel: number,
  unquoteLevel: number
): Generator<Context, ExpressibleValue[]> {
  if (expression.type !== 'UnquoteSplicing') {
    return [yield* quasiquoteExpression(expression, context, quoteLevel, unquoteLevel)]
  }

  if (quoteLevel === unquoteLevel) {
    const unquoted = yield* evaluate(expression.expression, context)
    if (unquoted.type === 'EVEmptyList') {
      return []
    }

    let list: List | null
    if (unquoted.type !== 'EVPair' || !(list = tryConvertToList(unquoted))) {
      return handleRuntimeError(
        context,
        new errors.UnquoteSplicingEvaluatedToNonList(unquoted, expression)
      )
    }

    return list.map(element => element.value)
  } else if (quoteLevel > unquoteLevel) {
    return [
      listOfValues(
        { type: 'EVSymbol', value: 'unquote-splicing' },
        ...(yield* quasiquoteExpressionInListContext(
          expression.expression,
          context,
          quoteLevel,
          unquoteLevel + 1
        ))
      )
    ]
  } else {
    return handleRuntimeError(
      context,
      new errors.UnreachableCodeReached('invalid quasiquotation depth')
    )
  }
}

export function* quasiquoteExpression(
  expression: SchemeExpression,
  context: Context,
  quoteLevel: number,
  unquoteLevel: number
): ValueGenerator {
  switch (expression.type) {
    case 'BoolLiteral':
    case 'NumberLiteral':
    case 'StringLiteral':
    case 'Identifier':
      return quoteLiteral(expression)
    case 'List': {
      const quoted: ExpressibleValue[] = []
      for (const element of expression.elements) {
        // Spread each result
        quoted.push(
          ...(yield* quasiquoteExpressionInListContext(element, context, quoteLevel, unquoteLevel))
        )
      }
      return listOfValues(...quoted)
    }
    case 'Quote': {
      return listOfValues(
        { type: 'EVSymbol', value: 'quote' },
        ...(yield* quasiquoteExpressionInListContext(
          expression.expression,
          context,
          quoteLevel,
          unquoteLevel
        ))
      )
    }
    case 'Quasiquote':
      return listOfValues(
        { type: 'EVSymbol', value: 'quosiquote' },
        ...(yield* quasiquoteExpressionInListContext(
          expression.expression,
          context,
          quoteLevel + 1,
          unquoteLevel
        ))
      )
    case 'Unquote': {
      if (quoteLevel === unquoteLevel) {
        return yield* evaluate(expression.expression, context)
      } else if (quoteLevel > unquoteLevel) {
        return listOfValues(
          { type: 'EVSymbol', value: 'unquote' },
          ...(yield* quasiquoteExpressionInListContext(
            expression.expression,
            context,
            quoteLevel,
            unquoteLevel + 1
          ))
        )
      } else {
        return handleRuntimeError(
          context,
          new errors.UnreachableCodeReached('invalid quasiquotation depth')
        )
      }
    }
    case 'UnquoteSplicing': {
      return handleRuntimeError(context, new errors.UnquoteSplicingInNonListContext(expression))
    }
    case 'Program':
    case 'Sequence':
      return handleRuntimeError(
        context,
        new errors.UnexpectedQuotationError(context.runtime.nodes[0])
      )
  }
}
