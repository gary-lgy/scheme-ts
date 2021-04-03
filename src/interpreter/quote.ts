import * as errors from '../errors/errors'
import {
  SchemeBoolLiteral,
  SchemeExpression,
  SchemeIdentifier,
  SchemeList,
  SchemeNumberLiteral,
  SchemeStringLiteral
} from '../lang/scheme'
import { Context } from '../types'
import { List, tryConvertToList } from '../utils/listHelpers'
import { ExpressibleValue, makeList } from './ExpressibleValue'
import { evaluate, ValueGenerator } from './interpreter'
import { handleRuntimeError, isDefined } from './util'

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
      return makeList(...expression.elements.map(elem => quoteExpression(elem, context)))
    case 'Program':
    case 'Sequence':
      return handleRuntimeError(
        context,
        new errors.UnexpectedQuotationError(context.runtime.nodes[0])
      )
  }
}

// Handle (quasiquote expr), (unquote expr), and (unquote-splicing expr) specially, if they have not been redefined
function* handleSpecialQuotationForm(
  expression: SchemeList,
  context: Context,
  quoteLevel: number,
  unquoteLevel: number,
  isUnquoteSplicingAllowed: boolean
): Generator<Context, ExpressibleValue[] | null> {
  if (
    !(
      expression.elements.length > 1 &&
      expression.elements[0].type === 'Identifier' &&
      (expression.elements[0].name === 'quasiquote' ||
        expression.elements[0].name === 'unquote' ||
        expression.elements[0].name === 'unquote-splicing') &&
      !isDefined(context, expression.elements[0].name)
    )
  ) {
    return null
  }

  if (expression.elements.length !== 2) {
    return handleRuntimeError(
      context,
      new errors.QuoteSyntaxError(expression.elements[0].name, expression)
    )
  }

  const quoteType = expression.elements[0].name
  const subExpression = expression.elements[1]
  switch (quoteType) {
    case 'quasiquote': {
      return [
        makeList(
          { type: 'EVSymbol', value: 'quasiquote' },
          ...(yield* quasiquoteExpressionInner(
            subExpression,
            context,
            quoteLevel + 1,
            unquoteLevel,
            true
          ))
        )
      ]
    }
    case 'unquote': {
      if (quoteLevel === unquoteLevel) {
        return [yield* evaluate(subExpression, context)]
      } else if (quoteLevel > unquoteLevel) {
        return [
          makeList(
            { type: 'EVSymbol', value: 'unquote' },
            ...(yield* quasiquoteExpressionInner(
              subExpression,
              context,
              quoteLevel,
              unquoteLevel + 1,
              true
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
    case 'unquote-splicing': {
      if (!isUnquoteSplicingAllowed) {
        return handleRuntimeError(context, new errors.UnquoteSplicingInNonListContext(expression))
      }

      if (quoteLevel === unquoteLevel) {
        const unquoted = yield* evaluate(subExpression, context)
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
          makeList(
            { type: 'EVSymbol', value: 'unquote-splicing' },
            ...(yield* quasiquoteExpressionInner(
              subExpression,
              context,
              quoteLevel,
              unquoteLevel + 1,
              true
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
  }
}

export function* quasiquoteExpression(
  expression: SchemeExpression,
  context: Context
): ValueGenerator {
  const quoted = yield* quasiquoteExpressionInner(expression, context, 1, 1, false)
  if (quoted.length !== 1) {
    return handleRuntimeError(
      context,
      new errors.UnreachableCodeReached('top-level quasiquote should return a single value')
    )
  }
  return quoted[0]
}

function* quasiquoteExpressionInner(
  expression: SchemeExpression,
  context: Context,
  quoteLevel: number,
  unquoteLevel: number,
  isUnquoteSplicingAllowed: boolean
): Generator<Context, ExpressibleValue[]> {
  switch (expression.type) {
    case 'BoolLiteral':
    case 'NumberLiteral':
    case 'StringLiteral':
    case 'Identifier':
      return [quoteLiteral(expression)]
    case 'List': {
      // Handle special forms, i.e., (quasiquote expr), (unquote expr), and (unquote-splicing expr)
      const maybeHandledAsSpecialForm:
        | ExpressibleValue[]
        | null = yield* handleSpecialQuotationForm(
        expression,
        context,
        quoteLevel,
        unquoteLevel,
        isUnquoteSplicingAllowed
      )
      if (maybeHandledAsSpecialForm) {
        return maybeHandledAsSpecialForm
      }

      // Handle as a normal list
      const quoted: ExpressibleValue[] = []
      for (const element of expression.elements) {
        // Allow unquote-splicing and spread each result
        quoted.push(
          ...(yield* quasiquoteExpressionInner(element, context, quoteLevel, unquoteLevel, true))
        )
      }
      return [makeList(...quoted)]
    }
    case 'Program':
    case 'Sequence':
      return handleRuntimeError(
        context,
        new errors.UnexpectedQuotationError(context.runtime.nodes[0])
      )
  }
}
