import * as errors from '../errors/errors'
import { Context } from '../types'
import { flattenPairToList } from '../utils/listHelpers'
import { evaluate, ValueGenerator } from './interpreter'
import { makeSymbol, SBool, SNumber, SString, SSymbol } from './sExpression'
import { SyntaxList, SyntaxNode } from './syntax'
import { handleRuntimeError, isDefined } from './util'
import { makeImproperList, makeList, Value } from './value'

const quoteLiteral = (literal: SBool | SNumber | SString | SSymbol): Value => {
  return { ...literal }
}

export const quoteExpression = (expression: SyntaxNode, context: Context): Value => {
  switch (expression.type) {
    case 'boolean':
    case 'number':
    case 'string':
    case 'symbol':
      return quoteLiteral(expression)
    case 'list':
      return makeList(expression.elements.map(elem => quoteExpression(elem, context)))
    case 'dotted list':
      return makeImproperList(
        expression.pre.map(elem => quoteExpression(elem, context)),
        quoteExpression(expression.post, context)
      )
  }
}

// Handle (quasiquote expr), (unquote expr), and (unquote-splicing expr) specially, if they have not been redefined
function* handleSpecialQuotationForm(
  expression: SyntaxList,
  context: Context,
  quoteLevel: number,
  unquoteLevel: number,
  isUnquoteSplicingAllowed: boolean
): Generator<Context, Value[] | null> {
  if (
    !(
      expression.elements.length > 1 &&
      expression.elements[0].type === 'symbol' &&
      (expression.elements[0].value === 'quasiquote' ||
        expression.elements[0].value === 'unquote' ||
        expression.elements[0].value === 'unquote-splicing') &&
      !isDefined(context, expression.elements[0].value)
    )
  ) {
    return null
  }

  if (expression.elements.length !== 2) {
    return handleRuntimeError(
      context,
      new errors.QuoteSyntaxError(expression.elements[0].value, expression)
    )
  }

  const quoteType = expression.elements[0].value
  const subExpression = expression.elements[1]
  switch (quoteType) {
    case 'quasiquote': {
      return [
        makeList([
          makeSymbol(quoteType, true),
          ...(yield* quasiquoteExpressionInner(
            subExpression,
            context,
            quoteLevel + 1,
            unquoteLevel,
            true
          ))
        ])
      ]
    }
    case 'unquote': {
      if (quoteLevel === unquoteLevel) {
        return [yield* evaluate(subExpression, context)]
      } else if (quoteLevel > unquoteLevel) {
        return [
          makeList([
            makeSymbol(quoteType, true),
            ...(yield* quasiquoteExpressionInner(
              subExpression,
              context,
              quoteLevel,
              unquoteLevel + 1,
              true
            ))
          ])
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
        if (unquoted.type === 'empty list') {
          return []
        }

        if (unquoted.type !== 'pair') {
          return handleRuntimeError(
            context,
            new errors.UnquoteSplicingEvaluatedToNonList(unquoted, expression)
          )
        }
        const list = flattenPairToList(unquoted)
        if (list.type !== 'List') {
          return handleRuntimeError(
            context,
            new errors.UnquoteSplicingEvaluatedToNonList(unquoted, expression)
          )
        }

        return list.value.map(element => element.value)
      } else if (quoteLevel > unquoteLevel) {
        return [
          makeList([
            makeSymbol(quoteType, true),
            ...(yield* quasiquoteExpressionInner(
              subExpression,
              context,
              quoteLevel,
              unquoteLevel + 1,
              true
            ))
          ])
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

export function* quasiquoteExpression(expression: SyntaxNode, context: Context): ValueGenerator {
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
  expression: SyntaxNode,
  context: Context,
  quoteLevel: number,
  unquoteLevel: number,
  isUnquoteSplicingAllowed: boolean
): Generator<Context, Value[]> {
  switch (expression.type) {
    case 'boolean':
    case 'number':
    case 'string':
    case 'symbol':
      return [quoteLiteral(expression)]
    case 'list': {
      // Handle special forms, i.e., (quasiquote expr), (unquote expr), and (unquote-splicing expr)
      const maybeHandledAsSpecialForm: Value[] | null = yield* handleSpecialQuotationForm(
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
      const quoted: Value[] = []
      for (const element of expression.elements) {
        // Allow unquote-splicing and spread each result
        quoted.push(
          ...(yield* quasiquoteExpressionInner(element, context, quoteLevel, unquoteLevel, true))
        )
      }
      return [makeList(quoted)]
    }
    case 'dotted list': {
      const beforeDot: Value[] = []
      for (const element of expression.pre) {
        // Allow unquote-splicing and spread each result
        beforeDot.push(
          ...(yield* quasiquoteExpressionInner(element, context, quoteLevel, unquoteLevel, true))
        )
      }
      // Do not allow splicing unquote at the tail
      const afterDot = (yield* quasiquoteExpressionInner(
        expression.post,
        context,
        quoteLevel,
        unquoteLevel,
        false
      ))[0]
      return [makeImproperList(beforeDot, afterDot)]
    }
  }
}
