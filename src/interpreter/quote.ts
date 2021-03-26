import * as errors from '../errors/errors'
import { SchemeExpression } from '../lang/scheme'
import { Context } from '../types'
import { ExpressibleValue } from './runtime'
import { handleRuntimeError, listOfValues } from './util'

export const quoteExpression = (
  expression: SchemeExpression,
  context: Context
): ExpressibleValue => {
  switch (expression.type) {
    case 'BoolLiteral':
      return {
        type: 'EVBool',
        value: expression.value
      }
    case 'NumberLiteral':
      return {
        type: 'EVNumber',
        value: expression.value
      }
    case 'StringLiteral':
      return {
        type: 'EVString',
        value: expression.value
      }
    case 'Identifier':
      return {
        type: 'EVSymbol',
        value: expression.name
      }
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
