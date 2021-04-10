import { UNKNOWN_LOCATION } from '../constants'
import { MacroExpansionError } from '../errors/errors'
import { Context } from '../types'
import { flattenPairToList } from '../utils/listHelpers'
import { ExpressibleValue, Macro, makeImproperList, makeList } from './ExpressibleValue'
import { evaluate, ValueGenerator } from './interpreter'
import {
  checkNumberOfArguments,
  extendEnvironmentWithNewBindings,
  matchArgumentsToParameters,
  tryEnterTailContext
} from './procedure'
import { SyntaxNode } from './SchemeSyntax'
import { handleRuntimeError, popEnvironment, pushEnvironment } from './util'

export function* expandMacro(
  context: Context,
  macro: Macro,
  suppliedArgs: ExpressibleValue[],
  node: SyntaxNode
): ValueGenerator {
  checkNumberOfArguments(context, macro.name, macro.callSignature, suppliedArgs.length, node)
  const environment = extendEnvironmentWithNewBindings(
    context,
    macro.environment,
    macro.name,
    matchArgumentsToParameters(macro.callSignature, suppliedArgs)
  )
  pushEnvironment(context, environment)

  let result!: ExpressibleValue
  for (let i = 0; i < macro.body.length; i++) {
    const expression = macro.body[i]
    result = yield* evaluate(expression, context)
  }

  popEnvironment(context)

  return result
}

/**
 * Expands the given macro using the given arguments and evaluate the expanded form.
 */
export function* useMacro(
  context: Context,
  macro: Macro,
  suppliedArgs: SyntaxNode[],
  node: SyntaxNode
): ValueGenerator {
  // Convert the syntax forms to expressible values for use in the macro body
  const suppliedSExpressions = suppliedArgs.map(syntaxToExpressibleValue)

  const expandedSExpression = yield* expandMacro(context, macro, suppliedSExpressions, node)

  let expandedSyntax: SyntaxNode
  try {
    expandedSyntax = expressibleValueToSyntax(expandedSExpression)
  } catch (e: any) {
    return handleRuntimeError(context, new MacroExpansionError(e, node))
  }

  // Must pass down the tail context of the macro use site
  tryEnterTailContext(context)
  return yield* evaluate(expandedSyntax, context)
}

const syntaxToExpressibleValue = (syntax: SyntaxNode): ExpressibleValue => {
  switch (syntax.type) {
    case 'boolean':
    case 'number':
    case 'string':
    case 'symbol':
      return { ...syntax }
    case 'dotted list':
      return makeImproperList(
        syntax.pre.map(syntaxToExpressibleValue),
        syntaxToExpressibleValue(syntax.post),
        syntax.loc
      )
    case 'list':
      return makeList(syntax.elements.map(syntaxToExpressibleValue), syntax.loc)
  }
}

const expressibleValueToSyntax = (value: ExpressibleValue): SyntaxNode => {
  switch (value.type) {
    case 'boolean':
    case 'number':
    case 'string':
    case 'symbol':
      return value
    case 'pair':
      const list = flattenPairToList(value)
      if (list.type === 'List') {
        return {
          type: 'list',
          elements: list.value.map(element => expressibleValueToSyntax(element.value)),
          loc: value.loc ?? UNKNOWN_LOCATION
        }
      } else {
        return {
          type: 'dotted list',
          pre: [
            ...list.value.properPart.map(element => element.value),
            list.value.lastPair.head
          ].map(element => expressibleValueToSyntax(element)),
          post: expressibleValueToSyntax(list.value.lastPair.tail),
          loc: value.loc ?? UNKNOWN_LOCATION
        }
      }
    case 'empty list':
      return {
        type: 'list',
        elements: [],
        loc: value.loc ?? UNKNOWN_LOCATION
      }
    case 'macro':
    case 'procedure':
    case 'TailCall':
      throw value
  }
}
