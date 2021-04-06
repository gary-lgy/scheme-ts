import { MacroExpansionError } from '../errors/errors'
import { SyntaxNode } from '../lang/SchemeSyntax'
import { Context } from '../types'
import { flattenPairToList } from '../utils/listHelpers'
import {
  EVMacro,
  ExpressibleValue,
  makeBool,
  makeImproperList,
  makeList,
  makeNumber,
  makeString,
  makeSymbol
} from './ExpressibleValue'
import { evaluate, ValueGenerator } from './interpreter'
import {
  checkNumberOfArguments,
  extendEnvironmentWithNewBindings,
  matchArgumentsToParameters,
  tryEnterTailContext
} from './procedure'
import { handleRuntimeError, popEnvironment, pushEnvironment } from './util'

export function* expandMacro(
  context: Context,
  macro: EVMacro,
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
  macro: EVMacro,
  suppliedArgs: SyntaxNode[],
  node: SyntaxNode
): ValueGenerator {
  // Convert the syntax forms to expressible values for use in the macro body
  const suppliedSExpressions = suppliedArgs.map(syntaxToExpressibleValue)

  const expandedSExpression = yield* expandMacro(context, macro, suppliedSExpressions, node)

  let expandedSyntax: SyntaxNode
  try {
    expandedSyntax = expressibleValueToSyntax(expandedSExpression, node)
  } catch (e: any) {
    return handleRuntimeError(context, new MacroExpansionError(e, node))
  }

  // Must pass down the tail context of the macro use site
  tryEnterTailContext(context)
  return yield* evaluate(expandedSyntax, context)
}

const syntaxToExpressibleValue = (syntax: SyntaxNode): ExpressibleValue => {
  switch (syntax.type) {
    case 'BoolLiteral':
      return makeBool(syntax.value)
    case 'NumberLiteral':
      return makeNumber(syntax.value)
    case 'StringLiteral':
      return makeString(syntax.value)
    case 'Identifier':
      return makeSymbol(syntax.name, syntax.isFromSource)
    case 'DottedList':
      return makeImproperList(
        syntax.pre.map(syntaxToExpressibleValue),
        syntaxToExpressibleValue(syntax.post)
      )
    case 'List':
      return makeList(...syntax.elements.map(syntaxToExpressibleValue))
  }
}

const expressibleValueToSyntax = (value: ExpressibleValue, macroNode: SyntaxNode): SyntaxNode => {
  switch (value.type) {
    case 'EVBool':
      return { type: 'BoolLiteral', value: value.value, loc: macroNode.loc }
    case 'EVNumber':
      return { type: 'NumberLiteral', value: value.value, loc: macroNode.loc }
    case 'EVString':
      return { type: 'StringLiteral', value: value.value, loc: macroNode.loc }
    case 'EVSymbol':
      return {
        type: 'Identifier',
        name: value.value,
        isFromSource: value.isFromSource,
        loc: macroNode.loc
      }
    case 'EVPair':
      const list = flattenPairToList(value)
      if (list.type === 'List') {
        return {
          type: 'List',
          elements: list.value.map(element => expressibleValueToSyntax(element.value, macroNode)),
          loc: macroNode.loc
        }
      } else {
        return {
          type: 'DottedList',
          pre: [
            ...list.value.properPart.map(element => element.value),
            list.value.lastPair.head
          ].map(element => expressibleValueToSyntax(element, macroNode)),
          post: expressibleValueToSyntax(list.value.lastPair.tail, macroNode),
          loc: macroNode.loc
        }
      }
    case 'EVEmptyList':
      return {
        type: 'List',
        elements: [],
        loc: macroNode.loc
      }
    case 'EVMacro':
    case 'EVProcedure':
    case 'TailCall':
      throw value
  }
}
