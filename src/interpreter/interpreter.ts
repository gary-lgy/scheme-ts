import * as errors from '../errors/errors'
import { Context } from '../types'
import { ExpressibleValue, TailCall } from './ExpressibleValue'
import { useMacro } from './macro'
import { apply, isParentInTailContext, listOfArguments, tryEnterTailContext } from './procedure'
import {
  SchemeProgram,
  SyntaxDottedList,
  SyntaxList,
  SyntaxNode,
  SyntaxNodeType
} from './SchemeSyntax'
import {
  makeBool,
  makeEmptyList,
  makeNumber,
  makeString,
  SBool,
  SNumber,
  SString,
  SSymbol
} from './SExpression'
import { listToSpecialForm } from './SpecialForm/converters'
import { evaluateSpecialForm } from './SpecialForm/evaluators'
import { extendCurrentEnvironment, getVariable, handleRuntimeError, pushEnvironment } from './util'

function* visit(context: Context, node: SyntaxNode) {
  context.runtime.nodes.unshift(node)
  // Start each node in non-tail context
  // The evaluator for the node can change this to true when it enters tail context
  context.runtime.inTailContext.unshift(false)
  yield context
}

function* leave(context: Context) {
  context.runtime.nodes.shift()
  context.runtime.inTailContext.shift()
  yield context
}

export type ValueGenerator = Generator<Context, ExpressibleValue | TailCall>
export type Evaluator<T extends SyntaxNode> = (node: T, context: Context) => ValueGenerator

export const evaluators: { [key in SyntaxNodeType]: Evaluator<SyntaxNode> } = {
  list: function* (node: SyntaxList, context: Context): ValueGenerator {
    if (node.elements.length === 0) {
      // Empty list - return empty list
      return makeEmptyList()
    }

    const firstElement = node.elements[0]
    if (firstElement.type === 'symbol' && !getVariable(context, firstElement.value)) {
      // No procedure bound to the name - treat it as a special form
      const specialForm = listToSpecialForm(firstElement.value, node, context)
      if (!specialForm) {
        return handleRuntimeError(context, new errors.UndefinedVariable(firstElement.value, node))
      }
      return yield* evaluateSpecialForm(specialForm, context)
    }

    const firstElementValue = yield* evaluate(firstElement, context)
    if (firstElementValue.type === 'procedure') {
      // Procedure invocation
      const procedure = firstElementValue
      const args = yield* listOfArguments(node.elements.slice(1), context)
      if (isParentInTailContext(context)) {
        const tailCall: TailCall = {
          type: 'TailCall',
          procedure,
          args,
          node
        }
        return tailCall
      } else {
        return yield* apply(context, procedure, args, node)
      }
    } else if (firstElementValue.type === 'macro') {
      // Macro use
      const macro = firstElementValue
      return yield* useMacro(context, macro, node.elements.slice(1), node)
    } else {
      return handleRuntimeError(
        context,
        new errors.CallingNonFunctionValue(firstElementValue, node)
      )
    }
  },

  'dotted list': function* (node: SyntaxDottedList, context: Context): ValueGenerator {
    return handleRuntimeError(context, new errors.UnexpectedDottedList(node))
  },

  string: function* (node: SString): ValueGenerator {
    return makeString(node.value)
  },

  number: function* (node: SNumber): ValueGenerator {
    return makeNumber(node.value)
  },

  boolean: function* (node: SBool): ValueGenerator {
    return makeBool(node.value)
  },

  symbol: function* (node: SSymbol, context: Context): ValueGenerator {
    const boundValue = getVariable(context, node.value)
    if (boundValue) {
      return boundValue
    } else {
      return handleRuntimeError(context, new errors.UndefinedVariable(node.value, node))
    }
  }
}

export function* evaluateProgram(program: SchemeProgram, context: Context): ValueGenerator {
  context.numberOfOuterEnvironments += 1
  const environment = extendCurrentEnvironment(context, 'programEnvironment')
  pushEnvironment(context, environment)

  let result!: ExpressibleValue
  for (const expression of program.body) {
    result = yield* evaluate(expression, context)
  }

  if (result.type === 'TailCall') {
    return handleRuntimeError(
      context,
      new errors.UnreachableCodeReached('top-level eval should not return a TailCall object')
    )
  }

  return result
}

export function* evaluate(node: SyntaxNode, context: Context): ValueGenerator {
  yield* visit(context, node)
  const evaluator = evaluators[node.type]
  const result = yield* evaluator(node, context)
  yield* leave(context)
  return result
}

export function* evaluateSequence(
  expressions: SyntaxNode[],
  context: Context,
  isTailSequence: boolean,
  defaultResult?: ExpressibleValue
): ValueGenerator {
  if (expressions.length === 0) {
    if (defaultResult !== undefined) {
      return defaultResult
    }
    return handleRuntimeError(
      context,
      new errors.UnreachableCodeReached('a sequence should have at least one form')
    )
  }

  for (let i = 0; i < expressions.length - 1; i++) {
    const expression = expressions[i]
    yield* evaluate(expression, context)
  }

  const lastExprression = expressions[expressions.length - 1]
  if (isTailSequence) {
    // evaluate the last expression in tail context
    tryEnterTailContext(context)
  }
  return yield* evaluate(lastExprression, context)
}
