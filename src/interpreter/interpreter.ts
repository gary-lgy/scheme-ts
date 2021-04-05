import * as errors from '../errors/errors'
import {
  SchemeBoolLiteral,
  SchemeDottedList,
  SchemeExpression,
  SchemeExpressionType,
  SchemeIdentifier,
  SchemeList,
  SchemeNumberLiteral,
  SchemeProgram,
  SchemeSequence,
  SchemeStringLiteral
} from '../lang/scheme'
import { Context } from '../types'
import {
  ExpressibleValue,
  makeBool,
  makeEmptyList,
  makeNumber,
  makeString,
  TailCall
} from './ExpressibleValue'
import { apply, isParentInTailContext, listOfArguments, tryEnterTailContext } from './procedure'
import { listToSpecialForm } from './SpecialForm/converters'
import { evaluateSpecialForm } from './SpecialForm/evaluators'
import { extendCurrentEnvironment, getVariable, handleRuntimeError, pushEnvironment } from './util'

function* visit(context: Context, node: SchemeExpression) {
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
export type Evaluator<T extends SchemeExpression> = (node: T, context: Context) => ValueGenerator

export const evaluators: { [key in SchemeExpressionType]: Evaluator<SchemeExpression> } = {
  Program: function* (node: SchemeProgram, context: Context): ValueGenerator {
    context.numberOfOuterEnvironments += 1
    const environment = extendCurrentEnvironment(context, 'programEnvironment')
    pushEnvironment(context, environment)
    const result = yield* evaluate(node.body, context)

    if (result.type === 'TailCall') {
      return handleRuntimeError(
        context,
        new errors.UnreachableCodeReached('top-level eval should not return a TailCall object')
      )
    }

    return result
  },

  Sequence: function* (node: SchemeSequence, context: Context): ValueGenerator {
    let result: ExpressibleValue
    for (const expression of node.expressions) {
      result = yield* evaluate(expression, context)
    }
    return result!
  },

  List: function* (node: SchemeList, context: Context): ValueGenerator {
    if (node.elements.length === 0) {
      // Empty list - return empty list
      return makeEmptyList()
    }

    const firstElement = node.elements[0]
    if (firstElement.type === 'Identifier' && !getVariable(context, firstElement.name)) {
      // No procedure bound to the name - treat it as a special form
      const specialForm = listToSpecialForm(firstElement.name, node, context)
      if (!specialForm) {
        return handleRuntimeError(context, new errors.UndefinedVariable(firstElement.name, node))
      }
      return yield* evaluateSpecialForm(specialForm, context)
    }

    // Procedure invocation - procedure is the value bound to the identifier
    const procedure = yield* evaluate(firstElement, context)
    if (procedure.type !== 'EVProcedure') {
      return handleRuntimeError(context, new errors.CallingNonFunctionValue(procedure, node))
    }

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
  },

  DottedList: function* (node: SchemeDottedList, context: Context): ValueGenerator {
    return handleRuntimeError(context, new errors.UnexpectedDottedList(node))
  },

  StringLiteral: function* (node: SchemeStringLiteral): ValueGenerator {
    return makeString(node.value)
  },

  NumberLiteral: function* (node: SchemeNumberLiteral): ValueGenerator {
    return makeNumber(node.value)
  },

  BoolLiteral: function* (node: SchemeBoolLiteral): ValueGenerator {
    return makeBool(node.value)
  },

  Identifier: function* (node: SchemeIdentifier, context: Context): ValueGenerator {
    const boundValue = getVariable(context, node.name)
    if (boundValue) {
      return boundValue
    } else {
      return handleRuntimeError(context, new errors.UndefinedVariable(node.name, node))
    }
  }
}

export function* evaluate(node: SchemeExpression, context: Context): ValueGenerator {
  yield* visit(context, node)
  const evaluator = evaluators[node.type]
  const result = yield* evaluator(node, context)
  yield* leave(context)
  return result
}

export function* evaluateSequence(
  expressions: SchemeExpression[],
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
