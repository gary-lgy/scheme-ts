import * as errors from '../errors/errors'
import {
  SchemeBoolLiteral,
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
import { apply, listOfArguments } from './procedure'
import { ExpressibleValue } from './runtime'
import { evaluateSpecialForm, listToSpecialForm } from './SpecialForm'
import { extendCurrentEnvironment, getVariable, handleRuntimeError, pushEnvironment } from './util'

function* visit(context: Context, node: SchemeExpression) {
  context.runtime.nodes.unshift(node)
  yield context
}

function* leave(context: Context) {
  context.runtime.nodes.shift()
  yield context
}

export type ValueGenerator = Generator<Context, ExpressibleValue>
export type Evaluator<T extends SchemeExpression> = (node: T, context: Context) => ValueGenerator

export const evaluators: { [key in SchemeExpressionType]: Evaluator<SchemeExpression> } = {
  Program: function* (node: SchemeProgram, context: Context): ValueGenerator {
    context.numberOfOuterEnvironments += 1
    const environment = extendCurrentEnvironment(context, 'programEnvironment')
    pushEnvironment(context, environment)
    const result = yield* evaluate(node.body, context)
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
      return { type: 'EVEmptyList' }
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
    const procedureName =
      firstElement.type === 'Identifier' ? firstElement.name : '[Anonymous procedure]'
    return yield* apply(context, procedure, procedureName, args, node)
  },

  StringLiteral: function* (node: SchemeStringLiteral, context: Context): ValueGenerator {
    return {
      type: 'EVString',
      value: node.value
    }
  },

  NumberLiteral: function* (node: SchemeNumberLiteral, context: Context): ValueGenerator {
    return {
      type: 'EVNumber',
      value: node.value
    }
  },

  BoolLiteral: function* (node: SchemeBoolLiteral, context: Context): ValueGenerator {
    return {
      type: 'EVBool',
      value: node.value
    }
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

export function* evaluate(
  node: SchemeExpression,
  context: Context
): Generator<Context, ExpressibleValue> {
  yield* visit(context, node)
  const evaluator = evaluators[node.type]
  const result = yield* evaluator(node, context)
  yield* leave(context)
  return result
}
