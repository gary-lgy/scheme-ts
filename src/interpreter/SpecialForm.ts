import * as errors from '../errors/errors'
import { SchemeList } from '../lang/scheme'
import { Context } from '../types'
import { evaluate, ValueGenerator } from './interpreter'
import { quasiquoteExpression, quoteExpression } from './quote'
import { SpecialForm } from './runtime'
import { handleRuntimeError, isTruthy, setVariable } from './util'

export function* evaluateSpecialForm(form: SpecialForm, context: Context): ValueGenerator {
  const environment = context.runtime.environments[0]
  switch (form.tag) {
    case 'define': {
      // TODO: disallow mixing of definitions and expressions?
      const value = yield* evaluate(form.value, context)
      const frame = context.runtime.environments[0].head
      frame[form.name] = value
      return { type: 'EVEmptyList' }
    }
    case 'lambda': {
      return {
        type: 'EVProcedure',
        parameters: form.parameters,
        argumentPassingStyle: form.argumentPassingStyle,
        variant: 'CompoundProcedure',
        body: form.body,
        environment
      }
    }
    case 'set!': {
      const value = yield* evaluate(form.value, context)
      setVariable(context, form.name, value)
      return { type: 'EVEmptyList' }
    }
    case 'if': {
      const testValue = yield* evaluate(form.test, context)
      if (isTruthy(testValue)) {
        return yield* evaluate(form.consequent, context)
      } else if (form.alternative) {
        return yield* evaluate(form.alternative, context)
      } else {
        return { type: 'EVEmptyList' }
      }
    }
    case 'quote': {
      return quoteExpression(form.expression, context)
    }
    case 'quasiquote': {
      return yield* quasiquoteExpression(form.expression, context, 1, 1)
    }
    case 'unquote':
    case 'unquote-splicing':
      return handleRuntimeError(context, new errors.UnquoteInWrongContext(form.expression))
  }
}

// Convert a SchemeList to a special form, if the list has appropriate format.
// If no conversion is possible, return undefined
export const listToSpecialForm = (
  tag: string,
  list: SchemeList,
  context: Context
): SpecialForm | undefined => {
  if (tag === 'define') {
    // TODO: allow procedure definition using `define'?
    if (list.elements.length !== 3) {
      return handleRuntimeError(context, new errors.DefineSyntaxError(list))
    }
    const identifier = list.elements[1]
    if (identifier.type === 'Identifier') {
      return {
        tag,
        name: identifier.name,
        value: list.elements[2]
      }
    } else {
      return handleRuntimeError(context, new errors.DefineSyntaxError(list))
    }
  } else if (tag === 'lambda') {
    // TODO: varargs?
    if (list.elements.length <= 2 || list.elements[1].type !== 'List') {
      return handleRuntimeError(context, new errors.LambdaSyntaxError(list))
    }
    const parameters: string[] = []
    list.elements[1].elements.forEach(element => {
      if (element.type === 'Identifier') {
        return parameters.push(element.name)
      } else {
        return handleRuntimeError(context, new errors.LambdaSyntaxError(list))
      }
    })
    return {
      tag,
      parameters,
      body: {
        type: 'Sequence',
        expressions: list.elements.slice(2),
        loc: list.loc
      },
      argumentPassingStyle: {
        style: 'fixed-args',
        numParams: parameters.length
      }
    }
  } else if (tag === 'set!') {
    if (list.elements.length !== 3 || list.elements[1].type !== 'Identifier') {
      return handleRuntimeError(context, new errors.SetSyntaxError(list))
    }
    return {
      tag,
      name: list.elements[1].name,
      value: list.elements[2]
    }
  } else if (tag === 'if') {
    if (list.elements.length < 3 || list.elements.length > 4) {
      return handleRuntimeError(context, new errors.IfSyntaxError(list))
    }
    return {
      tag,
      test: list.elements[1],
      consequent: list.elements[2],
      alternative: list.elements[3]
    }
  } else if (
    tag === 'quote' ||
    tag === 'quasiquote' ||
    tag === 'unquote' ||
    tag === 'unquote-splicing'
  ) {
    if (list.elements.length !== 2) {
      return handleRuntimeError(context, new errors.QuoteSyntaxError(tag, list))
    }
    return {
      tag,
      expression: list.elements[1]
    }
  } else {
    return undefined
  }
}
