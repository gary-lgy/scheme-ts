import * as errors from '../errors/errors'
import { SchemeExpression, SchemeIdentifier, SchemeList, SchemeSequence } from '../lang/scheme'
import { Context, Frame } from '../types'
import { makeEmptyList } from './ExpressibleValue'
import { evaluate, ValueGenerator } from './interpreter'
import { quasiquoteExpression, quoteExpression } from './quote'
import {
  extendCurrentEnvironment,
  handleRuntimeError,
  isTruthy,
  popEnvironment,
  pushEnvironment,
  setVariable
} from './util'

// Special forms definitions

export type SpecialForm =
  | DefineForm
  | SetForm
  | LambdaForm
  | IfForm
  | LetForm
  | LetStarForm
  | LetRecForm
  | QuoteForm
  | QuasiquoteForm
  | UnquoteForm
  | UnquoteSplicingForm

export type DefineForm = {
  tag: 'define'
  name: SchemeIdentifier
  value: SchemeExpression
}

export type SetForm = {
  tag: 'set!'
  name: SchemeIdentifier
  value: SchemeExpression
}

export type LambdaArgumentPassingStyle =
  | { style: 'fixed-args'; numParams: number }
  | { style: 'var-args'; minNumParams: number }
  | { style: 'rest-args' }

export type LambdaForm = {
  tag: 'lambda'
  parameters: SchemeIdentifier[]
  body: SchemeExpression[]
  argumentPassingStyle: LambdaArgumentPassingStyle
}

export type IfForm = {
  tag: 'if'
  test: SchemeExpression
  consequent: SchemeExpression
  alternative?: SchemeExpression
}

export type LetBinding = {
  name: SchemeIdentifier
  value: SchemeExpression
}

export type LetForm = {
  tag: 'let'
  bindings: LetBinding[]
  body: SchemeSequence
}

export type LetStarForm = {
  tag: 'let*'
  bindings: LetBinding[]
  body: SchemeSequence
}

export type LetRecForm = {
  tag: 'letrec'
  bindings: LetBinding[]
  body: SchemeSequence
}

export type QuoteForm = {
  tag: 'quote'
  expression: SchemeExpression
}

export type QuasiquoteForm = {
  tag: 'quasiquote'
  expression: SchemeExpression
}

export type UnquoteForm = {
  tag: 'unquote'
  expression: SchemeExpression
}

export type UnquoteSplicingForm = {
  tag: 'unquote-splicing'
  expression: SchemeExpression
}

// Evaluation of special forms

export function* evaluateSpecialForm(form: SpecialForm, context: Context): ValueGenerator {
  const environment = context.runtime.environments[0]
  switch (form.tag) {
    case 'define': {
      // TODO: disallow mixing of definitions and expressions?
      const value = yield* evaluate(form.value, context)
      const frame = context.runtime.environments[0].head
      frame[form.name.name] = value
      return { type: 'EVEmptyList' }
    }
    case 'lambda': {
      return {
        type: 'EVProcedure',
        parameters: form.parameters.map(id => id.name),
        argumentPassingStyle: form.argumentPassingStyle,
        variant: 'CompoundProcedure',
        body: form.body,
        environment
      }
    }
    case 'set!': {
      const value = yield* evaluate(form.value, context)
      setVariable(context, form.name.name, value)
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
    case 'let': {
      const frame: Frame = {}
      for (const binding of form.bindings) {
        frame[binding.name.name] = yield* evaluate(binding.value, context)
      }
      const newEnvironment = extendCurrentEnvironment(context, 'letEnvironment', frame)

      pushEnvironment(context, newEnvironment)
      const result = yield* evaluate(form.body, context)
      popEnvironment(context)

      return result
    }
    case 'let*': {
      let numNewFrames = 0
      for (const binding of form.bindings) {
        const frame: Frame = {}
        frame[binding.name.name] = yield* evaluate(binding.value, context)
        const newEnvironment = extendCurrentEnvironment(context, 'let*Environment', frame)
        pushEnvironment(context, newEnvironment)
        numNewFrames++
      }

      const result = yield* evaluate(form.body, context)

      while (numNewFrames--) {
        popEnvironment(context)
      }

      return result
    }
    case 'letrec': {
      const frame: Frame = {}
      for (const binding of form.bindings) {
        frame[binding.name.name] = makeEmptyList()
      }
      const newEnvironment = extendCurrentEnvironment(context, 'letrecEnvironment', frame)
      pushEnvironment(context, newEnvironment)

      for (const binding of form.bindings) {
        frame[binding.name.name] = yield* evaluate(binding.value, context)
      }

      const result = yield* evaluate(form.body, context)
      popEnvironment(context)

      return result
    }
    case 'quote': {
      return quoteExpression(form.expression, context)
    }
    case 'quasiquote': {
      const quoted = yield* quasiquoteExpression(form.expression, context, 1, 1, false)
      if (quoted.length !== 1) {
        return handleRuntimeError(
          context,
          new errors.UnreachableCodeReached('top-level quasiquote should return a single value')
        )
      }
      return quoted[0]
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
  switch (tag) {
    case 'define': {
      // TODO: allow procedure definition using `define'?
      if (list.elements.length !== 3) {
        return handleRuntimeError(context, new errors.DefineSyntaxError(list))
      }
      const identifier = list.elements[1]
      if (identifier.type === 'Identifier') {
        return {
          tag,
          name: identifier,
          value: list.elements[2]
        }
      } else {
        return handleRuntimeError(context, new errors.DefineSyntaxError(list))
      }
    }
    case 'lambda': {
      // TODO: varargs?
      if (list.elements.length <= 2 || list.elements[1].type !== 'List') {
        return handleRuntimeError(context, new errors.LambdaSyntaxError(list))
      }
      const parameters: SchemeIdentifier[] = []
      list.elements[1].elements.forEach(element => {
        if (element.type === 'Identifier') {
          return parameters.push(element)
        } else {
          return handleRuntimeError(context, new errors.LambdaSyntaxError(list))
        }
      })
      return {
        tag,
        parameters,
        body: list.elements.slice(2),
        argumentPassingStyle: {
          style: 'fixed-args',
          numParams: parameters.length
        }
      }
    }
    case 'set!': {
      if (list.elements.length !== 3 || list.elements[1].type !== 'Identifier') {
        return handleRuntimeError(context, new errors.SetSyntaxError(list))
      }
      return {
        tag,
        name: list.elements[1],
        value: list.elements[2]
      }
    }
    case 'if': {
      if (list.elements.length < 3 || list.elements.length > 4) {
        return handleRuntimeError(context, new errors.IfSyntaxError(list))
      }
      return {
        tag,
        test: list.elements[1],
        consequent: list.elements[2],
        alternative: list.elements[3]
      }
    }
    case 'let':
    case 'let*':
    case 'letrec': {
      if (list.elements.length < 3 || list.elements[1].type !== 'List') {
        return handleRuntimeError(context, new errors.LetSyntaxError(list))
      }
      const bindings: LetBinding[] = list.elements[1].elements.map(pair => {
        if (
          pair.type !== 'List' ||
          pair.elements.length !== 2 ||
          pair.elements[0].type !== 'Identifier'
        ) {
          return handleRuntimeError(context, new errors.LetSyntaxError(list))
        }

        return {
          name: pair.elements[0],
          value: pair.elements[1]
        }
      })

      return {
        tag,
        bindings,
        body: {
          type: 'Sequence',
          expressions: list.elements.slice(2),
          loc: list.loc
        }
      }
    }
    case 'quote':
    case 'quasiquote':
    case 'unquote':
    case 'unquote-splicing': {
      if (list.elements.length !== 2) {
        return handleRuntimeError(context, new errors.QuoteSyntaxError(tag, list))
      }
      return {
        tag,
        expression: list.elements[1]
      }
    }
    default:
      return undefined
  }
}
