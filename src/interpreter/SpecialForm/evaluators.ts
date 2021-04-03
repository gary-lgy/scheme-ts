// Evaluation of special forms

import * as errors from '../../errors/errors'
import { Context, Frame } from '../../types'
import { ExpressibleValue, makeEmptyList } from '../ExpressibleValue'
import { evaluate, ValueGenerator } from '../interpreter'
import { quasiquoteExpression, quoteExpression } from '../quote'
import {
  extendCurrentEnvironment,
  handleRuntimeError,
  isTruthy,
  popEnvironment,
  pushEnvironment,
  setVariable
} from '../util'
import {
  DefineForm,
  IfForm,
  LambdaForm,
  LetForm,
  LetRecForm,
  LetStarForm,
  SetBangForm,
  SpecialForm
} from './definitions'

export function* evaluateSpecialForm(form: SpecialForm, context: Context): ValueGenerator {
  switch (form.tag) {
    case 'define':
      return yield* evaluateDefineForm(form, context)
    case 'lambda':
      return yield* evaluateLambdaForm(form, context)
    case 'set!':
      return yield* evaluateSetBangForm(form, context)
    case 'if':
      return yield* evaluateIfForm(form, context)
    case 'let':
      return yield* evaluateLetForm(form, context)
    case 'let*':
      return yield* evaluateLetStarForm(form, context)
    case 'letrec':
      return yield* evaluateLetRecForm(form, context)
    case 'quote':
      return quoteExpression(form.expression, context)
    case 'quasiquote':
      return yield* quasiquoteExpression(form.expression, context)
    case 'unquote':
    case 'unquote-splicing':
      return handleRuntimeError(context, new errors.UnquoteInWrongContext(form.expression))
  }
}

function* evaluateDefineForm(defineForm: DefineForm, context: Context): ValueGenerator {
  // TODO: disallow mixing of definitions and expressions?
  let value: ExpressibleValue
  if (defineForm.variant === 'basic') {
    value = yield* evaluate(defineForm.value, context)
  } else {
    value = {
      type: 'EVProcedure',
      argumentPassingStyle: defineForm.argumentPassingStyle,
      parameters: defineForm.parameters.map(param => param.name),
      variant: 'CompoundProcedure',
      body: defineForm.body,
      environment: context.runtime.environments[0]
    }
  }

  const frame = context.runtime.environments[0].head
  frame[defineForm.name.name] = value
  return makeEmptyList()
}

function* evaluateLambdaForm(lambdaForm: LambdaForm, context: Context): ValueGenerator {
  return {
    type: 'EVProcedure',
    parameters: lambdaForm.parameters.map(id => id.name),
    argumentPassingStyle: lambdaForm.argumentPassingStyle,
    variant: 'CompoundProcedure',
    body: lambdaForm.body,
    environment: context.runtime.environments[0]
  }
}

function* evaluateSetBangForm(setBangForm: SetBangForm, context: Context): ValueGenerator {
  const value = yield* evaluate(setBangForm.value, context)
  setVariable(context, setBangForm.name.name, value)
  return makeEmptyList()
}

function* evaluateIfForm(ifForm: IfForm, context: Context): ValueGenerator {
  const testValue = yield* evaluate(ifForm.test, context)
  if (isTruthy(testValue)) {
    return yield* evaluate(ifForm.consequent, context)
  } else if (ifForm.alternative) {
    return yield* evaluate(ifForm.alternative, context)
  } else {
    return { type: 'EVEmptyList' }
  }
}

function* evaluateLetForm(letForm: LetForm, context: Context): ValueGenerator {
  const frame: Frame = {}
  for (const binding of letForm.bindings) {
    frame[binding.name.name] = yield* evaluate(binding.value, context)
  }
  const newEnvironment = extendCurrentEnvironment(context, 'letEnvironment', frame)

  pushEnvironment(context, newEnvironment)
  const result = yield* evaluate(letForm.body, context)
  popEnvironment(context)

  return result
}

function* evaluateLetStarForm(letStarForm: LetStarForm, context: Context): ValueGenerator {
  let numNewFrames = 0
  for (const binding of letStarForm.bindings) {
    const frame: Frame = {}
    frame[binding.name.name] = yield* evaluate(binding.value, context)
    const newEnvironment = extendCurrentEnvironment(context, 'let*Environment', frame)
    pushEnvironment(context, newEnvironment)
    numNewFrames++
  }

  const result = yield* evaluate(letStarForm.body, context)

  while (numNewFrames--) {
    popEnvironment(context)
  }

  return result
}

function* evaluateLetRecForm(letRecForm: LetRecForm, context: Context): ValueGenerator {
  const frame: Frame = {}
  for (const binding of letRecForm.bindings) {
    frame[binding.name.name] = makeEmptyList()
  }
  const newEnvironment = extendCurrentEnvironment(context, 'letrecEnvironment', frame)
  pushEnvironment(context, newEnvironment)

  for (const binding of letRecForm.bindings) {
    frame[binding.name.name] = yield* evaluate(binding.value, context)
  }

  const result = yield* evaluate(letRecForm.body, context)
  popEnvironment(context)

  return result
}
