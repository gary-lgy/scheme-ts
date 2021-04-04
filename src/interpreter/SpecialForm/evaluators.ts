// Evaluation of special forms

import * as errors from '../../errors/errors'
import { Context, Frame } from '../../types'
import { ExpressibleValue, makeBool, makeEmptyList } from '../ExpressibleValue'
import { evaluate, ValueGenerator } from '../interpreter'
import { apply } from '../procedure'
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
  AndForm,
  BeginForm,
  CondForm,
  DefineForm,
  IfForm,
  LambdaForm,
  LetForm,
  LetRecForm,
  LetStarForm,
  OrForm,
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
    case 'cond':
      return yield* evaluateCondForm(form, context)
    case 'begin':
      return yield* evaluateBeginForm(form, context)
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
    case 'and':
      return yield* evaluateAndForm(form, context)
    case 'or':
      return yield* evaluateOrForm(form, context)
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

function* evaluateCondForm(condForm: CondForm, context: Context): ValueGenerator {
  for (const clause of condForm.clauses) {
    const testResult = yield* evaluate(clause.test, context)
    if (!isTruthy(testResult)) {
      continue
    }

    if (clause.type === 'basic') {
      let result = testResult
      for (const bodyExpression of clause.body) {
        result = yield* evaluate(bodyExpression, context)
      }

      return result
    } else {
      const procedure = yield* evaluate(clause.body, context)

      if (procedure.type !== 'EVProcedure') {
        return handleRuntimeError(
          context,
          new errors.CondProcedureClauseError(context.runtime.nodes[0])
        )
      }

      const procedureName =
        clause.body.type === 'Identifier' ? clause.body.name : '[Anonymous procedure]'

      return yield* apply(context, procedure, procedureName, [testResult], clause.node)
    }
  }

  // none of the clauses were evaluated, evaluate the else clause
  if (!condForm.elseClause) {
    return makeEmptyList()
  }

  let result: ExpressibleValue = makeEmptyList()
  for (const bodyExpression of condForm.elseClause.body) {
    result = yield* evaluate(bodyExpression, context)
  }
  return result
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

function* evaluateBeginForm(beginForm: BeginForm, context: Context): ValueGenerator {
  return yield* evaluate(beginForm.body, context)
}

function* evaluateAndForm(andForm: AndForm, context: Context): ValueGenerator {
  if (andForm.arguments.length === 0) {
    return makeBool(true)
  } else {
    let result: ExpressibleValue = yield* evaluate(andForm.arguments[0], context)
    for (let i = 1; i < andForm.arguments.length; i++) {
      if (!isTruthy(result)) {
        return makeBool(false)
      }
      const arg = andForm.arguments[i]
      result = yield* evaluate(arg, context)
    }
    return result
  }
}

function* evaluateOrForm(orForm: OrForm, context: Context): ValueGenerator {
  if (orForm.arguments.length === 0) {
    return makeBool(false)
  } else {
    let result: ExpressibleValue = yield* evaluate(orForm.arguments[0], context)
    for (let i = 1; i < orForm.arguments.length; i++) {
      if (isTruthy(result)) {
        return result
      }
      const arg = orForm.arguments[i]
      result = yield* evaluate(arg, context)
    }
    return result
  }
}
