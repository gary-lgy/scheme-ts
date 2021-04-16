// Evaluation of special forms

import * as errors from '../../errors/errors'
import { Context, Frame } from '../../types'
import { evaluate, evaluateSequence, ValueGenerator } from '../interpreter'
import { apply, isParentInTailContext, tryEnterTailContext } from '../procedure'
import { quasiquoteExpression, quoteExpression } from '../quote'
import { makeBool, makeEmptyList } from '../SExpression'
import {
  extendCurrentEnvironment,
  handleRuntimeError,
  introduceBinding,
  isTruthy,
  popEnvironment,
  pushEnvironment,
  setVariable
} from '../util'
import { Macro, Value } from '../Value'
import {
  AndForm,
  BeginForm,
  CondForm,
  DefineForm,
  DefMacroForm,
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
    case 'defmacro':
      return yield* evaluateDefMacroForm(form, context)
  }
}

function* evaluateDefineForm(defineForm: DefineForm, context: Context): ValueGenerator {
  // TODO: disallow mixing of definitions and expressions?
  let value: Value
  if (defineForm.variant === 'basic') {
    value = yield* evaluate(defineForm.value, context)
  } else {
    value = {
      type: 'procedure',
      callSignature: defineForm.callSignature,
      name: defineForm.name.value,
      variant: 'CompoundProcedure',
      body: defineForm.body,
      environment: context.runtime.environments[0]
    }
  }

  const frame = context.runtime.environments[0].head
  introduceBinding(context, frame, defineForm.name.isFromSource, defineForm.name.value, value)
  return makeEmptyList()
}

function* evaluateLambdaForm(lambdaForm: LambdaForm, context: Context): ValueGenerator {
  return {
    type: 'procedure',
    callSignature: lambdaForm.callSignature,
    name: '[anonymous procedure]',
    variant: 'CompoundProcedure',
    body: lambdaForm.body,
    environment: context.runtime.environments[0]
  }
}

function* evaluateSetBangForm(setBangForm: SetBangForm, context: Context): ValueGenerator {
  const value = yield* evaluate(setBangForm.value, context)
  setVariable(context, setBangForm.name.value, value)
  return makeEmptyList()
}

function* evaluateIfForm(ifForm: IfForm, context: Context): ValueGenerator {
  const testValue = yield* evaluate(ifForm.test, context)
  tryEnterTailContext(context)
  if (isTruthy(testValue)) {
    return yield* evaluate(ifForm.consequent, context)
  } else if (ifForm.alternative) {
    return yield* evaluate(ifForm.alternative, context)
  } else {
    return makeEmptyList()
  }
}

function* evaluateCondForm(condForm: CondForm, context: Context): ValueGenerator {
  for (const clause of condForm.clauses) {
    const testResult = yield* evaluate(clause.test, context)
    if (!isTruthy(testResult)) {
      continue
    }

    if (clause.type === 'basic') {
      return yield* evaluateSequence(clause.body, context, true, testResult)
    } else {
      const procedure = yield* evaluate(clause.body, context)

      if (procedure.type !== 'procedure') {
        return handleRuntimeError(
          context,
          new errors.CondProcedureClauseError(context.runtime.nodes[0])
        )
      }

      if (isParentInTailContext(context)) {
        return {
          type: 'TailCall',
          procedure,
          args: [testResult],
          node: clause.node
        }
      } else {
        return yield* apply(context, procedure, [testResult], clause.node)
      }
    }
  }

  // none of the clauses were evaluated, evaluate the else clause
  if (!condForm.elseClause) {
    return makeEmptyList()
  }

  return yield* evaluateSequence(condForm.elseClause.body, context, true)
}

function* evaluateLetForm(letForm: LetForm, context: Context): ValueGenerator {
  const frame: Frame = {}
  for (const binding of letForm.bindings) {
    const value = yield* evaluate(binding.value, context)
    introduceBinding(context, frame, binding.name.isFromSource, binding.name.value, value)
  }
  const newEnvironment = extendCurrentEnvironment(context, 'letEnvironment', frame)

  pushEnvironment(context, newEnvironment)
  const result = yield* evaluateSequence(letForm.body, context, true)
  popEnvironment(context)

  return result
}

function* evaluateLetStarForm(letStarForm: LetStarForm, context: Context): ValueGenerator {
  let numNewFrames = 0
  for (const binding of letStarForm.bindings) {
    const frame: Frame = {}
    const value = yield* evaluate(binding.value, context)
    introduceBinding(context, frame, binding.name.isFromSource, binding.name.value, value)

    const newEnvironment = extendCurrentEnvironment(context, 'let*Environment', frame)
    pushEnvironment(context, newEnvironment)
    numNewFrames++
  }

  const result = yield* evaluateSequence(letStarForm.body, context, true)

  while (numNewFrames--) {
    popEnvironment(context)
  }

  return result
}

function* evaluateLetRecForm(letRecForm: LetRecForm, context: Context): ValueGenerator {
  const frame: Frame = {}
  for (const binding of letRecForm.bindings) {
    introduceBinding(context, frame, binding.name.isFromSource, binding.name.value, makeEmptyList())
  }
  const newEnvironment = extendCurrentEnvironment(context, 'letrecEnvironment', frame)
  pushEnvironment(context, newEnvironment)

  for (const binding of letRecForm.bindings) {
    frame[binding.name.value] = yield* evaluate(binding.value, context)
  }

  const result = yield* evaluateSequence(letRecForm.body, context, true)
  popEnvironment(context)

  return result
}

function* evaluateBeginForm(beginForm: BeginForm, context: Context): ValueGenerator {
  return yield* evaluateSequence(beginForm.body, context, true)
}

function* evaluateAndForm(andForm: AndForm, context: Context): ValueGenerator {
  if (andForm.arguments.length === 0) {
    return makeBool(true)
  } else {
    for (let i = 0; i < andForm.arguments.length - 1; i++) {
      const arg = andForm.arguments[i]
      const result = yield* evaluate(arg, context)
      if (!isTruthy(result)) {
        return makeBool(false)
      }
    }

    // Evaluate the last form in tail context
    const lastArg = andForm.arguments[andForm.arguments.length - 1]
    tryEnterTailContext(context)
    return yield* evaluate(lastArg, context)
  }
}

function* evaluateOrForm(orForm: OrForm, context: Context): ValueGenerator {
  if (orForm.arguments.length === 0) {
    return makeBool(false)
  } else {
    for (let i = 0; i < orForm.arguments.length - 1; i++) {
      const arg = orForm.arguments[i]
      const result = yield* evaluate(arg, context)
      if (isTruthy(result)) {
        return result
      }
    }

    // Evaluate the last form in tail context
    const lastArg = orForm.arguments[orForm.arguments.length - 1]
    tryEnterTailContext(context)
    return yield* evaluate(lastArg, context)
  }
}

function* evaluateDefMacroForm(defMacroForm: DefMacroForm, context: Context): ValueGenerator {
  const macro: Macro = {
    type: 'macro',
    name: defMacroForm.name.value,
    environment: context.runtime.environments[0],
    body: defMacroForm.body,
    callSignature: defMacroForm.callSignature
  }
  const frame = context.runtime.environments[0].head
  introduceBinding(context, frame, defMacroForm.name.isFromSource, defMacroForm.name.value, macro)
  return makeEmptyList()
}
