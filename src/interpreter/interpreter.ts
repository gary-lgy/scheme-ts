/* tslint:disable:max-classes-per-file */
import * as errors from '../errors/errors'
import { RuntimeSourceError } from '../errors/runtimeSourceError'
import {
  SchemeBoolLiteral,
  SchemeExpression,
  SchemeExpressionType,
  SchemeIdentifier,
  SchemeList,
  SchemeNumberLiteral,
  SchemeProgram,
  SchemeQuasiquote,
  SchemeQuote,
  SchemeSequence,
  SchemeStringLiteral,
  SchemeUnquote,
  SchemeUnquoteSplicing
} from '../lang/scheme'
import { Context, Environment, Frame } from '../types'
import { EVPair, EVProcedure, ExpressibleValue, SpecialForm } from './runtime'

const extendProcedureEnvironment = (
  environment: Environment,
  parameters: string[],
  procedureName: string,
  args: ExpressibleValue[]
): Environment => {
  const frame = {}
  const newEnvironment: Environment = {
    name: procedureName,
    tail: environment,
    head: frame,
    procedureName: procedureName
  }
  parameters.forEach((param, index) => {
    frame[param] = args[index]
  })
  return newEnvironment
}

const extendCurrentEnvironment = (
  context: Context,
  name: string,
  head: Frame = {}
): Environment => {
  return {
    name,
    tail: currentEnvironment(context),
    head
  }
}

const handleRuntimeError = (context: Context, error: RuntimeSourceError): never => {
  context.errors.push(error)
  context.runtime.environments = context.runtime.environments.slice(
    -context.numberOfOuterEnvironments
  )
  throw error
}

function* visit(context: Context, node: SchemeExpression) {
  context.runtime.nodes.unshift(node)
  yield context
}

function* leave(context: Context) {
  context.runtime.nodes.shift()
  yield context
}

const currentEnvironment = (context: Context) => context.runtime.environments[0]
// const replaceEnvironment = (context: Context, environment: Environment) =>
//   (context.runtime.environments[0] = environment)
const popEnvironment = (context: Context) => context.runtime.environments.shift()
const pushEnvironment = (context: Context, environment: Environment) =>
  context.runtime.environments.unshift(environment)

const getVariable = (context: Context, name: string) => {
  let environment: Environment | null = context.runtime.environments[0]
  while (environment) {
    if (environment.head.hasOwnProperty(name)) {
      return environment.head[name]
    } else {
      environment = environment.tail
    }
  }
  return undefined
}

const setVariable = (context: Context, name: string, value: any) => {
  let environment: Environment | null = context.runtime.environments[0]
  while (environment) {
    if (environment.head.hasOwnProperty(name)) {
      environment.head[name] = value
      return
    } else {
      environment = environment.tail
    }
  }
  return handleRuntimeError(context, new errors.UndefinedVariable(name, context.runtime.nodes[0]))
}

const isTruthy = (value: ExpressibleValue) => value.type !== 'EVBool' || value.value

function* evaluateSpecialForm(form: SpecialForm, context: Context): ValueGenerator {
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
  }
}

const listToSpecialForm = (
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
    if (list.elements.length != 3 || list.elements[1].type !== 'Identifier') {
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
  } else {
    return undefined
  }
}

const listOfValues = (...values: ExpressibleValue[]): ExpressibleValue => {
  const precursor: EVPair = {
    type: 'EVPair',
    head: {
      type: 'EVEmptyList'
    },
    tail: {
      type: 'EVEmptyList'
    }
  }
  let prev = precursor
  for (const value of values) {
    const newTail: EVPair = {
      type: 'EVPair',
      head: value,
      tail: {
        type: 'EVEmptyList'
      }
    }
    prev.tail = newTail
    prev = newTail
  }
  return precursor.tail
}

const quoteExpression = (expression: SchemeExpression, context: Context): ExpressibleValue => {
  switch (expression.type) {
    case 'BoolLiteral':
      return {
        type: 'EVBool',
        value: expression.value
      }
    case 'NumberLiteral':
      return {
        type: 'EVNumber',
        value: expression.value
      }
    case 'StringLiteral':
      return {
        type: 'EVString',
        value: expression.value
      }
    case 'Identifier':
      return {
        type: 'EVSymbol',
        value: expression.name
      }
    case 'List':
      return listOfValues(...expression.elements.map(elem => quoteExpression(elem, context)))
    case 'Quote':
      return listOfValues(
        { type: 'EVSymbol', value: 'quote' },
        quoteExpression(expression.expression, context)
      )
    case 'Quasiquote':
      return listOfValues(
        { type: 'EVSymbol', value: 'quosiquote' },
        quoteExpression(expression.expression, context)
      )
    case 'Unquote':
      return listOfValues(
        { type: 'EVSymbol', value: 'unquote' },
        quoteExpression(expression.expression, context)
      )
    case 'UnquoteSplicing':
      return listOfValues(
        { type: 'EVSymbol', value: 'unquote-splicing' },
        quoteExpression(expression.expression, context)
      )
    case 'Program':
    case 'Sequence':
      return handleRuntimeError(
        context,
        new errors.UnexpectedQuotationError(context.runtime.nodes[0])
      )
  }
}

export type ValueGenerator = Generator<Context, ExpressibleValue>
export type Evaluator<T extends SchemeExpression> = (node: T, context: Context) => ValueGenerator

// TODO: refactor type Value to ExpressibleValue?
/**
 * WARNING: Do not use object literal shorthands, e.g.
 *   {
 *     *Literal(node: es.Literal, ...) {...},
 *     *ThisExpression(node: es.ThisExpression, ..._ {...},
 *     ...
 *   }
 * They do not minify well, raising uncaught syntax errors in production.
 * See: https://github.com/webpack/webpack/issues/7566
 */
// tslint:disable:object-literal-shorthand
// prettier-ignore
export const evaluators: { [key in SchemeExpressionType]: Evaluator<SchemeExpression> } = {
  Program: function* (node: SchemeProgram, context: Context): ValueGenerator {
    context.numberOfOuterEnvironments += 1
    const environment = extendCurrentEnvironment(context, 'programEnvironment')
    pushEnvironment(context, environment)
    const result = yield* evaluate(node.body, context);
    return result;
  },

  Sequence: function* (node: SchemeSequence, context: Context): ValueGenerator {
    let result : ExpressibleValue
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
      return handleRuntimeError(context, new errors.CallingNonFunctionValue(context, node))
    }

    const args = yield* listOfArguments(node.elements.slice(1), context)
    const procedureName = firstElement.type === 'Identifier' ? firstElement.name : '[Anonymous procedure]'
    return yield* apply(context, procedure, procedureName, args, node)
  },

  Quote: function* (node: SchemeQuote, context: Context): ValueGenerator {
    return quoteExpression(node.expression, context)
  },

  Quasiquote: function* (node: SchemeQuasiquote, context: Context): ValueGenerator {
    throw new Error('quasiquote is not implemented')
  },

  Unquote: function* (node: SchemeUnquote, context: Context): ValueGenerator {
    throw new Error('unquote is not implemented')
  },

  UnquoteSplicing: function* (node: SchemeUnquoteSplicing, context: Context): ValueGenerator {
    throw new Error('unquote-splicing is not implemented')
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
      type:'EVBool',
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
  },
}
// tslint:enable:object-literal-shorthand

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

const checkNumberOfArguments = (
  context: Context,
  procedure: EVProcedure,
  procedureName: string,
  callExpression: SchemeList
) => {
  const numArgs = callExpression.elements.length - 1
  if (
    procedure.argumentPassingStyle.style === 'fixed-args' &&
    procedure.argumentPassingStyle.numParams !== numArgs
  ) {
    return handleRuntimeError(
      context,
      new errors.InvalidNumberOfArguments(
        callExpression,
        procedureName,
        procedure.argumentPassingStyle.numParams,
        numArgs
      )
    )
  } else if (
    procedure.argumentPassingStyle.style === 'var-args' &&
    numArgs < procedure.argumentPassingStyle.minNumParams
  ) {
    return handleRuntimeError(
      context,
      new errors.NotEnoughArguments(
        callExpression,
        procedureName,
        procedure.argumentPassingStyle.minNumParams,
        numArgs
      )
    )
  }
  return undefined
}

function* listOfArguments(
  expressions: SchemeExpression[],
  context: Context
): Generator<Context, ExpressibleValue[]> {
  const values: ExpressibleValue[] = []
  for (const expression of expressions) {
    values.push(yield* evaluate(expression, context))
  }
  return values
}

function* apply(
  context: Context,
  procedure: EVProcedure,
  procedureName: string,
  args: ExpressibleValue[],
  node: SchemeList
) {
  // TODO: TCO
  checkNumberOfArguments(context, procedure, procedureName, node)
  if (procedure.variant === 'CompoundProcedure') {
    const environment = extendProcedureEnvironment(
      procedure.environment,
      procedure.parameters,
      procedureName,
      args
    )
    pushEnvironment(context, environment)
    const result = yield* evaluate(procedure.body, context)
    popEnvironment(context)
    return result
  } else {
    try {
      return procedure.body(args)
    } catch (e) {
      return handleRuntimeError(context, new errors.BuiltinProcedureError(e, node))
    }
  }
}
