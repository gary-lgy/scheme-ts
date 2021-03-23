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
  SchemeSequence,
  SchemeStringLiteral
} from '../lang/scheme'
import { Context, Environment } from '../types'
import { EVList, ExpressibleValue, Frame, FrameBinding } from './runtime'

// const createEnvironment = (
//   closure: Closure,
//   args: Value[],
//   callExpression?: es.CallExpression
// ): Environment => {
//   const environment: Environment = {
//     name: closure.functionName, // TODO: Change this
//     tail: closure.environment,
//     head: {}
//   }
//   if (callExpression) {
//     environment.callExpression = {
//       ...callExpression,
//       arguments: args.map(primitive)
//     }
//   }
//   closure.node.params.forEach((param, index) => {
//     const ident = param as es.Identifier
//     environment.head[ident.name] = args[index]
//   })
//   return environment
// }

const createBlockEnvironment = (
  context: Context,
  name = 'blockEnvironment',
  head: Frame = new Frame()
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

// const DECLARED_BUT_NOT_YET_ASSIGNED = Symbol('Used to implement hoisting')

// function declareIdentifier(context: Context, name: string, node: SchemeExpression) {
//   const environment = currentEnvironment(context)
//   if (environment.head.hasOwnProperty(name)) {
//     const descriptors = Object.getOwnPropertyDescriptors(environment.head)

//     return handleRuntimeError(
//       context,
//       new errors.VariableRedeclaration(node, name, descriptors[name].writable)
//     )
//   }
//   environment.head[name] = DECLARED_BUT_NOT_YET_ASSIGNED
//   return environment
// }

// function declareVariables(context: Context, node: es.VariableDeclaration) {
//   for (const declaration of node.declarations) {
//     declareIdentifier(context, (declaration.id as es.Identifier).name, node)
//   }
// }

// function declareFunctionsAndVariables(context: Context, node: es.BlockStatement) {
//   for (const statement of node.body) {
//     switch (statement.type) {
//       case 'VariableDeclaration':
//         declareVariables(context, statement)
//         break
//       case 'FunctionDeclaration':
//         declareIdentifier(context, (statement.id as es.Identifier).name, statement)
//         break
//     }
//   }
// }

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
// const popEnvironment = (context: Context) => context.runtime.environments.shift()
const pushEnvironment = (context: Context, environment: Environment) =>
  context.runtime.environments.unshift(environment)

const variableDo = <T, U>(
  context: Context,
  name: string,
  bindingFoundCallback: (binding: FrameBinding) => T,
  bindingNotFoundCallback: () => U
) => {
  let environment: Environment | null = context.runtime.environments[0]
  while (environment) {
    const frame = environment.head
    const currentBinding = frame.get(name)
    if (currentBinding) {
      return bindingFoundCallback(currentBinding)
    } else {
      environment = environment.tail
    }
  }
  return bindingNotFoundCallback()
}

const getVariable = (context: Context, name: string) =>
  variableDo(
    context,
    name,
    binding => binding.value,
    () => undefined
  )

// const setVariable = (context: Context, name: string, newValue: ExpressibleValue) =>
//   variableDo(
//     context,
//     name,
//     binding => (binding.value = newValue),
//     () => handleRuntimeError(context, new errors.UndefinedVariable(name, context.runtime.nodes[0]))
//   )

// const checkNumberOfArguments = (
//   context: Context,
//   callee: Closure | Value,
//   args: Value[],
//   exp: es.CallExpression
// ) => {
//   if (callee instanceof Closure) {
//     if (callee.node.params.length !== args.length) {
//       return handleRuntimeError(
//         context,
//         new errors.InvalidNumberOfArguments(exp, callee.node.params.length, args.length)
//       )
//     }
//   } else {
//     if (callee.hasVarArgs === false && callee.length !== args.length) {
//       return handleRuntimeError(
//         context,
//         new errors.InvalidNumberOfArguments(exp, callee.length, args.length)
//       )
//     }
//   }
//   return undefined
// }

const processSpecialForm = (list: SchemeList, context: Context): ExpressibleValue => {
  // TODO: implement special form processing
  return {
    type: 'EVString',
    value: 'special form invocation'
  }
}

export type ValueGenerator = Generator<Context, ExpressibleValue>
export type Evaluator<T extends SchemeExpression> = (node: T, context: Context) => ValueGenerator

// function* evaluateBlockSatement(context: Context, node: es.BlockStatement) {
//   declareFunctionsAndVariables(context, node)
//   let result
//   for (const statement of node.body) {
//     result = yield* evaluate(statement, context)
//     if (
//       result instanceof ReturnValue ||
//       result instanceof TailCallReturnValue ||
//       result instanceof BreakValue ||
//       result instanceof ContinueValue
//     ) {
//       break
//     }
//   }
//   return result
// }

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
    const environment = createBlockEnvironment(context, 'programEnvironment')
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
      const list: EVList = {
        type: 'EVList',
        value: []
      }
      return list
    }

    const firstElement = node.elements[0]
    if (firstElement.type === 'Identifier') {
      const boundValue = getVariable(context, firstElement.name)
      if (boundValue) {
        const procedure = yield* evaluate(firstElement, context)
        // Procedure invocation - procedure is the value bound to the identifier
        if (boundValue.type !== 'EVProcedure') {
          return handleRuntimeError(context, new errors.CallingNonFunctionValue(context, node))
        }
        // TODO: implement invocation
        return {
          type: 'EVString',
          value: 'procedure invocation',
        }
      } else {
        // No procedure bound to the name - treat it as a special forms
        return processSpecialForm(node, context)
      }
    }

    if (firstElement.type === 'List') {
      // Procedure invocation - the procedure is the result of evaluating the first element
      // const procedure = yield* evaluate(firstElement, context)
      return {
        type: 'EVString',
        value: 'procedure invocation',
      }
      // TODO: implement invocation
    }
    return handleRuntimeError(context, new errors.CallingNonFunctionValue(context, node))
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

// export function* apply(
//   context: Context,
//   fun: Closure | Value,
//   args: (Thunk | Value)[],
//   node: es.CallExpression,
//   thisContext?: Value
// ) {
//   let result: Value
//   let total = 0

//   while (!(result instanceof ReturnValue)) {
//     if (fun instanceof Closure) {
//       checkNumberOfArguments(context, fun, args, node!)
//       const environment = createEnvironment(fun, args, node)
//       if (result instanceof TailCallReturnValue) {
//         replaceEnvironment(context, environment)
//       } else {
//         pushEnvironment(context, environment)
//         total++
//       }
//       const bodyEnvironment = createBlockEnvironment(context, 'functionBodyEnvironment')
//       bodyEnvironment.thisContext = thisContext
//       pushEnvironment(context, bodyEnvironment)
//       result = yield* evaluateBlockSatement(context, fun.node.body as es.BlockStatement)
//       popEnvironment(context)
//       if (result instanceof TailCallReturnValue) {
//         fun = result.callee
//         node = result.node
//         args = result.args
//       } else if (!(result instanceof ReturnValue)) {
//         // No Return Value, set it as undefined
//         result = new ReturnValue(undefined)
//       }
//     } else if (typeof fun === 'function') {
//       checkNumberOfArguments(context, fun, args, node!)
//       try {
//         const forcedArgs = []

//         for (const arg of args) {
//           forcedArgs.push(yield* forceIt(arg, context))
//         }

//         result = fun.apply(thisContext, forcedArgs)
//         break
//       } catch (e) {
//         // Recover from exception
//         context.runtime.environments = context.runtime.environments.slice(
//           -context.numberOfOuterEnvironments
//         )

//         const loc = node ? node.loc! : constants.UNKNOWN_LOCATION
//         if (!(e instanceof RuntimeSourceError || e instanceof errors.ExceptionError)) {
//           // The error could've arisen when the builtin called a source function which errored.
//           // If the cause was a source error, we don't want to include the error.
//           // However if the error came from the builtin itself, we need to handle it.
//           return handleRuntimeError(context, new errors.ExceptionError(e, loc))
//         }
//         result = undefined
//         throw e
//       }
//     } else {
//       return handleRuntimeError(context, new errors.CallingNonFunctionValue(fun, node))
//     }
//   }
//   // Unwraps return value and release stack environment
//   if (result instanceof ReturnValue) {
//     result = result.value
//   }
//   for (let i = 1; i <= total; i++) {
//     popEnvironment(context)
//   }
//   return result
// }
