import { Context, createContext, Variant } from '.'
import { ExpressibleValue } from './interpreter/ExpressibleValue'
import { evaluateProgram } from './interpreter/interpreter'
import { parse } from './parser/parser'

export const runUntilDone = (
  code: string,
  context: Context
): { value: ExpressibleValue; maxNumEnvironment: number } => {
  const program = parse(code, context)
  if (!program || context.errors.length > 0) {
    throw new Error('parse unsuccessful')
  }

  const it = evaluateProgram(program, context)
  let maxNumEnvironment = 0
  context.runtime.isRunning = true
  let itValue: IteratorResult<Context, ExpressibleValue>
  try {
    while (true) {
      itValue = it.next()
      if (itValue.done) {
        break
      }
      maxNumEnvironment = Math.max(maxNumEnvironment, itValue.value.runtime.environments.length)
    }
  } catch (e) {
    context.runtime.isRunning = false
    throw e
  }

  return { value: itValue.value, maxNumEnvironment }
}

export const prepareContext = (variant: Variant, prelude?: string): Context => {
  const context = createContext(variant)
  context.errors = []

  if (context.prelude) {
    runUntilDone(context.prelude, context)
  }
  if (prelude) {
    runUntilDone(prelude, context)
  }

  return context
}
