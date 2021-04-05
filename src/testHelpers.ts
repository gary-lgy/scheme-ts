import { Context, createContext } from '.'
import { ExpressibleValue } from './interpreter/ExpressibleValue'
import { evaluate } from './interpreter/interpreter'
import { parse } from './parser/parser'
import { sicpMce } from './stdlib/sicp-mce'

const runUntilDone = (
  code: string,
  context: Context
): { value: ExpressibleValue; maxNumEnvironment: number } => {
  const program = parse(code, context)
  if (!program || context.errors.length > 0) {
    throw new Error('parse unsuccessful')
  }

  const it = evaluate(program, context)
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

const prepareContext = (prelude?: string): Context => {
  const context = createContext()
  context.errors = []

  if (context.prelude) {
    runUntilDone(context.prelude, context)
  }
  if (prelude) {
    runUntilDone(prelude, context)
  }

  return context
}

export const evaluateUntilDone = (code: string, prelude?: string): ExpressibleValue => {
  const context = prepareContext(prelude)
  return runUntilDone(code, context).value
}

export const evaluateInMce = (code: string): ExpressibleValue => {
  return evaluateUntilDone(code, sicpMce)
}

export const evaluateAndCountEnvironmentsUntilDone = (
  code: string
): ReturnType<typeof runUntilDone> => {
  const context = prepareContext()
  return runUntilDone(code, context)
}
