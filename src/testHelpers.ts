import { Context, createContext } from '.'
import { ExpressibleValue } from './interpreter/ExpressibleValue'
import { evaluate } from './interpreter/interpreter'
import { parse } from './parser/parser'

const runUntilDone = (code: string, context: Context): ExpressibleValue => {
  const program = parse(code, context)
  if (!program || context.errors.length > 0) {
    throw new Error('parse unsuccessful')
  }

  const it = evaluate(program, context)
  context.runtime.isRunning = true
  let itValue = it.next()
  try {
    while (!itValue.done) {
      itValue = it.next()
    }
  } catch (e) {
    context.runtime.isRunning = false
    throw e
  }

  return itValue.value
}

export const evaluateUntilDone = (code: string): ExpressibleValue => {
  const context = createContext('s1', undefined, undefined)
  context.errors = []

  if (context.prelude) {
    runUntilDone(context.prelude, context)
  }

  return runUntilDone(code, context)
}
