import { createContext } from '.'
import { ExpressibleValue } from './interpreter/ExpressibleValue'
import { evaluate } from './interpreter/interpreter'
import { parse } from './parser/parser'

export const evaluateUntilDone = (code: string): ExpressibleValue => {
  const context = createContext('s1', undefined, undefined)
  context.errors = []

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
