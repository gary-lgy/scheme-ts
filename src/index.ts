import { SourceMapConsumer } from 'source-map'
import createContext from './createContext'
import { evaluate } from './interpreter/interpreter'
import { parse } from './parser/parser'
import { PreemptiveScheduler } from './schedulers'
import {
  Context,
  Error as ResultError,
  ExecutionMethod,
  Finished,
  Result,
  Scheduler,
  SourceError,
  Variant
} from './types'

export { SourceDocumentation } from './editors/ace/docTooltip'
export { Variant } from './types'
export { createContext, Context, Result }

export interface IOptions {
  scheduler: 'preemptive' | 'async'
  steps: number
  stepLimit: number
  executionMethod: ExecutionMethod
  variant: Variant
  originalMaxExecTime: number
  isPrelude: boolean
}

const DEFAULT_OPTIONS: IOptions = {
  scheduler: 'async',
  steps: 1000,
  stepLimit: 1000,
  executionMethod: 'auto',
  variant: 'base',
  originalMaxExecTime: 1000,
  isPrelude: false
}

// needed to work on browsers
if (typeof process === 'undefined') {
  // @ts-ignore
  SourceMapConsumer.initialize({
    'lib/mappings.wasm': 'https://unpkg.com/source-map@0.7.3/lib/mappings.wasm'
  })
}

// deals with parsing error objects and converting them to strings (for repl at least)

const verboseErrors = false
const resolvedErrorPromise = Promise.resolve({ status: 'error' } as Result)

export function parseError(errors: SourceError[], verbose: boolean = verboseErrors): string {
  const errorMessagesArr = errors.map(error => {
    const line = error.location ? error.location.start.line : '<unknown>'
    const column = error.location ? error.location.start.column : '<unknown>'
    const explanation = error.explain()

    if (verbose) {
      // TODO currently elaboration is just tagged on to a new line after the error message itself. find a better
      // way to display it.
      const elaboration = error.elaborate()
      return `Line ${line}, Column ${column}: ${explanation}\n${elaboration}\n`
    } else {
      return `Line ${line}: ${explanation}`
    }
  })
  return errorMessagesArr.join('\n')
}

export async function runInContext(
  code: string,
  context: Context,
  options: Partial<IOptions> = {}
): Promise<Result> {
  const theOptions: IOptions = { ...DEFAULT_OPTIONS, ...options }
  context.variant = determineVariant(context, options)
  context.errors = []

  const program = parse(code, context)
  if (!program) {
    return resolvedErrorPromise
  }
  if (context.errors.length > 0) {
    return resolvedErrorPromise
  }

  if (context.prelude !== null) {
    const prelude = context.prelude
    context.prelude = null
    await runInContext(prelude, context, { ...options, isPrelude: true })
    return runInContext(code, context, options)
  }

  const it = evaluate(program, context)
  const scheduler: Scheduler = new PreemptiveScheduler(theOptions.steps)
  return scheduler.run(it, context)
}

/**
 * Small function to determine the variant to be used
 * by a program, as both context and options can have
 * a variant. The variant provided in options will
 * have precedence over the variant provided in context.
 *
 * @param context The context of the program.
 * @param options Options to be used when
 *                running the program.
 *
 * @returns The variant that the program is to be run in
 */
function determineVariant(context: Context, options: Partial<IOptions>): Variant {
  if (options.variant) {
    return options.variant
  } else {
    return context.variant
  }
}

export function resume(result: Result): Finished | ResultError | Promise<Result> {
  if (result.status === 'finished' || result.status === 'error') {
    return result
  } else {
    return result.scheduler.run(result.it, result.context)
  }
}
