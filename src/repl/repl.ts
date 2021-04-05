#!/usr/bin/env node
import { start } from 'repl' // 'repl' here refers to the module named 'repl' in index.d.ts
import { sourceLanguages } from '../constants'
import { createContext, IOptions, parseError, runInContext } from '../index'
import { ExecutionMethod, Variant } from '../types'
import { stringify } from '../utils/stringify'

function startRepl(
  executionMethod: ExecutionMethod = 'interpreter',
  variant: Variant = 'base',
  useRepl: boolean,
  prelude = ''
) {
  // use defaults for everything
  const context = createContext(variant, undefined, undefined)
  const options: Partial<IOptions> = {
    scheduler: 'preemptive',
    executionMethod,
    variant
  }
  runInContext(prelude, context, options).then(preludeResult => {
    if (preludeResult.status === 'finished' || preludeResult.status === 'suspended-non-det') {
      if (preludeResult.value) {
        console.log(stringify(preludeResult.value))
      }
      if (!useRepl) {
        return
      }
      start(
        // the object being passed as argument fits the interface ReplOptions in the repl module.
        {
          eval: (cmd, unusedContext, unusedFilename, callback) => {
            runInContext(cmd, context, options).then(obj => {
              if (obj.status === 'finished' || obj.status === 'suspended-non-det') {
                callback(null, obj.value)
              } else {
                callback(new Error(parseError(context.errors)), undefined)
              }
            })
          },
          writer: output => {
            if (output instanceof Error) {
              return output.message
            }
            return stringify(output)
          }
        }
      )
    } else {
      console.error(parseError(context.errors))
    }
  })
}

/**
 * Returns true iff the given variant combination is supported.
 */
function validChapterVariant(variant: any): boolean {
  for (const lang of sourceLanguages) {
    if (lang.variant === variant) return true
  }
  return false
}

function main() {
  const opt = require('node-getopt')
    .create([
      ['v', 'variant=VARIANT', 'set the Source variant (i.e., no-tco)', 'base'],
      ['h', 'help', 'display this help'],
      ['e', 'eval', "don't show REPL, only display output of evaluation"]
    ])
    .bindHelp()
    .setHelp('Usage: js-slang [PROGRAM_STRING] [OPTION]\n\n[[OPTIONS]]')
    .parseSystem()

  const variant = opt.options.variant
  const areValidChapterVariant: boolean = validChapterVariant(variant)
  if (!areValidChapterVariant) {
    throw new Error(
      'The variant combination provided is unsupported. Use the -h option to view valid chapters and variants.'
    )
  }

  const executionMethod = opt.options.variant === 'interpreter' ? 'interpreter' : 'native'
  const useRepl = !opt.options.e
  const prelude = opt.argv[0] ?? ''
  startRepl(executionMethod, variant, useRepl, prelude)
}

main()
