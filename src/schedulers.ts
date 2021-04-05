/* tslint:disable:max-classes-per-file */
import { MaximumStackLimitExceeded } from './errors/errors'
import { Context, Result, Scheduler, Value } from './types'

export class PreemptiveScheduler implements Scheduler {
  constructor(public steps: number) {}

  public run(it: IterableIterator<Value>, context: Context): Promise<Result> {
    return new Promise(resolve => {
      context.runtime.isRunning = true
      // This is used in the evaluation of the REPL during a paused state.
      // The debugger is turned off while the code evaluates just above the debugger statement.
      let itValue = it.next()
      const interval: number = setInterval(() => {
        let step = 0
        try {
          while (!itValue.done && step < this.steps) {
            step++
            itValue = it.next()
          }
        } catch (e) {
          checkForStackOverflow(e, context)
          context.runtime.isRunning = false
          clearInterval(interval)
          resolve({ status: 'error' })
        }
        if (itValue.done) {
          context.runtime.isRunning = false
          clearInterval(interval)
          resolve({ status: 'finished', context, value: itValue.value })
        }
      })
    })
  }
}

function checkForStackOverflow(error: any, context: Context) {
  if (/Maximum call stack/.test(error.toString())) {
    const environments = context.runtime.environments
    const stacks: string[] = []
    let counter = 0
    for (
      let i = 0;
      counter < MaximumStackLimitExceeded.MAX_CALLS_TO_SHOW && i < environments.length;
      i++
    ) {
      const procedureName = environments[i].procedureName
      if (procedureName) {
        stacks.unshift(procedureName)
        counter++
      }
    }
    context.errors.push(new MaximumStackLimitExceeded(context.runtime.nodes[0], stacks))
  }
}
