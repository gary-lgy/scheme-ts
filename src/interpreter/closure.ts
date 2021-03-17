/* tslint:disable:max-classes-per-file */
import { SchemeSequence } from '../lang/scheme'
import { Context, Environment } from '../types'

/**
 * Models function value in the interpreter environment.
 */
export default class Closure {
  constructor(public body: SchemeSequence, public environment: Environment, context: Context) {}
}
