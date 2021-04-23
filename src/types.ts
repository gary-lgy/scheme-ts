/*
	This file contains definitions of some interfaces and classes that are used in Source (such as
	error-related classes).
*/

/* tslint:disable:max-classes-per-file */

import { Value } from './interpreter/value'
import { SyntaxNode } from './lang/syntax'
import { SourceLocation } from './sExpression'

/**
 * Defines functions that act as built-ins, but might rely on
 * different implementations. e.g display() in a web application.
 */
export interface CustomBuiltIns {
  rawDisplay: (str: string, prepend: string, externalContext: any) => void
}

export enum ErrorType {
  SYNTAX = 'Syntax',
  TYPE = 'Type',
  RUNTIME = 'Runtime'
}

export enum ErrorSeverity {
  WARNING = 'Warning',
  ERROR = 'Error'
}

// any and all errors ultimately implement this interface. as such, changes to this will affect every type of error.
export interface SourceError {
  type: ErrorType
  severity: ErrorSeverity
  location: SourceLocation
  explain(): string
  elaborate(): string
}

export type ExecutionMethod = 'native' | 'interpreter' | 'auto'

export type BaseVariant = 'base'
export type NoTCOVariant = 'no-tco'
export type MacroVariant = 'macro'
export type Variant = BaseVariant | NoTCOVariant | MacroVariant

export interface SourceLanguage {
  variant: Variant
}

export interface Context<T = any> {
  /** The external symbols that exist in the Context. */
  externalSymbols: string[]

  /** All the errors gathered */
  errors: SourceError[]

  /** Runtime Sepecific state */
  runtime: {
    isRunning: boolean
    environments: Environment[]
    nodes: SyntaxNode[]
    inTailContext: boolean[]
    nextUniqueSymbolNumber: number
  }

  moduleParams?: any

  numberOfOuterEnvironments: number

  prelude: string | null

  /**
   * Used for storing external properties.
   * For e.g, this can be used to store some application-related
   * context for use in your own built-in functions (like `display(a)`)
   */
  externalContext?: T

  /**
   * Describes the language processor to be used for evaluation
   */
  executionMethod: ExecutionMethod

  /**
   * Describes the strategy / paradigm to be used for evaluation
   * Examples: lazy, concurrent or non-deterministic
   */
  variant: Variant
}

export interface Frame {
  [name: string]: Value
}

export interface Environment {
  name: string
  tail: Environment | null
  procedureName?: string
  head: Frame
}

export interface Error {
  status: 'error'
}

export interface Finished {
  status: 'finished'
  context: Context
  value: Value
}

export interface Suspended {
  status: 'suspended'
  it: Iterator<Context, Value>
  scheduler: Scheduler
  context: Context
}

export type SuspendedNonDet = Omit<Suspended, 'status'> & { status: 'suspended-non-det' } & {
  value: Value
}

export type Result = Suspended | SuspendedNonDet | Finished | Error

export interface Scheduler {
  run(it: Iterator<Context, Value>, context: Context): Promise<Result>
}
