import { SchemeSequence } from '../lang/scheme'
import { Environment } from '../types'

export type EVNumber = {
  type: 'EVNumber'
  value: number
}

export type EVString = {
  type: 'EVString'
  value: string
}

export type EVBool = {
  type: 'EVBool'
  value: boolean
}

export type EVClosure = {
  type: 'EVProcedure'
  value: {
    body: SchemeSequence
    environment: Environment
  }
}

export type EVList = {
  type: 'EVList'
  value: ExpressibleValue[]
}

export type ExpressibleValue = EVNumber | EVString | EVBool | EVClosure | EVList

export interface FrameBinding {
  value: ExpressibleValue
}

export class Frame {
  private bindings: Map<string, FrameBinding> = new Map()

  get(name: string): FrameBinding | undefined {
    return this.bindings.get(name)
  }

  set(name: string, newBinding: FrameBinding) {
    this.bindings.set(name, newBinding)
  }
}
