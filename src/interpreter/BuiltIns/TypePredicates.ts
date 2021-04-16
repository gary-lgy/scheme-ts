import { makeBool } from '../SExpression'
import { Procedure, Value } from '../Value'

export const isNumber: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'number?',
  callSignature: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: Value[]) => makeBool(args[0].type === 'number')
}

export const isBool: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'boolean?',
  callSignature: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: Value[]) => makeBool(args[0].type === 'boolean')
}

export const isString: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'string?',
  callSignature: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: Value[]) => makeBool(args[0].type === 'string')
}

export const isSymbol: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'symbol?',
  callSignature: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: Value[]) => makeBool(args[0].type === 'symbol')
}

export const isProcedure: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'procedure?',
  callSignature: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: Value[]) => makeBool(args[0].type === 'procedure')
}

export const isPair: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'pair?',
  callSignature: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: Value[]) => makeBool(args[0].type === 'pair')
}

export const isNull: Procedure = {
  type: 'procedure',
  variant: 'BuiltInProcedure',
  name: 'null?',
  callSignature: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: Value[]) => makeBool(args[0].type === 'empty list')
}
