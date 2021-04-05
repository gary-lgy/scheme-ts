import { EVProcedure, ExpressibleValue, makeBool } from '../ExpressibleValue'

export const isNumber: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'number?',
  argumentPassingStyle: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: ExpressibleValue[]) => makeBool(args[0].type === 'EVNumber')
}

export const isBool: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'boolean?',
  argumentPassingStyle: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: ExpressibleValue[]) => makeBool(args[0].type === 'EVBool')
}

export const isString: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'string?',
  argumentPassingStyle: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: ExpressibleValue[]) => makeBool(args[0].type === 'EVString')
}

export const isSymbol: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'symbol?',
  argumentPassingStyle: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: ExpressibleValue[]) => makeBool(args[0].type === 'EVSymbol')
}

export const isProcedure: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'procedure?',
  argumentPassingStyle: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: ExpressibleValue[]) => makeBool(args[0].type === 'EVProcedure')
}

export const isPair: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'pair?',
  argumentPassingStyle: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: ExpressibleValue[]) => makeBool(args[0].type === 'EVPair')
}

export const isNull: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'null?',
  argumentPassingStyle: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: ExpressibleValue[]) => makeBool(args[0].type === 'EVEmptyList')
}
