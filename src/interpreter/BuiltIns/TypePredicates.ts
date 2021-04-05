import { EVProcedure, ExpressibleValue, makeBool } from '../ExpressibleValue'

export const isNumber: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'number?',
  parameterPassingStyle: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: ExpressibleValue[]) => makeBool(args[0].type === 'EVNumber')
}

export const isBool: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'boolean?',
  parameterPassingStyle: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: ExpressibleValue[]) => makeBool(args[0].type === 'EVBool')
}

export const isString: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'string?',
  parameterPassingStyle: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: ExpressibleValue[]) => makeBool(args[0].type === 'EVString')
}

export const isSymbol: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'symbol?',
  parameterPassingStyle: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: ExpressibleValue[]) => makeBool(args[0].type === 'EVSymbol')
}

export const isProcedure: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'procedure?',
  parameterPassingStyle: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: ExpressibleValue[]) => makeBool(args[0].type === 'EVProcedure')
}

export const isPair: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'pair?',
  parameterPassingStyle: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: ExpressibleValue[]) => makeBool(args[0].type === 'EVPair')
}

export const isNull: EVProcedure = {
  type: 'EVProcedure',
  variant: 'BuiltInProcedure',
  name: 'null?',
  parameterPassingStyle: {
    style: 'fixed-args',
    numParams: 1
  },
  body: (args: ExpressibleValue[]) => makeBool(args[0].type === 'EVEmptyList')
}
