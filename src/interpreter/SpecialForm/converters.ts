import * as errors from '../../errors/errors'
import { SchemeIdentifier, SchemeList } from '../../lang/scheme'
import { Context } from '../../types'
import { handleRuntimeError } from '../util'
import {
  DefineForm,
  IfForm,
  LambdaForm,
  LetBinding,
  LetForm,
  LetRecForm,
  LetStarForm,
  QuasiquoteForm,
  QuoteForm,
  SetBangForm,
  SpecialForm,
  UnquoteForm,
  UnquoteSplicingForm
} from './definitions'

// Convert a SchemeList to a special form, if the list has appropriate format.
// If no conversion is possible, return undefined
export const listToSpecialForm = (
  tag: string,
  list: SchemeList,
  context: Context
): SpecialForm | undefined => {
  switch (tag) {
    case 'define':
      return listToDefine(list, context)
    case 'lambda':
      return listToLambda(list, context)
    case 'set!':
      return listToSetBang(list, context)
    case 'if':
      return listToIf(list, context)
    case 'let':
    case 'let*':
    case 'letrec':
      return listToLet(tag, list, context)
    case 'quote':
    case 'quasiquote':
    case 'unquote':
    case 'unquote-splicing':
      return listToQuote(tag, list, context)
    default:
      return undefined
  }
}

const listToDefine = (list: SchemeList, context: Context): DefineForm | undefined => {
  if (list.elements.length < 3) {
    return handleRuntimeError(context, new errors.DefineSyntaxError(list))
  }

  if (list.elements[1].type === 'List') {
    if (list.elements[1].elements.length < 1) {
      return handleRuntimeError(context, new errors.DefineSyntaxError(list))
    }

    const argsList: SchemeIdentifier[] = list.elements[1].elements.map(element => {
      if (element.type !== 'Identifier') {
        return handleRuntimeError(context, new errors.DefineSyntaxError(list))
      }
      return element
    })

    return {
      tag: 'define',
      variant: 'procedure',
      name: argsList[0],
      parameters: argsList.slice(1),
      argumentPassingStyle: {
        style: 'fixed-args',
        numParams: argsList.length - 1
      },
      body: list.elements.slice(2)
    }
  } else if (list.elements[1].type === 'Identifier') {
    if (list.elements.length !== 3) {
      return handleRuntimeError(context, new errors.DefineSyntaxError(list))
    }
    const identifier = list.elements[1]
    return {
      tag: 'define',
      variant: 'basic',
      name: identifier,
      value: list.elements[2]
    }
  } else {
    return handleRuntimeError(context, new errors.DefineSyntaxError(list))
  }
}

const listToLambda = (list: SchemeList, context: Context): LambdaForm | undefined => {
  // TODO: varargs?
  if (list.elements.length <= 2 || list.elements[1].type !== 'List') {
    return handleRuntimeError(context, new errors.LambdaSyntaxError(list))
  }
  const parameters: SchemeIdentifier[] = []
  list.elements[1].elements.forEach(element => {
    if (element.type === 'Identifier') {
      return parameters.push(element)
    } else {
      return handleRuntimeError(context, new errors.LambdaSyntaxError(list))
    }
  })
  return {
    tag: 'lambda',
    parameters,
    body: list.elements.slice(2),
    argumentPassingStyle: {
      style: 'fixed-args',
      numParams: parameters.length
    }
  }
}

const listToSetBang = (list: SchemeList, context: Context): SetBangForm | undefined => {
  if (list.elements.length !== 3 || list.elements[1].type !== 'Identifier') {
    return handleRuntimeError(context, new errors.SetSyntaxError(list))
  }
  return {
    tag: 'set!',
    name: list.elements[1],
    value: list.elements[2]
  }
}

const listToIf = (list: SchemeList, context: Context): IfForm | undefined => {
  if (list.elements.length < 3 || list.elements.length > 4) {
    return handleRuntimeError(context, new errors.IfSyntaxError(list))
  }
  return {
    tag: 'if',
    test: list.elements[1],
    consequent: list.elements[2],
    alternative: list.elements[3]
  }
}

const listToLet = (
  tag: 'let' | 'let*' | 'letrec',
  list: SchemeList,
  context: Context
): LetForm | LetStarForm | LetRecForm | undefined => {
  if (list.elements.length < 3 || list.elements[1].type !== 'List') {
    return handleRuntimeError(context, new errors.LetSyntaxError(list))
  }
  const bindings: LetBinding[] = list.elements[1].elements.map(pair => {
    if (
      pair.type !== 'List' ||
      pair.elements.length !== 2 ||
      pair.elements[0].type !== 'Identifier'
    ) {
      return handleRuntimeError(context, new errors.LetSyntaxError(list))
    }

    return {
      name: pair.elements[0],
      value: pair.elements[1]
    }
  })

  return {
    tag,
    bindings,
    body: {
      type: 'Sequence',
      expressions: list.elements.slice(2),
      loc: list.loc
    }
  }
}

const listToQuote = (
  tag: 'quote' | 'quasiquote' | 'unquote' | 'unquote-splicing',
  list: SchemeList,
  context: Context
): QuoteForm | QuasiquoteForm | UnquoteForm | UnquoteSplicingForm | undefined => {
  if (list.elements.length !== 2) {
    return handleRuntimeError(context, new errors.QuoteSyntaxError(tag, list))
  }
  return {
    tag,
    expression: list.elements[1]
  }
}
