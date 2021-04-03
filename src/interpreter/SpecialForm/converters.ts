import * as errors from '../../errors/errors'
import { SchemeExpression, SchemeIdentifier, SchemeList } from '../../lang/scheme'
import { Context } from '../../types'
import { handleRuntimeError, isDefined } from '../util'
import {
  BeginForm,
  CondClause,
  CondElseClause,
  CondForm,
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
): SpecialForm | null => {
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
    case 'cond':
      return listToCond(list, context)
    case 'begin':
      return listToBegin(list, context)
    case 'quote':
    case 'quasiquote':
    case 'unquote':
    case 'unquote-splicing':
      return listToQuote(tag, list, context)
    default:
      return null
  }
}

const listToDefine = (list: SchemeList, context: Context): DefineForm => {
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
      argumentPassingStyle: {
        style: 'fixed-args',
        numParams: argsList.length - 1,
        parameters: argsList.slice(1)
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
  } else if (list.elements[1].type === 'DottedList') {
    const beforeDot: SchemeIdentifier[] = list.elements[1].pre.map(expr => {
      if (expr.type !== 'Identifier') {
        return handleRuntimeError(context, new errors.DefineSyntaxError(list))
      }
      return expr
    })
    if (list.elements[1].post.type !== 'Identifier') {
      return handleRuntimeError(context, new errors.DefineSyntaxError(list))
    }
    const afterDot: SchemeIdentifier = list.elements[1].post

    return {
      tag: 'define',
      variant: 'procedure',
      name: beforeDot[0],
      argumentPassingStyle: {
        style: 'var-args',
        numCompulsoryParameters: beforeDot.length - 1,
        compulsoryParameters: beforeDot.slice(1),
        restParameters: afterDot
      },
      body: list.elements.slice(2)
    }
  } else {
    return handleRuntimeError(context, new errors.DefineSyntaxError(list))
  }
}

const listToLambda = (list: SchemeList, context: Context): LambdaForm => {
  if (list.elements.length <= 2) {
    return handleRuntimeError(context, new errors.LambdaSyntaxError(list))
  }

  if (list.elements[1].type === 'List') {
    // Fixed number of arguments
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
      body: list.elements.slice(2),
      argumentPassingStyle: {
        style: 'fixed-args',
        numParams: parameters.length,
        parameters
      }
    }
  } else if (list.elements[1].type === 'DottedList') {
    // variable number of arguments with compulsory arguments
    const compulsoryParameters: SchemeIdentifier[] = list.elements[1].pre.map(element => {
      if (element.type === 'Identifier') {
        return element
      } else {
        return handleRuntimeError(context, new errors.LambdaSyntaxError(list))
      }
    })
    if (list.elements[1].post.type !== 'Identifier') {
      return handleRuntimeError(context, new errors.LambdaSyntaxError(list))
    }
    const restParameters: SchemeIdentifier = list.elements[1].post

    return {
      tag: 'lambda',
      body: list.elements.slice(2),
      argumentPassingStyle: {
        style: 'var-args',
        numCompulsoryParameters: compulsoryParameters.length,
        compulsoryParameters,
        restParameters
      }
    }
  } else if (list.elements[1].type === 'Identifier') {
    // variable number of arguments without compulsory arguments
    return {
      tag: 'lambda',
      body: list.elements.slice(2),
      argumentPassingStyle: {
        style: 'var-args',
        numCompulsoryParameters: 0,
        compulsoryParameters: [],
        restParameters: list.elements[1]
      }
    }
  } else {
    return handleRuntimeError(context, new errors.LambdaSyntaxError(list))
  }
}

const listToSetBang = (list: SchemeList, context: Context): SetBangForm => {
  if (list.elements.length !== 3 || list.elements[1].type !== 'Identifier') {
    return handleRuntimeError(context, new errors.SetSyntaxError(list))
  }
  return {
    tag: 'set!',
    name: list.elements[1],
    value: list.elements[2]
  }
}

const listToIf = (list: SchemeList, context: Context): IfForm => {
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
): LetForm | LetStarForm | LetRecForm => {
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

const listToCond = (list: SchemeList, context: Context): CondForm => {
  if (list.elements.length <= 1) {
    return handleRuntimeError(context, new errors.CondSyntaxError(list))
  }

  const clauses: CondClause[] = []
  let elseClause: CondElseClause | undefined = undefined

  const throwSyntaxError = () => handleRuntimeError(context, new errors.CondSyntaxError(list))

  for (let i = 1; i < list.elements.length; i++) {
    const clause = parseCondClause(list.elements[i], context, throwSyntaxError)
    if (clause.type === 'else') {
      if (i !== list.elements.length - 1) {
        throwSyntaxError()
      }
      elseClause = clause
    } else {
      clauses.push(clause)
    }
  }

  return { tag: 'cond', clauses, elseClause }
}

const parseCondClause = (
  clause: SchemeExpression,
  context: Context,
  throwSyntaxError: () => never
): CondClause | CondElseClause => {
  if (clause.type !== 'List' || clause.elements.length < 1) {
    throwSyntaxError()
  }

  const test = clause.elements[0]
  if (test.type === 'Identifier' && test.name === 'else' && !isDefined(context, 'else')) {
    // else clause
    if (clause.elements.length === 1) {
      throwSyntaxError()
    }
    return { type: 'else', body: clause.elements.slice(1), node: clause }
  } else if (clause.elements.length === 1) {
    // basic clause with no body
    return {
      type: 'basic',
      test,
      node: clause,
      body: []
    }
  } else {
    const secondElement = clause.elements[1]
    if (
      secondElement.type === 'Identifier' &&
      secondElement.name === '=>' &&
      !isDefined(context, '=>')
    ) {
      // procedure clause
      if (clause.elements.length !== 3) {
        throwSyntaxError()
      }
      return {
        type: 'procedure',
        test,
        node: clause,
        body: clause.elements[2]
      }
    } else {
      // basic clause with at least one body expression
      return {
        type: 'basic',
        node: clause,
        test,
        body: clause.elements.slice(1)
      }
    }
  }
}

const listToBegin = (list: SchemeList, context: Context): BeginForm => {
  if (list.elements.length <= 1) {
    return handleRuntimeError(context, new errors.BeginSyntaxError(list))
  }

  return {
    tag: 'begin',
    body: {
      type: 'Sequence',
      expressions: list.elements.slice(1),
      loc: list.elements[1].loc
    }
  }
}

const listToQuote = (
  tag: 'quote' | 'quasiquote' | 'unquote' | 'unquote-splicing',
  list: SchemeList,
  context: Context
): QuoteForm | QuasiquoteForm | UnquoteForm | UnquoteSplicingForm => {
  if (list.elements.length !== 2) {
    return handleRuntimeError(context, new errors.QuoteSyntaxError(tag, list))
  }
  return {
    tag,
    expression: list.elements[1]
  }
}
