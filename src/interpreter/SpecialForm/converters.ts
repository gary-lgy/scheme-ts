import * as errors from '../../errors/errors'
import { SyntaxIdentifier, SyntaxList, SyntaxNode } from '../../lang/SchemeSyntax'
import { Context } from '../../types'
import { NamedParameterPassingStyle } from '../procedure'
import { handleRuntimeError, isDefined } from '../util'
import {
  AndForm,
  BeginForm,
  CondClause,
  CondElseClause,
  CondForm,
  DefineForm,
  DefMacroForm,
  IfForm,
  LambdaForm,
  LetBinding,
  LetForm,
  LetRecForm,
  LetStarForm,
  OrForm,
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
  list: SyntaxList,
  context: Context
): SpecialForm | null => {
  switch (tag) {
    // These primitives are present for all sublanguages
    case 'define':
      return listToDefine(list, context)
    case 'lambda':
      return listToLambda(list, context)
    case 'set!':
      return listToSetBang(list, context)
    case 'if':
      return listToIf(list, context)
    case 'quote':
    case 'quasiquote':
    case 'unquote':
    case 'unquote-splicing':
      return listToQuote(tag, list, context)
  }

  if (context.variant === 'macro') {
    // defmacro is only available for the sublanguage with macros
    switch (tag) {
      case 'defmacro':
        return listToDefMacro(list, context)
      default:
        return null
    }
  } else {
    // Hardwired special forms are only available for the sublanguages without macros
    switch (tag) {
      case 'let':
      case 'let*':
      case 'letrec':
        return listToLet(tag, list, context)
      case 'cond':
        return listToCond(list, context)
      case 'begin':
        return listToBegin(list, context)
      case 'and':
        return listToAnd(list)
      case 'or':
        return listToOr(list)
      default:
        return null
    }
  }
}

const listToDefine = (list: SyntaxList, context: Context): DefineForm => {
  if (list.elements.length < 3) {
    return handleRuntimeError(context, new errors.DefineSyntaxError(list))
  }

  if (list.elements[1].type === 'List') {
    if (list.elements[1].elements.length < 1) {
      return handleRuntimeError(context, new errors.DefineSyntaxError(list))
    }

    const argsList: SyntaxIdentifier[] = list.elements[1].elements.map(element => {
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
    const beforeDot: SyntaxIdentifier[] = list.elements[1].pre.map(expr => {
      if (expr.type !== 'Identifier') {
        return handleRuntimeError(context, new errors.DefineSyntaxError(list))
      }
      return expr
    })
    if (list.elements[1].post.type !== 'Identifier') {
      return handleRuntimeError(context, new errors.DefineSyntaxError(list))
    }
    const afterDot: SyntaxIdentifier = list.elements[1].post

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

const listToLambda = (list: SyntaxList, context: Context): LambdaForm => {
  if (list.elements.length <= 2) {
    return handleRuntimeError(context, new errors.LambdaSyntaxError(list))
  }

  const parameterPassingStyle = parseParameters(list.elements[1])
  if (!parameterPassingStyle) {
    return handleRuntimeError(context, new errors.LambdaSyntaxError(list))
  }

  return {
    tag: 'lambda',
    body: list.elements.slice(2),
    argumentPassingStyle: parameterPassingStyle
  }
}

const parseParameters = (list: SyntaxNode): NamedParameterPassingStyle | null => {
  if (list.type === 'List') {
    // Fixed number of arguments
    const parameters: SyntaxIdentifier[] = []
    for (const element of list.elements) {
      if (element.type === 'Identifier') {
        parameters.push(element)
      } else {
        return null
      }
    }
    return {
      style: 'fixed-args',
      numParams: parameters.length,
      parameters
    }
  } else if (list.type === 'DottedList') {
    // variable number of arguments with compulsory arguments
    const compulsoryParameters: SyntaxIdentifier[] = []
    for (const element of list.pre) {
      if (element.type === 'Identifier') {
        compulsoryParameters.push(element)
      } else {
        return null
      }
    }
    if (list.post.type !== 'Identifier') {
      return null
    }
    const restParameters: SyntaxIdentifier = list.post

    return {
      style: 'var-args',
      numCompulsoryParameters: compulsoryParameters.length,
      compulsoryParameters,
      restParameters
    }
  } else if (list.type === 'Identifier') {
    // variable number of arguments without compulsory arguments
    return {
      style: 'var-args',
      numCompulsoryParameters: 0,
      compulsoryParameters: [],
      restParameters: list
    }
  } else {
    return null
  }
}

const listToSetBang = (list: SyntaxList, context: Context): SetBangForm => {
  if (list.elements.length !== 3 || list.elements[1].type !== 'Identifier') {
    return handleRuntimeError(context, new errors.SetSyntaxError(list))
  }
  return {
    tag: 'set!',
    name: list.elements[1],
    value: list.elements[2]
  }
}

const listToIf = (list: SyntaxList, context: Context): IfForm => {
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
  list: SyntaxList,
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
    body: list.elements.slice(2)
  }
}

const listToCond = (list: SyntaxList, context: Context): CondForm => {
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
  clause: SyntaxNode,
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

const listToBegin = (list: SyntaxList, context: Context): BeginForm => {
  if (list.elements.length <= 1) {
    return handleRuntimeError(context, new errors.BeginSyntaxError(list))
  }

  return {
    tag: 'begin',
    body: list.elements.slice(1)
  }
}

const listToQuote = (
  tag: 'quote' | 'quasiquote' | 'unquote' | 'unquote-splicing',
  list: SyntaxList,
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

const listToAnd = (list: SyntaxList): AndForm => {
  return { tag: 'and', arguments: list.elements.slice(1) }
}

const listToOr = (list: SyntaxList): OrForm => {
  return { tag: 'or', arguments: list.elements.slice(1) }
}

const listToDefMacro = (list: SyntaxList, context: Context): DefMacroForm => {
  if (list.elements.length < 4) {
    return handleRuntimeError(context, new errors.DefMacroSyntaxError(list))
  }

  const name = list.elements[1]
  if (name.type !== 'Identifier') {
    return handleRuntimeError(context, new errors.DefMacroSyntaxError(list))
  }

  const argumentPassingStyle = parseParameters(list.elements[2])
  if (!argumentPassingStyle) {
    return handleRuntimeError(context, new errors.DefMacroSyntaxError(list))
  }

  return {
    tag: 'defmacro',
    name,
    parameterPassingStyle: argumentPassingStyle,
    body: list.elements.slice(3)
  }
}
