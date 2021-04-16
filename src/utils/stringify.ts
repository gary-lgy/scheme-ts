import { MAX_LIST_DISPLAY_LENGTH } from '../constants'
import { CallSignature, NamedCallSignature } from '../interpreter/procedure'
import { Pair, Value } from '../interpreter/Value'
import { flattenPairToList, ImproperList, List } from './listHelpers'

function makeIndent(indent: number | string): string {
  if (typeof indent === 'number') {
    if (indent > 10) {
      indent = 10
    }
    return ' '.repeat(indent)
  } else {
    if (indent.length > 10) {
      indent = indent.substring(0, 10)
    }
    return indent
  }
}

function indentify(indent: string, s: string): string {
  return s
    .split('\n')
    .map(v => indent + v)
    .join('\n')
}

export const stringify = (
  value: Value,
  indent: number | string = 2,
  splitlineThreshold = 80
): string => {
  // Used to check if there are any cyclic structures
  const ancestors = new Set<Value>()

  // Precompute useful strings
  const indentString = makeIndent(indent)

  // Util functions

  // Determines if the array/object containing these (stringified) values
  // should be multiline and indented or oneline
  const shouldMultiline = (valueStrs: string[]) =>
    indentString !== '' &&
    (valueStrs.join(', ').length > splitlineThreshold || valueStrs.some(s => s.includes('\n')))

  // Stringify functions
  // The real one is stringifyValue

  const stringifyPair = (pair: Pair, indentLevel: number) => {
    const list = flattenPairToList(pair)
    if (list.type === 'List') {
      return stringifyList(list.value, indentLevel)
    } else {
      return stringifyImproperList(list.value, indentLevel)
    }
  }

  const stringifyList = (list: List, indentLevel: number) => {
    const prefix = '('
    const suffix = ')'
    const prefixIndented = prefix + indentString.substring(prefix.length)
    const suffixIndented = suffix

    const valueStrs = list.map(x => {
      ancestors.add(x.pair)
      const value = stringifyValue(x.value, 0)
      return value
    })

    list.forEach(x => ancestors.delete(x.pair))

    if (shouldMultiline(valueStrs)) {
      // indent second element onwards to match with first element
      return `${prefixIndented}${indentify(
        indentString.repeat(indentLevel) + ' '.repeat(prefixIndented.length),
        valueStrs.join(' \n')
      ).substring(prefixIndented.length)}${suffixIndented}`
    } else {
      return `${prefix}${valueStrs.join(' ')}${suffix}`
    }
  }

  const stringifyImproperList = (list: ImproperList, indentLevel: number) => {
    const prefix = '('
    const suffix = ')'
    const prefixIndented = prefix + indentString.substring(prefix.length)
    const suffixIndented = suffix

    const preStrs = list.properPart.map(x => {
      ancestors.add(x.pair)
      return stringifyValue(x.value, 0)
    })
    ancestors.add(list.lastPair)
    const postStrs = [stringifyValue(list.lastPair.head), '.', stringifyValue(list.lastPair.tail)]
    const valueStrs = preStrs.concat(postStrs)

    list.properPart.forEach(x => ancestors.delete(x.pair))
    ancestors.delete(list.lastPair)

    if (shouldMultiline(valueStrs)) {
      // indent second element onwards to match with first element
      return `${prefixIndented}${indentify(
        indentString.repeat(indentLevel) + ' '.repeat(prefixIndented.length),
        valueStrs.join(' \n')
      ).substring(prefixIndented.length)}${suffixIndented}`
    } else {
      return `${prefix}${valueStrs.join(' ')}${suffix}`
    }
  }

  const stringifyValue = (v: Value, indentLevel: number = 0): string => {
    if (ancestors.has(v)) {
      return '...<circular>'
    } else if (ancestors.size > MAX_LIST_DISPLAY_LENGTH) {
      return '...<truncated>'
    }

    switch (v.type) {
      case 'boolean':
        return v.value ? '#t' : '#f'
      case 'number':
        return `${v.value}`
      case 'empty list':
        return '()'
      case 'string':
        return `"${v.value}"`
      case 'symbol':
        return v.value
      case 'pair':
        return stringifyPair(v, indentLevel)
      case 'procedure':
        const procedureVariant =
          v.variant === 'CompoundProcedure' ? 'compound procedure' : 'built-in procedure'
        return `[${procedureVariant} ${stringifyCallSignature(v.name, v.callSignature)}]`
      case 'macro':
        return `[macro '${v.name}']`
      case 'TailCall':
        throw new Error('stringify should not see a TailCall value')
    }
  }

  return stringifyValue(value, 0)
}

export const stringifyCallSignature = (
  name: string,
  callSignature: CallSignature | NamedCallSignature
): string => {
  let argNames: string[] = []
  if (callSignature.style === 'fixed-args') {
    if ('parameters' in callSignature) {
      argNames = callSignature.parameters.map(param => param.value)
    } else {
      argNames = Array.from({ length: callSignature.numParams }).map((_, index) => `arg${index}`)
    }
  } else {
    if ('compulsoryParameters' in callSignature) {
      argNames = callSignature.compulsoryParameters
        .map(param => param.value)
        .concat(['...' + callSignature.restParameters.value])
    } else {
      argNames = Array.from({ length: callSignature.numCompulsoryParameters })
        .map((_, index) => `arg${index}`)
        .concat(['...rest-args'])
    }
  }

  return `(${[name, ...argNames].join(' ')})`
}
