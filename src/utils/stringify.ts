import { MAX_LIST_DISPLAY_LENGTH } from '../constants'
import { EVPair, ExpressibleValue } from '../interpreter/ExpressibleValue'
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
  value: ExpressibleValue,
  indent: number | string = 2,
  splitlineThreshold = 80
): string => {
  // Used to check if there are any cyclic structures
  const ancestors = new Set<ExpressibleValue>()

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

  const stringifyPair = (pair: EVPair, indentLevel: number) => {
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

  const stringifyValue = (v: ExpressibleValue, indentLevel: number = 0): string => {
    if (ancestors.has(v)) {
      return '...<circular>'
    } else if (ancestors.size > MAX_LIST_DISPLAY_LENGTH) {
      return '...<truncated>'
    }

    switch (v.type) {
      case 'EVBool':
        return v.value ? '#t' : '#f'
      case 'EVNumber':
        return `${v.value}`
      case 'EVEmptyList':
        return '()'
      case 'EVString':
        return `"${v.value}"`
      case 'EVSymbol':
        return v.value
      case 'EVPair':
        return stringifyPair(v, indentLevel)
      case 'EVProcedure':
        if (v.variant === 'BuiltInProcedure') {
          return `[built-in procedure '${v.name}']`
        } else {
          return `[compound procedure '${v.name}']`
        }
      case 'EVMacro':
        return `[macro '${v.name}']`
      case 'TailCall':
        throw new Error('stringify should not see a TailCall value')
    }
  }

  return stringifyValue(value, 0)
}
