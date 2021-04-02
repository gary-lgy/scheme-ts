import { MAX_LIST_DISPLAY_LENGTH } from '../constants'
import { EVPair, ExpressibleValue } from '../interpreter/ExpressibleValue'
import { NonEmptyList } from '../stdlib/list'
import { Type } from '../types'
import { List, tryConvertToList } from './listHelpers'

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
  const ancestors = new Set<ExpressibleValue | NonEmptyList>()

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
    const maybeList = tryConvertToList(pair)
    if (maybeList) {
      return stringifyList(maybeList, indentLevel)
    }

    ancestors.add(pair)
    const headStr = stringifyValue(pair.head, 0)
    const tailStr = stringifyValue(pair.tail, 0)
    ancestors.delete(pair)

    if (shouldMultiline([headStr, tailStr])) {
      // It's (probably) a source list
      // Don't increase indent on second element
      // so long lists don't look like crap
      return `(${indentify(indentString.repeat(indentLevel + 1), headStr).substring(
        indentString.length
      )} .
${indentify(indentString.repeat(indentLevel), tailStr)})`
    } else {
      return `(${headStr} . ${tailStr})`
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
      ancestors.delete(x.pair)
      return value
    })

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

  // const stringifyList = (list: NonEmptyList, indentLevel: number) => {
  //   const prefix = '('
  //   const suffix = ')'
  //   const prefixIndented = prefix + indentString.substring(prefix.length)
  //   const suffixIndented = suffix
  //   const flattened = flattenListToArray(list)

  //   ancestors.add(list)
  //   const valueStrs = flattened.map(x => stringifyValue(x, 0))
  //   ancestors.delete(list)

  //   if (shouldMultiline(valueStrs)) {
  //     // indent second element onwards to match with first element
  //     return `${prefixIndented}${indentify(
  //       indentString.repeat(indentLevel) + ' '.repeat(prefixIndented.length),
  //       valueStrs.join(' \n')
  //     ).substring(prefixIndented.length)}${suffixIndented}`
  //   } else {
  //     return `${prefix}${valueStrs.join(' ')}${suffix}`
  //   }
  // }

  // Convert any proper lists expressed as pairs to NonEmptyList for convenience
  // value = normaliseList(value)

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
        return '[Procedure]'
    }
  }

  return stringifyValue(value, 0)
}

export function typeToString(type: Type): string {
  return niceTypeToString(type)
}

function niceTypeToString(type: Type, nameMap = { _next: 0 }): string {
  function curriedTypeToString(t: Type) {
    return niceTypeToString(t, nameMap)
  }

  switch (type.kind) {
    case 'primitive':
      return type.name
    case 'variable':
      if (type.constraint && type.constraint !== 'none') {
        return type.constraint
      }
      if (!(type.name in nameMap)) {
        // type name is not in map, so add it
        nameMap[type.name] = 'T' + nameMap._next++
      }
      return nameMap[type.name]
    case 'list':
      return `List<${curriedTypeToString(type.elementType)}>`
    case 'array':
      return `Array<${curriedTypeToString(type.elementType)}>`
    case 'pair':
      const headType = curriedTypeToString(type.headType)
      // convert [T1 , List<T1>] back to List<T1>
      if (
        type.tailType.kind === 'list' &&
        headType === curriedTypeToString(type.tailType.elementType)
      )
        return `List<${headType}>`
      return `[${curriedTypeToString(type.headType)}, ${curriedTypeToString(type.tailType)}]`
    case 'function':
      let parametersString = type.parameterTypes.map(curriedTypeToString).join(', ')
      if (type.parameterTypes.length !== 1 || type.parameterTypes[0].kind === 'function') {
        parametersString = `(${parametersString})`
      }
      return `${parametersString} -> ${curriedTypeToString(type.returnType)}`
    default:
      return 'Unable to infer type'
  }
}
