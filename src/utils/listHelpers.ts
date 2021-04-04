import { EVPair, ExpressibleValue } from '../interpreter/ExpressibleValue'

// For flattening pairs into list
export type ListElement = {
  value: ExpressibleValue
  pair: EVPair
}
export type List = ListElement[]

/**
 * A chain of pairs not ending in the empty list is called an improper list.
 *
 * The last pair of an improper list is the last pair down the chain whose tail is not a pair and is not the empty list.
 * The proper part of an improper list is all the pairs proceeding the last pair.
 * E.g., (cons 1 (cons 2 (cons 3 4))) is a improper list whose proper part is (1 2) and the last pair is (3 . 4)
 */
export type ImproperList = {
  properPart: List
  lastPair: EVPair
}

/**
 * If the argument is a list, return a List object, backed by an array of the list elements.
 * Otherwise, return an ImproperList object containing the proper part and the last pair.
 */
export function flattenPairToList(
  pair: EVPair
): { type: 'List'; value: List } | { type: 'ImproperList'; value: ImproperList } {
  const seen: Set<EVPair> = new Set()
  const flattened: List = []
  while (true) {
    const tail = pair.tail
    if (tail.type === 'EVEmptyList') {
      flattened.push({ value: pair.head, pair })
      return { type: 'List', value: flattened }
    } else if (tail.type === 'EVPair') {
      seen.add(pair)
      if (seen.has(tail)) {
        return {
          type: 'ImproperList',
          value: { properPart: flattened, lastPair: pair }
        }
      }
      flattened.push({ value: pair.head, pair })
      pair = tail
    } else {
      return { type: 'ImproperList', value: { properPart: flattened, lastPair: pair } }
    }
  }
}
