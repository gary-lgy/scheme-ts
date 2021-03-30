import { EVPair, ExpressibleValue } from '../interpreter/runtime'

// For flattening pairs into list
export type ListElement = {
  value: ExpressibleValue
  pair: EVPair
}
export type List = ListElement[]

export function tryConvertToList(pair: EVPair): List | null {
  const seen: Set<EVPair> = new Set()
  const flattened: List = []
  while (true) {
    if (seen.has(pair)) {
      return null
    }
    seen.add(pair)
    flattened.push({ value: pair.head, pair })

    const tail = pair.tail
    if (tail.type === 'EVEmptyList') {
      return flattened
    }
    if (tail.type !== 'EVPair') {
      return null
    }
    pair = tail
  }
}
