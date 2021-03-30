import { EVEmptyList, EVPair, ExpressibleValue } from '../interpreter/runtime'

// List type for convenience
export type NonEmptyList = { type: 'NonEmptyList'; value: [ExpressibleValue | NonEmptyList, List] }
type List = {
  type: 'List'
  value: EVEmptyList | NonEmptyList
}

export const flattenListToArray = (list: NonEmptyList) => {
  const output: (ExpressibleValue | NonEmptyList)[] = []
  while (true) {
    output.push(list.value[0])
    if (list.value[1].value.type === 'NonEmptyList') {
      list = list.value[1].value
    } else {
      break
    }
  }
  return output
}

export function normaliseList(xs: ExpressibleValue): ExpressibleValue {
  const visited: Set<ExpressibleValue> = new Set()
  // If a pair in the original structure is a proper list, then it will map to a list
  // If it is not a proper list, then it will map to a ENVonNullPair
  const asListObjects: Map<EVPair, EVPair | NonEmptyList> = new Map() // maps original list nodes to new list nodes

  // Push all nodes in xs into this array
  // Each pair contributes to at least 3 elements in this array: itself, its head, and its tail
  const pairsToProcess: EVPair[] = []
  let i = 0
  if (xs.type === 'EVPair') {
    pairsToProcess.push(xs)
  }
  // we need the guarantee that if there are any proper lists,
  // then the nodes of the proper list appear as a subsequence of this array.
  // We ensure this by always adding the tail after the current node is processed.
  // This means that sometimes, we add the same pair more than once!
  // But because we only process each pair once due to the visited check,
  // and each pair can only contribute to at most 3 items in this array,
  // this array has O(n) elements.
  while (i < pairsToProcess.length) {
    const curXs = pairsToProcess[i]
    i++
    if (visited.has(curXs)) {
      continue
    }
    visited.add(curXs)
    if (curXs.head.type === 'EVPair') {
      pairsToProcess.push(curXs.head)
    }
    if (curXs.tail.type === 'EVPair') {
      pairsToProcess.push(curXs.tail)
    }
  }

  // go through pairs in reverse to ensure the dependencies are resolved first
  // For each pair encountered, if it's a proper list, construct it as a NonEmptyList object
  // If it's not a list object, just make a copy.
  // Either result is inserted into asListObject.
  while (pairsToProcess.length > 0) {
    const curXs = pairsToProcess.pop()!
    const h = curXs.head
    const t = curXs.tail
    if (t.type === 'EVEmptyList') {
      // tail is empty list: curXs is a proper list
      const newList: NonEmptyList = {
        type: 'NonEmptyList',
        value: [h, { type: 'List', value: { type: 'EVEmptyList' } }]
      }
      asListObjects.set(curXs, newList)
      continue
    }
    if (t.type === 'EVPair') {
      const savedTail = asListObjects.get(t)
      if (savedTail && savedTail.type === 'NonEmptyList') {
        // savedTail is a list: curXs is a proper list
        const newList: NonEmptyList = {
          type: 'NonEmptyList',
          value: [h, { type: 'List', value: savedTail }]
        }
        asListObjects.set(curXs, newList)
        continue
      }
    }

    // curXs is not a proper list
    asListObjects.set(curXs, { type: 'EVPair', head: h, tail: t })
  }

  const getListObject = (xs: ExpressibleValue): ExpressibleValue => {
    if (xs.type === 'EVPair') {
      const saved = asListObjects.get(xs)
      if (saved && saved.type === 'NonEmptyList') {
        // XXX: hack: EVPair.head and EVPair.tail must be ExpressibleValue
        // Therefore, cannot embed List into EVPair
        // Just erase the type and check the type before use
        return saved as any
      }
    }
    return xs
  }

  for (const curXs of asListObjects.values()) {
    if (curXs.type === 'EVPair') {
      curXs.head = getListObject(curXs.head)
      curXs.tail = getListObject(curXs.tail)
    } else {
      if (curXs.value[0].type !== 'NonEmptyList') {
        curXs.value[0] = getListObject(curXs.value[0])
      }
    }
  }
  return getListObject(xs)
}
