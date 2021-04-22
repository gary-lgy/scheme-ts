import { SourceLocation } from './sExpression'
import { SourceLanguage, Variant } from './types'

export const CUT = 'cut' // cut operator for Source 4.3
export const TRY_AGAIN = 'try_again' // command for Source 4.3
export const GLOBAL = typeof window === 'undefined' ? global : window
export const NATIVE_STORAGE_ID = 'nativeStorage'
export const MODULE_PARAMS_ID = 'moduleParams'
export const MAX_LIST_DISPLAY_LENGTH = 100
export const UNKNOWN_LOCATION: SourceLocation = {
  start: {
    line: -1,
    column: -1
  },
  end: {
    line: -1,
    column: -1
  }
}
export const JSSLANG_PROPERTIES = {
  maxExecTime: 1000,
  factorToIncreaseBy: 10
}

export const sourceLanguages: SourceLanguage[] = [
  { variant: 'base' },
  { variant: 'no-tco' },
  { variant: 'macro' }
]

export const defaultVariant: Variant = 'base'
