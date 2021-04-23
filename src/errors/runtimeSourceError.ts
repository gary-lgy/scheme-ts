import { UNKNOWN_LOCATION } from '../constants'
import { SyntaxNode } from '../lang/syntax'
import { SourceLocation } from '../sExpression'
import { ErrorSeverity, ErrorType, SourceError } from '../types'

export class RuntimeSourceError implements SourceError {
  public type = ErrorType.RUNTIME
  public severity = ErrorSeverity.ERROR
  public location: SourceLocation

  constructor(node?: SyntaxNode) {
    this.location = node ? node.loc! : UNKNOWN_LOCATION
  }

  public explain() {
    return ''
  }

  public elaborate() {
    return this.explain()
  }
}
