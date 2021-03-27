/* tslint:disable: max-classes-per-file */
import { ExpressibleValue } from '../interpreter/runtime'
import { SchemeExpression } from '../lang/scheme'
import { stringify } from '../utils/stringify'
import { RuntimeSourceError } from './runtimeSourceError'

export class DefineSyntaxError extends RuntimeSourceError {
  constructor(node: SchemeExpression) {
    super(node)
  }

  public explain() {
    return "Syntax for `define' is incorrect. Please use `(define variable value)'"
  }
}

export class LambdaSyntaxError extends RuntimeSourceError {
  constructor(node: SchemeExpression) {
    super(node)
  }

  public explain() {
    return "Syntax for `lambda' is incorrect. Please use `(lambda (arg1 arg2 ...) body)'"
  }
}

export class SetSyntaxError extends RuntimeSourceError {
  constructor(node: SchemeExpression) {
    super(node)
  }

  public explain() {
    return "Syntax for `set!' is incorrect. Please use `(set! variable value) body)'"
  }
}

export class IfSyntaxError extends RuntimeSourceError {
  constructor(node: SchemeExpression) {
    super(node)
  }

  public explain() {
    return "Syntax for `if' is incorrect. Please use `(if (test) (consequent))' or `(if (test) (consequent) (alternative))'"
  }
}

type QuoteType = 'quote' | 'quasiquote' | 'unquote' | 'unquote-splicing'
const quoteTypeToShorthand = (type: QuoteType): string => {
  switch (type) {
    case 'quote':
      return "'"
    case 'quasiquote':
      return '`'
    case 'unquote':
      return ','
    case 'unquote-splicing':
      return ',@'
  }
}
export class QuoteSyntaxError extends RuntimeSourceError {
  constructor(public quoteType: QuoteType, node: SchemeExpression) {
    super(node)
  }

  public explain() {
    return `Syntax for \`${this.quoteType}' is incorrect. Please use (${
      this.quoteType
    } expression) or ${quoteTypeToShorthand(this.quoteType)}expression`
  }
}

export class BuiltinProcedureError extends RuntimeSourceError {
  constructor(public cause: Error, node?: SchemeExpression) {
    super(node)
  }
  public explain() {
    return this.cause.message
  }
}

export class UnexpectedQuotationError extends RuntimeSourceError {
  constructor(node: SchemeExpression) {
    super(node)
  }

  public explain() {
    return `Unexpected quotation`
  }
}

export class UnquoteInWrongContext extends RuntimeSourceError {
  constructor(node: SchemeExpression) {
    super(node)
  }

  public explain() {
    return `\`unquote' and \`unquote-splicing' can only appear within quasiquotations`
  }
}

export class UnquoteSplicingEvaluatedToNonList extends RuntimeSourceError {
  constructor(public result: ExpressibleValue, node: SchemeExpression) {
    super(node)
  }

  public explain() {
    return `\`unquote-splicing' should evaluate to a list, but evaluated to ${this.result.type}`
  }
}

export class UnquoteSplicingInNonListContext extends RuntimeSourceError {
  constructor(node: SchemeExpression) {
    super(node)
  }

  public explain() {
    return `\`unquote-splicing' can only appear within a list`
  }
}

export class UnreachableCodeReached extends RuntimeSourceError {
  constructor(public message: string) {
    super()
  }

  public explain() {
    return `Unreachable code reached: ${this.message}`
  }
}

export class MaximumStackLimitExceeded extends RuntimeSourceError {
  public static MAX_CALLS_TO_SHOW = 3

  constructor(node: SchemeExpression, private calls: string[]) {
    super(node)
  }

  public explain() {
    return 'Maximum call stack size exceeded\n  ' + this.calls.join('  ')
  }
}

export class CallingNonFunctionValue extends RuntimeSourceError {
  constructor(private callee: ExpressibleValue, public node: SchemeExpression) {
    super(node)
  }

  public explain() {
    return `Calling non-function value ${stringify(this.callee)}.`
  }
}

export class UndefinedVariable extends RuntimeSourceError {
  constructor(public name: string, node: SchemeExpression) {
    super(node)
  }

  public explain() {
    return `Name ${this.name} not declared.`
  }

  public elaborate() {
    return `Before you can read the value of ${this.name}, you need to declare it as a variable or a constant. You can do this using the let or const keywords.`
  }
}

export class UnassignedVariable extends RuntimeSourceError {
  constructor(public name: string, node: SchemeExpression) {
    super(node)
  }

  public explain() {
    return `Name ${this.name} declared later in current scope but not yet assigned`
  }

  public elaborate() {
    return `If you're trying to access the value of ${this.name} from an outer scope, please rename the inner ${this.name}. An easy way to avoid this issue in future would be to avoid declaring any variables or constants with the name ${this.name} in the same scope.`
  }
}

export class InvalidNumberOfArguments extends RuntimeSourceError {
  constructor(
    node: SchemeExpression,
    private procedureName: string,
    private expected: number,
    private got: number
  ) {
    super(node)
  }

  public explain() {
    return `${this.procedureName} expected ${this.expected} arguments, but got ${this.got}.`
  }

  public elaborate() {
    const pluralS = this.expected === 1 ? '' : 's'
    return `Try calling procedure ${this.procedureName} again, but with ${this.expected} argument${pluralS} instead.`
  }
}

export class NotEnoughArguments extends RuntimeSourceError {
  constructor(
    node: SchemeExpression,
    private procedureName: string,
    private expected: number,
    private got: number
  ) {
    super(node)
  }

  public explain() {
    return `${this.procedureName} expected at least ${this.expected} arguments, but got ${this.got}.`
  }

  public elaborate() {
    const pluralS = this.expected === 1 ? '' : 's'
    return `Try calling procedure ${this.procedureName} again, but with at least ${this.expected} argument${pluralS} instead.`
  }
}

export class VariableRedeclaration extends RuntimeSourceError {
  constructor(private node: SchemeExpression, private name: string, private writable?: boolean) {
    super(node)
  }

  public explain() {
    return `Redeclaring name ${this.name}.`
  }

  public elaborate() {
    if (this.writable === true) {
      const elabStr = `Since ${this.name} has already been declared, you can assign a value to it without re-declaring.`

      const initStr = ''

      // To temporarily silence ts warning
      this.node

      // if (this.node.type === 'FunctionDeclaration') {
      //   initStr =
      //     '(' + (this.node as es.FunctionDeclaration).params.map(generate).join(',') + ') => {...'
      // } else if (this.node.type === 'VariableDeclaration') {
      //   initStr = generate((this.node as es.VariableDeclaration).declarations[0].init)
      // }

      return `${elabStr} As such, you can just do\n\n\t${this.name} = ${initStr};\n`
    } else if (this.writable === false) {
      return `You will need to declare another variable, as ${this.name} is read-only.`
    } else {
      return ''
    }
  }
}
