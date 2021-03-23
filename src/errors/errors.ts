/* tslint:disable: max-classes-per-file */
/* tslint:disable:max-line-length */
import * as es from 'estree'
import { SchemeExpression } from '../lang/scheme'
import { ErrorSeverity, ErrorType, SourceError, Value } from '../types'
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

export class ModuleNotFound extends RuntimeSourceError {
  constructor(public moduleName: string, node?: SchemeExpression) {
    super(node)
  }

  public explain() {
    return `Module "${this.moduleName}" not found.`
  }

  public elaborate() {
    return `
      You should check your Internet connection, and ensure you have used the correct module path.
    `
  }
}

export class ModuleInternalError extends RuntimeSourceError {
  constructor(public moduleName: string, node?: SchemeExpression) {
    super(node)
  }

  public explain() {
    return `Error(s) occured when executing the module "${this.moduleName}".`
  }

  public elaborate() {
    return `
      You may need to contact with the author for this module to fix this error.
    `
  }
}

export class ExceptionError implements SourceError {
  public type = ErrorType.RUNTIME
  public severity = ErrorSeverity.ERROR

  constructor(public error: Error, public location: es.SourceLocation) {}

  public explain() {
    return this.error.toString()
  }

  public elaborate() {
    return 'TODO'
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
  constructor(private callee: Value, private node: SchemeExpression) {
    super(node)
  }

  public explain() {
    return `Calling non-function value ${stringify(this.callee)}.`
  }

  public elaborate() {
    const calleeVal = this.callee
    const calleeStr = stringify(calleeVal)
    const argStr = ''

    // const callArgs = (this.node as es.CallExpression).arguments

    // To temporarily silence TS warnings
    this.node
    // argStr = callArgs.map(generate).join(', ')

    const elabStr = `Because ${calleeStr} is not a function, you cannot run ${calleeStr}(${argStr}).`
    const multStr = `If you were planning to perform multiplication by ${calleeStr}, you need to use the * operator.`

    if (Number.isFinite(calleeVal)) {
      return `${elabStr} ${multStr}`
    } else {
      return elabStr
    }
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

export class ConstAssignment extends RuntimeSourceError {
  constructor(node: SchemeExpression, private name: string) {
    super(node)
  }

  public explain() {
    return `Cannot assign new value to constant ${this.name}.`
  }

  public elaborate() {
    return `As ${this.name} was declared as a constant, its value cannot be changed. You will have to declare a new variable.`
  }
}

export class GetPropertyError extends RuntimeSourceError {
  constructor(node: SchemeExpression, private obj: Value, private prop: string) {
    super(node)
  }

  public explain() {
    return `Cannot read property ${this.prop} of ${stringify(this.obj)}.`
  }

  public elaborate() {
    return 'TODO'
  }
}

export class GetInheritedPropertyError extends RuntimeSourceError {
  public type = ErrorType.RUNTIME
  public severity = ErrorSeverity.ERROR
  public location: es.SourceLocation

  constructor(node: SchemeExpression, private obj: Value, private prop: string) {
    super(node)
    this.location = node.loc!
  }

  public explain() {
    return `Cannot read inherited property ${this.prop} of ${stringify(this.obj)}.`
  }

  public elaborate() {
    return 'TODO'
  }
}

export class SetPropertyError extends RuntimeSourceError {
  constructor(node: SchemeExpression, private obj: Value, private prop: string) {
    super(node)
  }

  public explain() {
    return `Cannot assign property ${this.prop} of ${stringify(this.obj)}.`
  }

  public elaborate() {
    return 'TODO'
  }
}
