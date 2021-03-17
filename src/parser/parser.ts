/* tslint:disable:max-classes-per-file */
import { ANTLRInputStream, CommonTokenStream } from 'antlr4ts'
import { ErrorNode } from 'antlr4ts/tree/ErrorNode'
import { ParseTree } from 'antlr4ts/tree/ParseTree'
import { RuleNode } from 'antlr4ts/tree/RuleNode'
import { TerminalNode } from 'antlr4ts/tree/TerminalNode'
import { ExpressionContext, NumberContext } from '../lang/CalcParser'
import {
  SchemeBoolLiteral,
  SchemeExpression,
  SchemeIdentifier,
  SchemeList,
  SchemeNumberLiteral,
  SchemeProgram,
  SchemeSequence,
  SchemeStringLiteral,
  SourceLocation
} from '../lang/scheme'
import { SchemeLexer } from '../lang/SchemeLexer'
import {
  BoolContext,
  IdentifierContext,
  ListContext,
  ProgramContext,
  SchemeParser,
  SequenceContext,
  StringContext
} from '../lang/SchemeParser'
import { SchemeVisitor } from '../lang/SchemeVisitor'
import { Context, ErrorSeverity, ErrorType, SourceError } from '../types'

export class FatalSyntaxError implements SourceError {
  public type = ErrorType.SYNTAX
  public severity = ErrorSeverity.ERROR
  public constructor(public location: SourceLocation, public message: string) {}

  public explain() {
    return this.message
  }

  public elaborate() {
    return 'There is a syntax error in your program'
  }
}

export class MissingSemicolonError implements SourceError {
  public type = ErrorType.SYNTAX
  public severity = ErrorSeverity.ERROR
  public constructor(public location: SourceLocation) {}

  public explain() {
    return 'Missing semicolon at the end of statement'
  }

  public elaborate() {
    return 'Every statement must be terminated by a semicolon.'
  }
}

export class TrailingCommaError implements SourceError {
  public type: ErrorType.SYNTAX
  public severity: ErrorSeverity.WARNING
  public constructor(public location: SourceLocation) {}

  public explain() {
    return 'Trailing comma'
  }

  public elaborate() {
    return 'Please remove the trailing comma'
  }
}

function contextToLocation(ctx: ExpressionContext): SourceLocation {
  return {
    start: {
      line: ctx.start.line,
      column: ctx.start.charPositionInLine
    },
    end: {
      line: ctx.stop ? ctx.stop.line : ctx.start.line,
      column: ctx.stop ? ctx.stop.charPositionInLine : ctx.start.charPositionInLine
    }
  }
}

class ExpressionGenerator implements SchemeVisitor<SchemeExpression> {
  visitList(ctx: ListContext): SchemeList {
    return {
      type: 'List',
      elements: ctx.expression().map(ex => ex.accept(this)),
      loc: contextToLocation(ctx)
    }
  }

  visitString(ctx: StringContext): SchemeStringLiteral {
    return {
      type: 'StringLiteral',
      // Remove the quotation marks
      value: ctx.text.slice(1, -1),
      loc: contextToLocation(ctx)
    }
  }

  visitNumber(ctx: NumberContext): SchemeNumberLiteral {
    return {
      type: 'NumberLiteral',
      value: Number(ctx.text),
      loc: contextToLocation(ctx)
    }
  }

  visitBool(ctx: BoolContext): SchemeBoolLiteral {
    console.assert(ctx.text === '#t' || ctx.text === '#f')

    return {
      type: 'BoolLiteral',
      value: ctx.text === '#t' ? true : false,
      loc: contextToLocation(ctx)
    }
  }

  visitIdentifier(ctx: IdentifierContext): SchemeIdentifier {
    return {
      type: 'Identifier',
      name: ctx.text,
      loc: contextToLocation(ctx)
    }
  }

  visitProgram(ctx: ProgramContext): SchemeProgram {
    return {
      type: 'Program',
      body: this.visitSequence(ctx.sequence()),
      loc: contextToLocation(ctx)
    }
  }

  visitSequence(ctx: SequenceContext): SchemeSequence {
    return {
      type: 'Sequence',
      expressions: ctx.expression().map(ex => ex.accept(this)),
      loc: contextToLocation(ctx)
    }
  }

  visitExpression?: ((ctx: ExpressionContext) => SchemeExpression) | undefined

  visitChildren(_: RuleNode): SchemeExpression {
    throw new Error('Method not implemented.')
  }

  visitTerminal(node: TerminalNode): SchemeExpression {
    return node.accept(this)
  }

  visit(tree: ParseTree): SchemeExpression {
    return tree.accept(this)
  }

  // visitChildren(node: RuleNode): es.Expression {
  //   const expressions: es.Expression[] = []
  //   for (let i = 0; i < node.childCount; i++) {
  //     expressions.push(node.getChild(i).accept(this))
  //   }
  //   return {
  //     type: 'SequenceExpression',
  //     expressions
  //   }
  // }

  visitErrorNode(node: ErrorNode): SchemeExpression {
    throw new FatalSyntaxError(
      {
        start: {
          line: node.symbol.line,
          column: node.symbol.charPositionInLine
        },
        end: {
          line: node.symbol.line,
          column: node.symbol.charPositionInLine + 1
        }
      },
      `invalid syntax ${node.text}`
    )
  }
}

function convertSource(expression: ProgramContext): SchemeExpression {
  const generator = new ExpressionGenerator()
  return expression.accept(generator)
}

export function parse(source: string, context: Context) {
  if (context.variant !== 's1') {
    return undefined
  }

  const inputStream = new ANTLRInputStream(source)
  const lexer = new SchemeLexer(inputStream)
  const tokenStream = new CommonTokenStream(lexer)
  const parser = new SchemeParser(tokenStream)
  parser.buildParseTree = true

  let program: SchemeExpression | undefined
  try {
    const tree = parser.program()
    program = convertSource(tree)
  } catch (error) {
    if (error instanceof FatalSyntaxError) {
      context.errors.push(error)
    } else {
      throw error
    }
  }

  const hasErrors = context.errors.find(m => m.severity === ErrorSeverity.ERROR)
  if (program && !hasErrors) {
    return program
  } else {
    return undefined
  }
}
