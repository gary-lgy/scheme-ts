/* tslint:disable:max-classes-per-file */
import { ANTLRInputStream, CommonTokenStream, ParserRuleContext } from 'antlr4ts'
import { AbstractParseTreeVisitor } from 'antlr4ts/tree/AbstractParseTreeVisitor'
import { ErrorNode } from 'antlr4ts/tree/ErrorNode'
import { ParseTree } from 'antlr4ts/tree/ParseTree'
import { TerminalNode } from 'antlr4ts/tree/TerminalNode'
import {
  SchemeBoolLiteral,
  SchemeDottedList,
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
  DottedListContext,
  ExpressionContext,
  IdentifierContext,
  ListContext,
  NumberContext,
  ProgramContext,
  QuasiquoteContext,
  QuoteContext,
  SchemeParser,
  SequenceContext,
  StringContext,
  UnquoteContext,
  UnquoteSplicingContext
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

function contextToLocation(ctx: ParserRuleContext): SourceLocation {
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

class ExpressionGenerator
  extends AbstractParseTreeVisitor<SchemeExpression>
  implements SchemeVisitor<SchemeExpression> {
  protected defaultResult(): SchemeExpression {
    // This method is called when there is no mathing parser rule
    // Need to investigate what's the effect of returning an empty program here
    return {
      type: 'Sequence',
      expressions: [],
      loc: {
        start: {
          line: 0,
          column: 0
        },
        end: {
          line: 0,
          column: 0
        }
      }
    }
  }

  visitList(ctx: ListContext): SchemeList {
    return {
      type: 'List',
      elements: ctx.expression().map(ex => ex.accept(this)),
      loc: contextToLocation(ctx)
    }
  }

  visitDottedList(ctx: DottedListContext): SchemeDottedList {
    const expressions = ctx.expression()
    const preExpressions = expressions.slice(0, -1)
    const postExpression = expressions[expressions.length - 1]
    return {
      type: 'DottedList',
      pre: preExpressions.map(ex => ex.accept(this)),
      post: postExpression.accept(this),
      loc: contextToLocation(ctx)
    }
  }

  visitQuote(ctx: QuoteContext): SchemeList {
    return {
      type: 'List',
      elements: [
        { type: 'Identifier', name: 'quote', loc: contextToLocation(ctx) },
        ctx.expression().accept(this)
      ],
      loc: contextToLocation(ctx)
    }
  }

  visitQuasiquote(ctx: QuasiquoteContext): SchemeList {
    return {
      type: 'List',
      elements: [
        { type: 'Identifier', name: 'quasiquote', loc: contextToLocation(ctx) },
        ctx.expression().accept(this)
      ],
      loc: contextToLocation(ctx)
    }
  }

  visitUnquote(ctx: UnquoteContext): SchemeList {
    return {
      type: 'List',
      elements: [
        { type: 'Identifier', name: 'unquote', loc: contextToLocation(ctx) },
        ctx.expression().accept(this)
      ],
      loc: contextToLocation(ctx)
    }
  }

  visitUnquoteSplicing(ctx: UnquoteSplicingContext): SchemeList {
    return {
      type: 'List',
      elements: [
        { type: 'Identifier', name: 'unquote-splicing', loc: contextToLocation(ctx) },
        ctx.expression().accept(this)
      ],
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

  visitTerminal(node: TerminalNode): SchemeExpression {
    return node.accept(this)
  }

  visit(tree: ParseTree): SchemeExpression {
    return tree.accept(this)
  }

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
