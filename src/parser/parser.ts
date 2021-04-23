/* tslint:disable:max-classes-per-file */
import {
  ANTLRErrorListener,
  ANTLRInputStream,
  CommonTokenStream,
  ParserRuleContext,
  Recognizer
} from 'antlr4ts'
import { ErrorNode } from 'antlr4ts/tree/ErrorNode'
import { ParseTree } from 'antlr4ts/tree/ParseTree'
import { RuleNode } from 'antlr4ts/tree/RuleNode'
import { TerminalNode } from 'antlr4ts/tree/TerminalNode'
import { UNKNOWN_LOCATION } from '../constants'
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
  StringContext,
  UnquoteContext,
  UnquoteSplicingContext
} from '../lang/SchemeParser'
import { SchemeVisitor } from '../lang/SchemeVisitor'
import { SchemeProgram, SyntaxDottedList, SyntaxList, SyntaxNode } from '../lang/syntax'
import { makeSymbol, SBool, SNumber, SourceLocation, SString, SSymbol } from '../sExpression'
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

class ExpressionGenerator implements SchemeVisitor<SyntaxNode> {
  visitProgram?: ((ctx: ProgramContext) => SyntaxNode) | undefined

  visitChildren(node: RuleNode): SyntaxNode {
    throw new FatalSyntaxError(UNKNOWN_LOCATION, `invalid syntax ${node.text}`)
  }

  visitList(ctx: ListContext): SyntaxList {
    return {
      type: 'list',
      elements: ctx.expression().map(ex => ex.accept(this)),
      loc: contextToLocation(ctx)
    }
  }

  visitDottedList(ctx: DottedListContext): SyntaxDottedList {
    const expressions = ctx.expression()
    const preExpressions = expressions.slice(0, -1)
    const postExpression = expressions[expressions.length - 1]
    return {
      type: 'dotted list',
      pre: preExpressions.map(ex => ex.accept(this)),
      post: postExpression.accept(this),
      loc: contextToLocation(ctx)
    }
  }

  visitQuote(ctx: QuoteContext): SyntaxList {
    return {
      type: 'list',
      elements: [makeSymbol('quote', true, contextToLocation(ctx)), ctx.expression().accept(this)],
      loc: contextToLocation(ctx)
    }
  }

  visitQuasiquote(ctx: QuasiquoteContext): SyntaxList {
    return {
      type: 'list',
      elements: [
        makeSymbol('quasiquote', true, contextToLocation(ctx)),
        ctx.expression().accept(this)
      ],
      loc: contextToLocation(ctx)
    }
  }

  visitUnquote(ctx: UnquoteContext): SyntaxList {
    return {
      type: 'list',
      elements: [
        makeSymbol('unquote', true, contextToLocation(ctx)),
        ctx.expression().accept(this)
      ],
      loc: contextToLocation(ctx)
    }
  }

  visitUnquoteSplicing(ctx: UnquoteSplicingContext): SyntaxList {
    return {
      type: 'list',
      elements: [
        makeSymbol('unquote-splicing', true, contextToLocation(ctx)),
        ctx.expression().accept(this)
      ],
      loc: contextToLocation(ctx)
    }
  }

  visitString(ctx: StringContext): SString {
    return {
      type: 'string',
      // Remove the quotation marks
      value: ctx.text.slice(1, -1),
      loc: contextToLocation(ctx)
    }
  }

  visitNumber(ctx: NumberContext): SNumber {
    return {
      type: 'number',
      value: Number(ctx.text),
      loc: contextToLocation(ctx)
    }
  }

  visitBool(ctx: BoolContext): SBool {
    console.assert(ctx.text === '#t' || ctx.text === '#f')

    return {
      type: 'boolean',
      value: ctx.text === '#t' ? true : false,
      loc: contextToLocation(ctx)
    }
  }

  visitIdentifier(ctx: IdentifierContext): SSymbol {
    return {
      type: 'symbol',
      isFromSource: true,
      value: ctx.text.toLowerCase(),
      loc: contextToLocation(ctx)
    }
  }

  visitExpression?: ((ctx: ExpressionContext) => SyntaxNode) | undefined

  visitTerminal(node: TerminalNode): SyntaxNode {
    return node.accept(this)
  }

  visit(tree: ParseTree): SyntaxNode {
    return tree.accept(this)
  }

  visitErrorNode(node: ErrorNode): SyntaxNode {
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

class ThrowingErrorListener implements ANTLRErrorListener<any> {
  private static formatErrorMessage(msg: string): string {
    if (msg.includes("mismatched input '<EOF>'")) {
      return 'unexpected EOF'
    } else if (msg.includes('no viable alternative')) {
      return msg.replace('no viable alternative', 'syntax error')
    } else {
      return msg
    }
  }

  public syntaxError<T>(
    _recognizer: Recognizer<T, any>,
    _offendingSymbol: T,
    line: number,
    charPositionInLine: number,
    msg: string
  ): void {
    throw new FatalSyntaxError(
      {
        start: {
          line,
          column: charPositionInLine + 1
        },
        end: {
          line,
          column: charPositionInLine + 1
        }
      },
      ThrowingErrorListener.formatErrorMessage(msg)
    )
  }
}

function convertSource(program: ProgramContext): SchemeProgram {
  const generator = new ExpressionGenerator()
  return {
    body: program.expression().map(ex => ex.accept(generator))
  }
}

export function parse(source: string, context: Context) {
  const inputStream = new ANTLRInputStream(source)
  const lexer = new SchemeLexer(inputStream)
  const tokenStream = new CommonTokenStream(lexer)
  const parser = new SchemeParser(tokenStream)
  parser.buildParseTree = true

  parser.removeErrorListeners()
  parser.addErrorListener(new ThrowingErrorListener())

  let program: SchemeProgram | undefined
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
