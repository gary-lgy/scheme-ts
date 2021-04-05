// Generated from ./src/lang/Scheme.g4 by ANTLR 4.9.0-SNAPSHOT
// @ts-ignore

import { ATN } from 'antlr4ts/atn/ATN'
import { ATNDeserializer } from 'antlr4ts/atn/ATNDeserializer'
import { ParserATNSimulator } from 'antlr4ts/atn/ParserATNSimulator'
import { FailedPredicateException } from 'antlr4ts/FailedPredicateException'
import * as Utils from 'antlr4ts/misc/Utils'
import { Parser } from 'antlr4ts/Parser'
import { ParserRuleContext } from 'antlr4ts/ParserRuleContext'
import { RecognitionException } from 'antlr4ts/RecognitionException'
import { TokenStream } from 'antlr4ts/TokenStream'
//import { RuleVersion } from "antlr4ts/RuleVersion";
import { TerminalNode } from 'antlr4ts/tree/TerminalNode'
import { Vocabulary } from 'antlr4ts/Vocabulary'
import { VocabularyImpl } from 'antlr4ts/VocabularyImpl'
import { SchemeListener } from './SchemeListener'
import { SchemeVisitor } from './SchemeVisitor'

export class SchemeParser extends Parser {
  public static readonly T__0 = 1
  public static readonly T__1 = 2
  public static readonly T__2 = 3
  public static readonly T__3 = 4
  public static readonly T__4 = 5
  public static readonly T__5 = 6
  public static readonly T__6 = 7
  public static readonly STRING = 8
  public static readonly NUMBER = 9
  public static readonly BOOL = 10
  public static readonly IDENTIFIER = 11
  public static readonly WHITESPACE = 12
  public static readonly COMMENT = 13
  public static readonly RULE_program = 0
  public static readonly RULE_expression = 1
  // tslint:disable:no-trailing-whitespace
  public static readonly ruleNames: string[] = ['program', 'expression']

  private static readonly _LITERAL_NAMES: Array<string | undefined> = [
    undefined,
    "'''",
    "'`'",
    "','",
    "',@'",
    "'('",
    "')'",
    "'.'"
  ]
  private static readonly _SYMBOLIC_NAMES: Array<string | undefined> = [
    undefined,
    undefined,
    undefined,
    undefined,
    undefined,
    undefined,
    undefined,
    undefined,
    'STRING',
    'NUMBER',
    'BOOL',
    'IDENTIFIER',
    'WHITESPACE',
    'COMMENT'
  ]
  public static readonly VOCABULARY: Vocabulary = new VocabularyImpl(
    SchemeParser._LITERAL_NAMES,
    SchemeParser._SYMBOLIC_NAMES,
    []
  )

  // @Override
  // @NotNull
  public get vocabulary(): Vocabulary {
    return SchemeParser.VOCABULARY
  }
  // tslint:enable:no-trailing-whitespace

  // @Override
  public get grammarFileName(): string {
    return 'Scheme.g4'
  }

  // @Override
  public get ruleNames(): string[] {
    return SchemeParser.ruleNames
  }

  // @Override
  public get serializedATN(): string {
    return SchemeParser._serializedATN
  }

  protected createFailedPredicateException(
    predicate?: string,
    message?: string
  ): FailedPredicateException {
    return new FailedPredicateException(this, predicate, message)
  }

  constructor(input: TokenStream) {
    super(input)
    this._interp = new ParserATNSimulator(SchemeParser._ATN, this)
  }
  // @RuleVersion(0)
  public program(): ProgramContext {
    let _localctx: ProgramContext = new ProgramContext(this._ctx, this.state)
    this.enterRule(_localctx, 0, SchemeParser.RULE_program)
    let _la: number
    try {
      this.enterOuterAlt(_localctx, 1)
      {
        this.state = 5
        this._errHandler.sync(this)
        _la = this._input.LA(1)
        do {
          {
            {
              this.state = 4
              this.expression()
            }
          }
          this.state = 7
          this._errHandler.sync(this)
          _la = this._input.LA(1)
        } while (
          (_la & ~0x1f) === 0 &&
          ((1 << _la) &
            ((1 << SchemeParser.T__0) |
              (1 << SchemeParser.T__1) |
              (1 << SchemeParser.T__2) |
              (1 << SchemeParser.T__3) |
              (1 << SchemeParser.T__4) |
              (1 << SchemeParser.STRING) |
              (1 << SchemeParser.NUMBER) |
              (1 << SchemeParser.BOOL) |
              (1 << SchemeParser.IDENTIFIER))) !==
            0
        )
        this.state = 9
        this.match(SchemeParser.EOF)
      }
    } catch (re) {
      if (re instanceof RecognitionException) {
        _localctx.exception = re
        this._errHandler.reportError(this, re)
        this._errHandler.recover(this, re)
      } else {
        throw re
      }
    } finally {
      this.exitRule()
    }
    return _localctx
  }
  // @RuleVersion(0)
  public expression(): ExpressionContext {
    let _localctx: ExpressionContext = new ExpressionContext(this._ctx, this.state)
    this.enterRule(_localctx, 2, SchemeParser.RULE_expression)
    let _la: number
    try {
      this.state = 41
      this._errHandler.sync(this)
      switch (this.interpreter.adaptivePredict(this._input, 3, this._ctx)) {
        case 1:
          _localctx = new StringContext(_localctx)
          this.enterOuterAlt(_localctx, 1)
          {
            this.state = 11
            this.match(SchemeParser.STRING)
          }
          break

        case 2:
          _localctx = new NumberContext(_localctx)
          this.enterOuterAlt(_localctx, 2)
          {
            this.state = 12
            this.match(SchemeParser.NUMBER)
          }
          break

        case 3:
          _localctx = new BoolContext(_localctx)
          this.enterOuterAlt(_localctx, 3)
          {
            this.state = 13
            this.match(SchemeParser.BOOL)
          }
          break

        case 4:
          _localctx = new IdentifierContext(_localctx)
          this.enterOuterAlt(_localctx, 4)
          {
            this.state = 14
            this.match(SchemeParser.IDENTIFIER)
          }
          break

        case 5:
          _localctx = new QuoteContext(_localctx)
          this.enterOuterAlt(_localctx, 5)
          {
            this.state = 15
            this.match(SchemeParser.T__0)
            this.state = 16
            this.expression()
          }
          break

        case 6:
          _localctx = new QuasiquoteContext(_localctx)
          this.enterOuterAlt(_localctx, 6)
          {
            this.state = 17
            this.match(SchemeParser.T__1)
            this.state = 18
            this.expression()
          }
          break

        case 7:
          _localctx = new UnquoteContext(_localctx)
          this.enterOuterAlt(_localctx, 7)
          {
            this.state = 19
            this.match(SchemeParser.T__2)
            this.state = 20
            this.expression()
          }
          break

        case 8:
          _localctx = new UnquoteSplicingContext(_localctx)
          this.enterOuterAlt(_localctx, 8)
          {
            this.state = 21
            this.match(SchemeParser.T__3)
            this.state = 22
            this.expression()
          }
          break

        case 9:
          _localctx = new ListContext(_localctx)
          this.enterOuterAlt(_localctx, 9)
          {
            this.state = 23
            this.match(SchemeParser.T__4)
            this.state = 27
            this._errHandler.sync(this)
            _la = this._input.LA(1)
            while (
              (_la & ~0x1f) === 0 &&
              ((1 << _la) &
                ((1 << SchemeParser.T__0) |
                  (1 << SchemeParser.T__1) |
                  (1 << SchemeParser.T__2) |
                  (1 << SchemeParser.T__3) |
                  (1 << SchemeParser.T__4) |
                  (1 << SchemeParser.STRING) |
                  (1 << SchemeParser.NUMBER) |
                  (1 << SchemeParser.BOOL) |
                  (1 << SchemeParser.IDENTIFIER))) !==
                0
            ) {
              {
                {
                  this.state = 24
                  ;(_localctx as ListContext)._elements = this.expression()
                }
              }
              this.state = 29
              this._errHandler.sync(this)
              _la = this._input.LA(1)
            }
            this.state = 30
            this.match(SchemeParser.T__5)
          }
          break

        case 10:
          _localctx = new DottedListContext(_localctx)
          this.enterOuterAlt(_localctx, 10)
          {
            this.state = 31
            this.match(SchemeParser.T__4)
            this.state = 33
            this._errHandler.sync(this)
            _la = this._input.LA(1)
            do {
              {
                {
                  this.state = 32
                  ;(_localctx as DottedListContext)._pre = this.expression()
                }
              }
              this.state = 35
              this._errHandler.sync(this)
              _la = this._input.LA(1)
            } while (
              (_la & ~0x1f) === 0 &&
              ((1 << _la) &
                ((1 << SchemeParser.T__0) |
                  (1 << SchemeParser.T__1) |
                  (1 << SchemeParser.T__2) |
                  (1 << SchemeParser.T__3) |
                  (1 << SchemeParser.T__4) |
                  (1 << SchemeParser.STRING) |
                  (1 << SchemeParser.NUMBER) |
                  (1 << SchemeParser.BOOL) |
                  (1 << SchemeParser.IDENTIFIER))) !==
                0
            )
            this.state = 37
            this.match(SchemeParser.T__6)
            this.state = 38
            ;(_localctx as DottedListContext)._post = this.expression()
            this.state = 39
            this.match(SchemeParser.T__5)
          }
          break
      }
    } catch (re) {
      if (re instanceof RecognitionException) {
        _localctx.exception = re
        this._errHandler.reportError(this, re)
        this._errHandler.recover(this, re)
      } else {
        throw re
      }
    } finally {
      this.exitRule()
    }
    return _localctx
  }

  public static readonly _serializedATN: string =
    '\x03\uC91D\uCABA\u058D\uAFBA\u4F53\u0607\uEA8B\uC241\x03\x0F.\x04\x02' +
    '\t\x02\x04\x03\t\x03\x03\x02\x06\x02\b\n\x02\r\x02\x0E\x02\t\x03\x02\x03' +
    '\x02\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03' +
    '\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x07\x03\x1C\n\x03\f\x03\x0E' +
    '\x03\x1F\v\x03\x03\x03\x03\x03\x03\x03\x06\x03$\n\x03\r\x03\x0E\x03%\x03' +
    '\x03\x03\x03\x03\x03\x03\x03\x05\x03,\n\x03\x03\x03\x02\x02\x02\x04\x02' +
    '\x02\x04\x02\x02\x02\x027\x02\x07\x03\x02\x02\x02\x04+\x03\x02\x02\x02' +
    '\x06\b\x05\x04\x03\x02\x07\x06\x03\x02\x02\x02\b\t\x03\x02\x02\x02\t\x07' +
    '\x03\x02\x02\x02\t\n\x03\x02\x02\x02\n\v\x03\x02\x02\x02\v\f\x07\x02\x02' +
    '\x03\f\x03\x03\x02\x02\x02\r,\x07\n\x02\x02\x0E,\x07\v\x02\x02\x0F,\x07' +
    '\f\x02\x02\x10,\x07\r\x02\x02\x11\x12\x07\x03\x02\x02\x12,\x05\x04\x03' +
    '\x02\x13\x14\x07\x04\x02\x02\x14,\x05\x04\x03\x02\x15\x16\x07\x05\x02' +
    '\x02\x16,\x05\x04\x03\x02\x17\x18\x07\x06\x02\x02\x18,\x05\x04\x03\x02' +
    '\x19\x1D\x07\x07\x02\x02\x1A\x1C\x05\x04\x03\x02\x1B\x1A\x03\x02\x02\x02' +
    '\x1C\x1F\x03\x02\x02\x02\x1D\x1B\x03\x02\x02\x02\x1D\x1E\x03\x02\x02\x02' +
    '\x1E \x03\x02\x02\x02\x1F\x1D\x03\x02\x02\x02 ,\x07\b\x02\x02!#\x07\x07' +
    '\x02\x02"$\x05\x04\x03\x02#"\x03\x02\x02\x02$%\x03\x02\x02\x02%#\x03' +
    "\x02\x02\x02%&\x03\x02\x02\x02&'\x03\x02\x02\x02'(\x07\t\x02\x02()\x05" +
    '\x04\x03\x02)*\x07\b\x02\x02*,\x03\x02\x02\x02+\r\x03\x02\x02\x02+\x0E' +
    '\x03\x02\x02\x02+\x0F\x03\x02\x02\x02+\x10\x03\x02\x02\x02+\x11\x03\x02' +
    '\x02\x02+\x13\x03\x02\x02\x02+\x15\x03\x02\x02\x02+\x17\x03\x02\x02\x02' +
    '+\x19\x03\x02\x02\x02+!\x03\x02\x02\x02,\x05\x03\x02\x02\x02\x06\t\x1D' +
    '%+'
  public static __ATN: ATN
  public static get _ATN(): ATN {
    if (!SchemeParser.__ATN) {
      SchemeParser.__ATN = new ATNDeserializer().deserialize(
        Utils.toCharArray(SchemeParser._serializedATN)
      )
    }

    return SchemeParser.__ATN
  }
}

export class ProgramContext extends ParserRuleContext {
  public EOF(): TerminalNode {
    return this.getToken(SchemeParser.EOF, 0)
  }
  public expression(): ExpressionContext[]
  public expression(i: number): ExpressionContext
  public expression(i?: number): ExpressionContext | ExpressionContext[] {
    if (i === undefined) {
      return this.getRuleContexts(ExpressionContext)
    } else {
      return this.getRuleContext(i, ExpressionContext)
    }
  }
  constructor(parent: ParserRuleContext | undefined, invokingState: number) {
    super(parent, invokingState)
  }
  // @Override
  public get ruleIndex(): number {
    return SchemeParser.RULE_program
  }
  // @Override
  public enterRule(listener: SchemeListener): void {
    if (listener.enterProgram) {
      listener.enterProgram(this)
    }
  }
  // @Override
  public exitRule(listener: SchemeListener): void {
    if (listener.exitProgram) {
      listener.exitProgram(this)
    }
  }
  // @Override
  public accept<Result>(visitor: SchemeVisitor<Result>): Result {
    if (visitor.visitProgram) {
      return visitor.visitProgram(this)
    } else {
      return visitor.visitChildren(this)
    }
  }
}

export class ExpressionContext extends ParserRuleContext {
  constructor(parent: ParserRuleContext | undefined, invokingState: number) {
    super(parent, invokingState)
  }
  // @Override
  public get ruleIndex(): number {
    return SchemeParser.RULE_expression
  }
  public copyFrom(ctx: ExpressionContext): void {
    super.copyFrom(ctx)
  }
}
export class StringContext extends ExpressionContext {
  public STRING(): TerminalNode {
    return this.getToken(SchemeParser.STRING, 0)
  }
  constructor(ctx: ExpressionContext) {
    super(ctx.parent, ctx.invokingState)
    this.copyFrom(ctx)
  }
  // @Override
  public enterRule(listener: SchemeListener): void {
    if (listener.enterString) {
      listener.enterString(this)
    }
  }
  // @Override
  public exitRule(listener: SchemeListener): void {
    if (listener.exitString) {
      listener.exitString(this)
    }
  }
  // @Override
  public accept<Result>(visitor: SchemeVisitor<Result>): Result {
    if (visitor.visitString) {
      return visitor.visitString(this)
    } else {
      return visitor.visitChildren(this)
    }
  }
}
export class NumberContext extends ExpressionContext {
  public NUMBER(): TerminalNode {
    return this.getToken(SchemeParser.NUMBER, 0)
  }
  constructor(ctx: ExpressionContext) {
    super(ctx.parent, ctx.invokingState)
    this.copyFrom(ctx)
  }
  // @Override
  public enterRule(listener: SchemeListener): void {
    if (listener.enterNumber) {
      listener.enterNumber(this)
    }
  }
  // @Override
  public exitRule(listener: SchemeListener): void {
    if (listener.exitNumber) {
      listener.exitNumber(this)
    }
  }
  // @Override
  public accept<Result>(visitor: SchemeVisitor<Result>): Result {
    if (visitor.visitNumber) {
      return visitor.visitNumber(this)
    } else {
      return visitor.visitChildren(this)
    }
  }
}
export class BoolContext extends ExpressionContext {
  public BOOL(): TerminalNode {
    return this.getToken(SchemeParser.BOOL, 0)
  }
  constructor(ctx: ExpressionContext) {
    super(ctx.parent, ctx.invokingState)
    this.copyFrom(ctx)
  }
  // @Override
  public enterRule(listener: SchemeListener): void {
    if (listener.enterBool) {
      listener.enterBool(this)
    }
  }
  // @Override
  public exitRule(listener: SchemeListener): void {
    if (listener.exitBool) {
      listener.exitBool(this)
    }
  }
  // @Override
  public accept<Result>(visitor: SchemeVisitor<Result>): Result {
    if (visitor.visitBool) {
      return visitor.visitBool(this)
    } else {
      return visitor.visitChildren(this)
    }
  }
}
export class IdentifierContext extends ExpressionContext {
  public IDENTIFIER(): TerminalNode {
    return this.getToken(SchemeParser.IDENTIFIER, 0)
  }
  constructor(ctx: ExpressionContext) {
    super(ctx.parent, ctx.invokingState)
    this.copyFrom(ctx)
  }
  // @Override
  public enterRule(listener: SchemeListener): void {
    if (listener.enterIdentifier) {
      listener.enterIdentifier(this)
    }
  }
  // @Override
  public exitRule(listener: SchemeListener): void {
    if (listener.exitIdentifier) {
      listener.exitIdentifier(this)
    }
  }
  // @Override
  public accept<Result>(visitor: SchemeVisitor<Result>): Result {
    if (visitor.visitIdentifier) {
      return visitor.visitIdentifier(this)
    } else {
      return visitor.visitChildren(this)
    }
  }
}
export class QuoteContext extends ExpressionContext {
  public expression(): ExpressionContext {
    return this.getRuleContext(0, ExpressionContext)
  }
  constructor(ctx: ExpressionContext) {
    super(ctx.parent, ctx.invokingState)
    this.copyFrom(ctx)
  }
  // @Override
  public enterRule(listener: SchemeListener): void {
    if (listener.enterQuote) {
      listener.enterQuote(this)
    }
  }
  // @Override
  public exitRule(listener: SchemeListener): void {
    if (listener.exitQuote) {
      listener.exitQuote(this)
    }
  }
  // @Override
  public accept<Result>(visitor: SchemeVisitor<Result>): Result {
    if (visitor.visitQuote) {
      return visitor.visitQuote(this)
    } else {
      return visitor.visitChildren(this)
    }
  }
}
export class QuasiquoteContext extends ExpressionContext {
  public expression(): ExpressionContext {
    return this.getRuleContext(0, ExpressionContext)
  }
  constructor(ctx: ExpressionContext) {
    super(ctx.parent, ctx.invokingState)
    this.copyFrom(ctx)
  }
  // @Override
  public enterRule(listener: SchemeListener): void {
    if (listener.enterQuasiquote) {
      listener.enterQuasiquote(this)
    }
  }
  // @Override
  public exitRule(listener: SchemeListener): void {
    if (listener.exitQuasiquote) {
      listener.exitQuasiquote(this)
    }
  }
  // @Override
  public accept<Result>(visitor: SchemeVisitor<Result>): Result {
    if (visitor.visitQuasiquote) {
      return visitor.visitQuasiquote(this)
    } else {
      return visitor.visitChildren(this)
    }
  }
}
export class UnquoteContext extends ExpressionContext {
  public expression(): ExpressionContext {
    return this.getRuleContext(0, ExpressionContext)
  }
  constructor(ctx: ExpressionContext) {
    super(ctx.parent, ctx.invokingState)
    this.copyFrom(ctx)
  }
  // @Override
  public enterRule(listener: SchemeListener): void {
    if (listener.enterUnquote) {
      listener.enterUnquote(this)
    }
  }
  // @Override
  public exitRule(listener: SchemeListener): void {
    if (listener.exitUnquote) {
      listener.exitUnquote(this)
    }
  }
  // @Override
  public accept<Result>(visitor: SchemeVisitor<Result>): Result {
    if (visitor.visitUnquote) {
      return visitor.visitUnquote(this)
    } else {
      return visitor.visitChildren(this)
    }
  }
}
export class UnquoteSplicingContext extends ExpressionContext {
  public expression(): ExpressionContext {
    return this.getRuleContext(0, ExpressionContext)
  }
  constructor(ctx: ExpressionContext) {
    super(ctx.parent, ctx.invokingState)
    this.copyFrom(ctx)
  }
  // @Override
  public enterRule(listener: SchemeListener): void {
    if (listener.enterUnquoteSplicing) {
      listener.enterUnquoteSplicing(this)
    }
  }
  // @Override
  public exitRule(listener: SchemeListener): void {
    if (listener.exitUnquoteSplicing) {
      listener.exitUnquoteSplicing(this)
    }
  }
  // @Override
  public accept<Result>(visitor: SchemeVisitor<Result>): Result {
    if (visitor.visitUnquoteSplicing) {
      return visitor.visitUnquoteSplicing(this)
    } else {
      return visitor.visitChildren(this)
    }
  }
}
export class ListContext extends ExpressionContext {
  public _elements!: ExpressionContext
  public expression(): ExpressionContext[]
  public expression(i: number): ExpressionContext
  public expression(i?: number): ExpressionContext | ExpressionContext[] {
    if (i === undefined) {
      return this.getRuleContexts(ExpressionContext)
    } else {
      return this.getRuleContext(i, ExpressionContext)
    }
  }
  constructor(ctx: ExpressionContext) {
    super(ctx.parent, ctx.invokingState)
    this.copyFrom(ctx)
  }
  // @Override
  public enterRule(listener: SchemeListener): void {
    if (listener.enterList) {
      listener.enterList(this)
    }
  }
  // @Override
  public exitRule(listener: SchemeListener): void {
    if (listener.exitList) {
      listener.exitList(this)
    }
  }
  // @Override
  public accept<Result>(visitor: SchemeVisitor<Result>): Result {
    if (visitor.visitList) {
      return visitor.visitList(this)
    } else {
      return visitor.visitChildren(this)
    }
  }
}
export class DottedListContext extends ExpressionContext {
  public _pre!: ExpressionContext
  public _post!: ExpressionContext
  public expression(): ExpressionContext[]
  public expression(i: number): ExpressionContext
  public expression(i?: number): ExpressionContext | ExpressionContext[] {
    if (i === undefined) {
      return this.getRuleContexts(ExpressionContext)
    } else {
      return this.getRuleContext(i, ExpressionContext)
    }
  }
  constructor(ctx: ExpressionContext) {
    super(ctx.parent, ctx.invokingState)
    this.copyFrom(ctx)
  }
  // @Override
  public enterRule(listener: SchemeListener): void {
    if (listener.enterDottedList) {
      listener.enterDottedList(this)
    }
  }
  // @Override
  public exitRule(listener: SchemeListener): void {
    if (listener.exitDottedList) {
      listener.exitDottedList(this)
    }
  }
  // @Override
  public accept<Result>(visitor: SchemeVisitor<Result>): Result {
    if (visitor.visitDottedList) {
      return visitor.visitDottedList(this)
    } else {
      return visitor.visitChildren(this)
    }
  }
}
