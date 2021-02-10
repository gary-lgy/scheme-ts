// Generated from ./src/lang/Calc.g4 by ANTLR 4.9.0-SNAPSHOT
// @ts-nocheck

import { ATN } from 'antlr4ts/atn/ATN'
import { ATNDeserializer } from 'antlr4ts/atn/ATNDeserializer'
import { FailedPredicateException } from 'antlr4ts/FailedPredicateException'
import { NotNull } from 'antlr4ts/Decorators'
import { NoViableAltException } from 'antlr4ts/NoViableAltException'
import { Override } from 'antlr4ts/Decorators'
import { Parser } from 'antlr4ts/Parser'
import { ParserRuleContext } from 'antlr4ts/ParserRuleContext'
import { ParserATNSimulator } from 'antlr4ts/atn/ParserATNSimulator'
import { ParseTreeListener } from 'antlr4ts/tree/ParseTreeListener'
import { ParseTreeVisitor } from 'antlr4ts/tree/ParseTreeVisitor'
import { RecognitionException } from 'antlr4ts/RecognitionException'
import { RuleContext } from 'antlr4ts/RuleContext'
//import { RuleVersion } from "antlr4ts/RuleVersion";
import { TerminalNode } from 'antlr4ts/tree/TerminalNode'
import { Token } from 'antlr4ts/Token'
import { TokenStream } from 'antlr4ts/TokenStream'
import { Vocabulary } from 'antlr4ts/Vocabulary'
import { VocabularyImpl } from 'antlr4ts/VocabularyImpl'

import * as Utils from 'antlr4ts/misc/Utils'

import { CalcListener } from './CalcListener'
import { CalcVisitor } from './CalcVisitor'

export class CalcParser extends Parser {
  public static readonly T__0 = 1
  public static readonly T__1 = 2
  public static readonly POW = 3
  public static readonly MUL = 4
  public static readonly DIV = 5
  public static readonly ADD = 6
  public static readonly SUB = 7
  public static readonly NUMBER = 8
  public static readonly WHITESPACE = 9
  public static readonly RULE_start = 0
  public static readonly RULE_expression = 1
  // tslint:disable:no-trailing-whitespace
  public static readonly ruleNames: string[] = ['start', 'expression']

  private static readonly _LITERAL_NAMES: Array<string | undefined> = [
    undefined,
    "'('",
    "')'",
    "'^'",
    "'*'",
    "'/'",
    "'+'",
    "'-'"
  ]
  private static readonly _SYMBOLIC_NAMES: Array<string | undefined> = [
    undefined,
    undefined,
    undefined,
    'POW',
    'MUL',
    'DIV',
    'ADD',
    'SUB',
    'NUMBER',
    'WHITESPACE'
  ]
  public static readonly VOCABULARY: Vocabulary = new VocabularyImpl(
    CalcParser._LITERAL_NAMES,
    CalcParser._SYMBOLIC_NAMES,
    []
  )

  // @Override
  // @NotNull
  public get vocabulary(): Vocabulary {
    return CalcParser.VOCABULARY
  }
  // tslint:enable:no-trailing-whitespace

  // @Override
  public get grammarFileName(): string {
    return 'Calc.g4'
  }

  // @Override
  public get ruleNames(): string[] {
    return CalcParser.ruleNames
  }

  // @Override
  public get serializedATN(): string {
    return CalcParser._serializedATN
  }

  protected createFailedPredicateException(
    predicate?: string,
    message?: string
  ): FailedPredicateException {
    return new FailedPredicateException(this, predicate, message)
  }

  constructor(input: TokenStream) {
    super(input)
    this._interp = new ParserATNSimulator(CalcParser._ATN, this)
  }
  // @RuleVersion(0)
  public start(): StartContext {
    const _localctx: StartContext = new StartContext(this._ctx, this.state)
    this.enterRule(_localctx, 0, CalcParser.RULE_start)
    try {
      this.enterOuterAlt(_localctx, 1)
      {
        this.state = 4
        this.expression(0)
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

  public expression(): ExpressionContext
  public expression(_p: number): ExpressionContext
  // @RuleVersion(0)
  public expression(_p?: number): ExpressionContext {
    if (_p === undefined) {
      _p = 0
    }

    const _parentctx: ParserRuleContext = this._ctx
    const _parentState: number = this.state
    let _localctx: ExpressionContext = new ExpressionContext(this._ctx, _parentState)
    let _prevctx: ExpressionContext = _localctx
    const _startState: number = 2
    this.enterRecursionRule(_localctx, 2, CalcParser.RULE_expression, _p)
    try {
      let _alt: number
      this.enterOuterAlt(_localctx, 1)
      {
        this.state = 12
        this._errHandler.sync(this)
        switch (this._input.LA(1)) {
          case CalcParser.NUMBER:
            {
              _localctx = new NumberContext(_localctx)
              this._ctx = _localctx
              _prevctx = _localctx

              this.state = 7
              this.match(CalcParser.NUMBER)
            }
            break
          case CalcParser.T__0:
            {
              _localctx = new ParenthesesContext(_localctx)
              this._ctx = _localctx
              _prevctx = _localctx
              this.state = 8
              this.match(CalcParser.T__0)
              this.state = 9
              ;(_localctx as ParenthesesContext)._inner = this.expression(0)
              this.state = 10
              this.match(CalcParser.T__1)
            }
            break
          default:
            throw new NoViableAltException(this)
        }
        this._ctx._stop = this._input.tryLT(-1)
        this.state = 31
        this._errHandler.sync(this)
        _alt = this.interpreter.adaptivePredict(this._input, 2, this._ctx)
        while (_alt !== 2 && _alt !== ATN.INVALID_ALT_NUMBER) {
          if (_alt === 1) {
            if (this._parseListeners != null) {
              this.triggerExitRuleEvent()
            }
            _prevctx = _localctx
            {
              this.state = 29
              this._errHandler.sync(this)
              switch (this.interpreter.adaptivePredict(this._input, 1, this._ctx)) {
                case 1:
                  {
                    _localctx = new PowerContext(new ExpressionContext(_parentctx, _parentState))
                    ;(_localctx as PowerContext)._left = _prevctx
                    this.pushNewRecursionContext(_localctx, _startState, CalcParser.RULE_expression)
                    this.state = 14
                    if (!this.precpred(this._ctx, 5)) {
                      throw this.createFailedPredicateException('this.precpred(this._ctx, 5)')
                    }
                    this.state = 15
                    ;(_localctx as PowerContext)._operator = this.match(CalcParser.POW)
                    this.state = 16
                    ;(_localctx as PowerContext)._right = this.expression(6)
                  }
                  break

                case 2:
                  {
                    _localctx = new MultiplicationContext(
                      new ExpressionContext(_parentctx, _parentState)
                    )
                    ;(_localctx as MultiplicationContext)._left = _prevctx
                    this.pushNewRecursionContext(_localctx, _startState, CalcParser.RULE_expression)
                    this.state = 17
                    if (!this.precpred(this._ctx, 4)) {
                      throw this.createFailedPredicateException('this.precpred(this._ctx, 4)')
                    }
                    this.state = 18
                    ;(_localctx as MultiplicationContext)._operator = this.match(CalcParser.MUL)
                    this.state = 19
                    ;(_localctx as MultiplicationContext)._right = this.expression(5)
                  }
                  break

                case 3:
                  {
                    _localctx = new DivisionContext(new ExpressionContext(_parentctx, _parentState))
                    ;(_localctx as DivisionContext)._left = _prevctx
                    this.pushNewRecursionContext(_localctx, _startState, CalcParser.RULE_expression)
                    this.state = 20
                    if (!this.precpred(this._ctx, 3)) {
                      throw this.createFailedPredicateException('this.precpred(this._ctx, 3)')
                    }
                    this.state = 21
                    ;(_localctx as DivisionContext)._operator = this.match(CalcParser.DIV)
                    this.state = 22
                    ;(_localctx as DivisionContext)._right = this.expression(4)
                  }
                  break

                case 4:
                  {
                    _localctx = new AdditionContext(new ExpressionContext(_parentctx, _parentState))
                    ;(_localctx as AdditionContext)._left = _prevctx
                    this.pushNewRecursionContext(_localctx, _startState, CalcParser.RULE_expression)
                    this.state = 23
                    if (!this.precpred(this._ctx, 2)) {
                      throw this.createFailedPredicateException('this.precpred(this._ctx, 2)')
                    }
                    this.state = 24
                    ;(_localctx as AdditionContext)._operator = this.match(CalcParser.ADD)
                    this.state = 25
                    ;(_localctx as AdditionContext)._right = this.expression(3)
                  }
                  break

                case 5:
                  {
                    _localctx = new SubtractionContext(
                      new ExpressionContext(_parentctx, _parentState)
                    )
                    ;(_localctx as SubtractionContext)._left = _prevctx
                    this.pushNewRecursionContext(_localctx, _startState, CalcParser.RULE_expression)
                    this.state = 26
                    if (!this.precpred(this._ctx, 1)) {
                      throw this.createFailedPredicateException('this.precpred(this._ctx, 1)')
                    }
                    this.state = 27
                    ;(_localctx as SubtractionContext)._operator = this.match(CalcParser.SUB)
                    this.state = 28
                    ;(_localctx as SubtractionContext)._right = this.expression(2)
                  }
                  break
              }
            }
          }
          this.state = 33
          this._errHandler.sync(this)
          _alt = this.interpreter.adaptivePredict(this._input, 2, this._ctx)
        }
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
      this.unrollRecursionContexts(_parentctx)
    }
    return _localctx
  }

  public sempred(_localctx: RuleContext, ruleIndex: number, predIndex: number): boolean {
    switch (ruleIndex) {
      case 1:
        return this.expression_sempred(_localctx as ExpressionContext, predIndex)
    }
    return true
  }
  private expression_sempred(_localctx: ExpressionContext, predIndex: number): boolean {
    switch (predIndex) {
      case 0:
        return this.precpred(this._ctx, 5)

      case 1:
        return this.precpred(this._ctx, 4)

      case 2:
        return this.precpred(this._ctx, 3)

      case 3:
        return this.precpred(this._ctx, 2)

      case 4:
        return this.precpred(this._ctx, 1)
    }
    return true
  }

  public static readonly _serializedATN: string =
    '\x03\uC91D\uCABA\u058D\uAFBA\u4F53\u0607\uEA8B\uC241\x03\v%\x04\x02\t' +
    '\x02\x04\x03\t\x03\x03\x02\x03\x02\x03\x03\x03\x03\x03\x03\x03\x03\x03' +
    '\x03\x03\x03\x05\x03\x0F\n\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03' +
    '\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03\x03' +
    '\x03\x03\x07\x03 \n\x03\f\x03\x0E\x03#\v\x03\x03\x03\x02\x02\x03\x04\x04' +
    '\x02\x02\x04\x02\x02\x02\x02(\x02\x06\x03\x02\x02\x02\x04\x0E\x03\x02' +
    '\x02\x02\x06\x07\x05\x04\x03\x02\x07\x03\x03\x02\x02\x02\b\t\b\x03\x01' +
    '\x02\t\x0F\x07\n\x02\x02\n\v\x07\x03\x02\x02\v\f\x05\x04\x03\x02\f\r\x07' +
    '\x04\x02\x02\r\x0F\x03\x02\x02\x02\x0E\b\x03\x02\x02\x02\x0E\n\x03\x02' +
    '\x02\x02\x0F!\x03\x02\x02\x02\x10\x11\f\x07\x02\x02\x11\x12\x07\x05\x02' +
    '\x02\x12 \x05\x04\x03\b\x13\x14\f\x06\x02\x02\x14\x15\x07\x06\x02\x02' +
    '\x15 \x05\x04\x03\x07\x16\x17\f\x05\x02\x02\x17\x18\x07\x07\x02\x02\x18' +
    ' \x05\x04\x03\x06\x19\x1A\f\x04\x02\x02\x1A\x1B\x07\b\x02\x02\x1B \x05' +
    '\x04\x03\x05\x1C\x1D\f\x03\x02\x02\x1D\x1E\x07\t\x02\x02\x1E \x05\x04' +
    '\x03\x04\x1F\x10\x03\x02\x02\x02\x1F\x13\x03\x02\x02\x02\x1F\x16\x03\x02' +
    '\x02\x02\x1F\x19\x03\x02\x02\x02\x1F\x1C\x03\x02\x02\x02 #\x03\x02\x02' +
    '\x02!\x1F\x03\x02\x02\x02!"\x03\x02\x02\x02"\x05\x03\x02\x02\x02#!\x03' +
    '\x02\x02\x02\x05\x0E\x1F!'
  public static __ATN: ATN
  public static get _ATN(): ATN {
    if (!CalcParser.__ATN) {
      CalcParser.__ATN = new ATNDeserializer().deserialize(
        Utils.toCharArray(CalcParser._serializedATN)
      )
    }

    return CalcParser.__ATN
  }
}

export class StartContext extends ParserRuleContext {
  public expression(): ExpressionContext {
    return this.getRuleContext(0, ExpressionContext)
  }
  constructor(parent: ParserRuleContext | undefined, invokingState: number) {
    super(parent, invokingState)
  }
  // @Override
  public get ruleIndex(): number {
    return CalcParser.RULE_start
  }
  // @Override
  public enterRule(listener: CalcListener): void {
    if (listener.enterStart) {
      listener.enterStart(this)
    }
  }
  // @Override
  public exitRule(listener: CalcListener): void {
    if (listener.exitStart) {
      listener.exitStart(this)
    }
  }
  // @Override
  public accept<Result>(visitor: CalcVisitor<Result>): Result {
    if (visitor.visitStart) {
      return visitor.visitStart(this)
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
    return CalcParser.RULE_expression
  }
  public copyFrom(ctx: ExpressionContext): void {
    super.copyFrom(ctx)
  }
}
export class NumberContext extends ExpressionContext {
  public NUMBER(): TerminalNode {
    return this.getToken(CalcParser.NUMBER, 0)
  }
  constructor(ctx: ExpressionContext) {
    super(ctx.parent, ctx.invokingState)
    this.copyFrom(ctx)
  }
  // @Override
  public enterRule(listener: CalcListener): void {
    if (listener.enterNumber) {
      listener.enterNumber(this)
    }
  }
  // @Override
  public exitRule(listener: CalcListener): void {
    if (listener.exitNumber) {
      listener.exitNumber(this)
    }
  }
  // @Override
  public accept<Result>(visitor: CalcVisitor<Result>): Result {
    if (visitor.visitNumber) {
      return visitor.visitNumber(this)
    } else {
      return visitor.visitChildren(this)
    }
  }
}
export class ParenthesesContext extends ExpressionContext {
  public _inner!: ExpressionContext
  public expression(): ExpressionContext {
    return this.getRuleContext(0, ExpressionContext)
  }
  constructor(ctx: ExpressionContext) {
    super(ctx.parent, ctx.invokingState)
    this.copyFrom(ctx)
  }
  // @Override
  public enterRule(listener: CalcListener): void {
    if (listener.enterParentheses) {
      listener.enterParentheses(this)
    }
  }
  // @Override
  public exitRule(listener: CalcListener): void {
    if (listener.exitParentheses) {
      listener.exitParentheses(this)
    }
  }
  // @Override
  public accept<Result>(visitor: CalcVisitor<Result>): Result {
    if (visitor.visitParentheses) {
      return visitor.visitParentheses(this)
    } else {
      return visitor.visitChildren(this)
    }
  }
}
export class PowerContext extends ExpressionContext {
  public _left!: ExpressionContext
  public _operator!: Token
  public _right!: ExpressionContext
  public expression(): ExpressionContext[]
  public expression(i: number): ExpressionContext
  public expression(i?: number): ExpressionContext | ExpressionContext[] {
    if (i === undefined) {
      return this.getRuleContexts(ExpressionContext)
    } else {
      return this.getRuleContext(i, ExpressionContext)
    }
  }
  public POW(): TerminalNode {
    return this.getToken(CalcParser.POW, 0)
  }
  constructor(ctx: ExpressionContext) {
    super(ctx.parent, ctx.invokingState)
    this.copyFrom(ctx)
  }
  // @Override
  public enterRule(listener: CalcListener): void {
    if (listener.enterPower) {
      listener.enterPower(this)
    }
  }
  // @Override
  public exitRule(listener: CalcListener): void {
    if (listener.exitPower) {
      listener.exitPower(this)
    }
  }
  // @Override
  public accept<Result>(visitor: CalcVisitor<Result>): Result {
    if (visitor.visitPower) {
      return visitor.visitPower(this)
    } else {
      return visitor.visitChildren(this)
    }
  }
}
export class MultiplicationContext extends ExpressionContext {
  public _left!: ExpressionContext
  public _operator!: Token
  public _right!: ExpressionContext
  public expression(): ExpressionContext[]
  public expression(i: number): ExpressionContext
  public expression(i?: number): ExpressionContext | ExpressionContext[] {
    if (i === undefined) {
      return this.getRuleContexts(ExpressionContext)
    } else {
      return this.getRuleContext(i, ExpressionContext)
    }
  }
  public MUL(): TerminalNode {
    return this.getToken(CalcParser.MUL, 0)
  }
  constructor(ctx: ExpressionContext) {
    super(ctx.parent, ctx.invokingState)
    this.copyFrom(ctx)
  }
  // @Override
  public enterRule(listener: CalcListener): void {
    if (listener.enterMultiplication) {
      listener.enterMultiplication(this)
    }
  }
  // @Override
  public exitRule(listener: CalcListener): void {
    if (listener.exitMultiplication) {
      listener.exitMultiplication(this)
    }
  }
  // @Override
  public accept<Result>(visitor: CalcVisitor<Result>): Result {
    if (visitor.visitMultiplication) {
      return visitor.visitMultiplication(this)
    } else {
      return visitor.visitChildren(this)
    }
  }
}
export class DivisionContext extends ExpressionContext {
  public _left!: ExpressionContext
  public _operator!: Token
  public _right!: ExpressionContext
  public expression(): ExpressionContext[]
  public expression(i: number): ExpressionContext
  public expression(i?: number): ExpressionContext | ExpressionContext[] {
    if (i === undefined) {
      return this.getRuleContexts(ExpressionContext)
    } else {
      return this.getRuleContext(i, ExpressionContext)
    }
  }
  public DIV(): TerminalNode {
    return this.getToken(CalcParser.DIV, 0)
  }
  constructor(ctx: ExpressionContext) {
    super(ctx.parent, ctx.invokingState)
    this.copyFrom(ctx)
  }
  // @Override
  public enterRule(listener: CalcListener): void {
    if (listener.enterDivision) {
      listener.enterDivision(this)
    }
  }
  // @Override
  public exitRule(listener: CalcListener): void {
    if (listener.exitDivision) {
      listener.exitDivision(this)
    }
  }
  // @Override
  public accept<Result>(visitor: CalcVisitor<Result>): Result {
    if (visitor.visitDivision) {
      return visitor.visitDivision(this)
    } else {
      return visitor.visitChildren(this)
    }
  }
}
export class AdditionContext extends ExpressionContext {
  public _left!: ExpressionContext
  public _operator!: Token
  public _right!: ExpressionContext
  public expression(): ExpressionContext[]
  public expression(i: number): ExpressionContext
  public expression(i?: number): ExpressionContext | ExpressionContext[] {
    if (i === undefined) {
      return this.getRuleContexts(ExpressionContext)
    } else {
      return this.getRuleContext(i, ExpressionContext)
    }
  }
  public ADD(): TerminalNode {
    return this.getToken(CalcParser.ADD, 0)
  }
  constructor(ctx: ExpressionContext) {
    super(ctx.parent, ctx.invokingState)
    this.copyFrom(ctx)
  }
  // @Override
  public enterRule(listener: CalcListener): void {
    if (listener.enterAddition) {
      listener.enterAddition(this)
    }
  }
  // @Override
  public exitRule(listener: CalcListener): void {
    if (listener.exitAddition) {
      listener.exitAddition(this)
    }
  }
  // @Override
  public accept<Result>(visitor: CalcVisitor<Result>): Result {
    if (visitor.visitAddition) {
      return visitor.visitAddition(this)
    } else {
      return visitor.visitChildren(this)
    }
  }
}
export class SubtractionContext extends ExpressionContext {
  public _left!: ExpressionContext
  public _operator!: Token
  public _right!: ExpressionContext
  public expression(): ExpressionContext[]
  public expression(i: number): ExpressionContext
  public expression(i?: number): ExpressionContext | ExpressionContext[] {
    if (i === undefined) {
      return this.getRuleContexts(ExpressionContext)
    } else {
      return this.getRuleContext(i, ExpressionContext)
    }
  }
  public SUB(): TerminalNode {
    return this.getToken(CalcParser.SUB, 0)
  }
  constructor(ctx: ExpressionContext) {
    super(ctx.parent, ctx.invokingState)
    this.copyFrom(ctx)
  }
  // @Override
  public enterRule(listener: CalcListener): void {
    if (listener.enterSubtraction) {
      listener.enterSubtraction(this)
    }
  }
  // @Override
  public exitRule(listener: CalcListener): void {
    if (listener.exitSubtraction) {
      listener.exitSubtraction(this)
    }
  }
  // @Override
  public accept<Result>(visitor: CalcVisitor<Result>): Result {
    if (visitor.visitSubtraction) {
      return visitor.visitSubtraction(this)
    } else {
      return visitor.visitChildren(this)
    }
  }
}
