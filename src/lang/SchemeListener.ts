// Generated from ./src/lang/Scheme.g4 by ANTLR 4.9.0-SNAPSHOT
// @ts-ignore

import { ParseTreeListener } from 'antlr4ts/tree/ParseTreeListener'
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
  StringContext,
  UnquoteContext,
  UnquoteSplicingContext
} from './SchemeParser'

/**
 * This interface defines a complete listener for a parse tree produced by
 * `SchemeParser`.
 */
export interface SchemeListener extends ParseTreeListener {
  /**
   * Enter a parse tree produced by the `String`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  enterString?: (ctx: StringContext) => void
  /**
   * Exit a parse tree produced by the `String`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  exitString?: (ctx: StringContext) => void

  /**
   * Enter a parse tree produced by the `Number`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  enterNumber?: (ctx: NumberContext) => void
  /**
   * Exit a parse tree produced by the `Number`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  exitNumber?: (ctx: NumberContext) => void

  /**
   * Enter a parse tree produced by the `Bool`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  enterBool?: (ctx: BoolContext) => void
  /**
   * Exit a parse tree produced by the `Bool`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  exitBool?: (ctx: BoolContext) => void

  /**
   * Enter a parse tree produced by the `Identifier`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  enterIdentifier?: (ctx: IdentifierContext) => void
  /**
   * Exit a parse tree produced by the `Identifier`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  exitIdentifier?: (ctx: IdentifierContext) => void

  /**
   * Enter a parse tree produced by the `Quote`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  enterQuote?: (ctx: QuoteContext) => void
  /**
   * Exit a parse tree produced by the `Quote`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  exitQuote?: (ctx: QuoteContext) => void

  /**
   * Enter a parse tree produced by the `Quasiquote`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  enterQuasiquote?: (ctx: QuasiquoteContext) => void
  /**
   * Exit a parse tree produced by the `Quasiquote`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  exitQuasiquote?: (ctx: QuasiquoteContext) => void

  /**
   * Enter a parse tree produced by the `Unquote`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  enterUnquote?: (ctx: UnquoteContext) => void
  /**
   * Exit a parse tree produced by the `Unquote`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  exitUnquote?: (ctx: UnquoteContext) => void

  /**
   * Enter a parse tree produced by the `UnquoteSplicing`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  enterUnquoteSplicing?: (ctx: UnquoteSplicingContext) => void
  /**
   * Exit a parse tree produced by the `UnquoteSplicing`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  exitUnquoteSplicing?: (ctx: UnquoteSplicingContext) => void

  /**
   * Enter a parse tree produced by the `List`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  enterList?: (ctx: ListContext) => void
  /**
   * Exit a parse tree produced by the `List`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  exitList?: (ctx: ListContext) => void

  /**
   * Enter a parse tree produced by the `DottedList`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  enterDottedList?: (ctx: DottedListContext) => void
  /**
   * Exit a parse tree produced by the `DottedList`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  exitDottedList?: (ctx: DottedListContext) => void

  /**
   * Enter a parse tree produced by `SchemeParser.program`.
   * @param ctx the parse tree
   */
  enterProgram?: (ctx: ProgramContext) => void
  /**
   * Exit a parse tree produced by `SchemeParser.program`.
   * @param ctx the parse tree
   */
  exitProgram?: (ctx: ProgramContext) => void

  /**
   * Enter a parse tree produced by `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  enterExpression?: (ctx: ExpressionContext) => void
  /**
   * Exit a parse tree produced by `SchemeParser.expression`.
   * @param ctx the parse tree
   */
  exitExpression?: (ctx: ExpressionContext) => void
}
