// Generated from ./src/lang/Scheme.g4 by ANTLR 4.9.0-SNAPSHOT
// @ts-ignore

import { ParseTreeVisitor } from 'antlr4ts/tree/ParseTreeVisitor'
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
 * This interface defines a complete generic visitor for a parse tree produced
 * by `SchemeParser`.
 *
 * @param <Result> The return type of the visit operation. Use `void` for
 * operations with no return type.
 */
export interface SchemeVisitor<Result> extends ParseTreeVisitor<Result> {
  /**
   * Visit a parse tree produced by the `String`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitString?: (ctx: StringContext) => Result

  /**
   * Visit a parse tree produced by the `Number`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitNumber?: (ctx: NumberContext) => Result

  /**
   * Visit a parse tree produced by the `Bool`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitBool?: (ctx: BoolContext) => Result

  /**
   * Visit a parse tree produced by the `Identifier`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitIdentifier?: (ctx: IdentifierContext) => Result

  /**
   * Visit a parse tree produced by the `Quote`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitQuote?: (ctx: QuoteContext) => Result

  /**
   * Visit a parse tree produced by the `Quasiquote`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitQuasiquote?: (ctx: QuasiquoteContext) => Result

  /**
   * Visit a parse tree produced by the `Unquote`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitUnquote?: (ctx: UnquoteContext) => Result

  /**
   * Visit a parse tree produced by the `UnquoteSplicing`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitUnquoteSplicing?: (ctx: UnquoteSplicingContext) => Result

  /**
   * Visit a parse tree produced by the `List`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitList?: (ctx: ListContext) => Result

  /**
   * Visit a parse tree produced by the `DottedList`
   * labeled alternative in `SchemeParser.expression`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitDottedList?: (ctx: DottedListContext) => Result

  /**
   * Visit a parse tree produced by `SchemeParser.program`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitProgram?: (ctx: ProgramContext) => Result

  /**
   * Visit a parse tree produced by `SchemeParser.expression`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitExpression?: (ctx: ExpressionContext) => Result
}
