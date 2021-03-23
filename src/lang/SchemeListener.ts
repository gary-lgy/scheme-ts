// Generated from ./src/lang/Scheme.g4 by ANTLR 4.9.0-SNAPSHOT
// @ts-ignore

import { ParseTreeListener } from 'antlr4ts/tree/ParseTreeListener'
import {
  BoolContext,
  ExpressionContext,
  IdentifierContext,
  ListContext,
  NumberContext,
  ProgramContext,
  SequenceContext,
  StringContext
} from './SchemeParser'

/**
 * This interface defines a complete listener for a parse tree produced by
 * `SchemeParser`.
 */
export interface SchemeListener extends ParseTreeListener {
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
   * Enter a parse tree produced by `SchemeParser.sequence`.
   * @param ctx the parse tree
   */
  enterSequence?: (ctx: SequenceContext) => void
  /**
   * Exit a parse tree produced by `SchemeParser.sequence`.
   * @param ctx the parse tree
   */
  exitSequence?: (ctx: SequenceContext) => void

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
