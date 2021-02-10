// Generated from ./src/lang/Calc.g4 by ANTLR 4.9.0-SNAPSHOT

import { ParseTreeVisitor } from 'antlr4ts/tree/ParseTreeVisitor'

import { NumberContext } from './CalcParser'
import { ParenthesesContext } from './CalcParser'
import { PowerContext } from './CalcParser'
import { MultiplicationContext } from './CalcParser'
import { DivisionContext } from './CalcParser'
import { AdditionContext } from './CalcParser'
import { SubtractionContext } from './CalcParser'
import { StartContext } from './CalcParser'
import { ExpressionContext } from './CalcParser'

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by `CalcParser`.
 *
 * @param <Result> The return type of the visit operation. Use `void` for
 * operations with no return type.
 */
export interface CalcVisitor<Result> extends ParseTreeVisitor<Result> {
  /**
   * Visit a parse tree produced by the `Number`
   * labeled alternative in `CalcParser.expression`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitNumber?: (ctx: NumberContext) => Result

  /**
   * Visit a parse tree produced by the `Parentheses`
   * labeled alternative in `CalcParser.expression`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitParentheses?: (ctx: ParenthesesContext) => Result

  /**
   * Visit a parse tree produced by the `Power`
   * labeled alternative in `CalcParser.expression`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitPower?: (ctx: PowerContext) => Result

  /**
   * Visit a parse tree produced by the `Multiplication`
   * labeled alternative in `CalcParser.expression`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitMultiplication?: (ctx: MultiplicationContext) => Result

  /**
   * Visit a parse tree produced by the `Division`
   * labeled alternative in `CalcParser.expression`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitDivision?: (ctx: DivisionContext) => Result

  /**
   * Visit a parse tree produced by the `Addition`
   * labeled alternative in `CalcParser.expression`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitAddition?: (ctx: AdditionContext) => Result

  /**
   * Visit a parse tree produced by the `Subtraction`
   * labeled alternative in `CalcParser.expression`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitSubtraction?: (ctx: SubtractionContext) => Result

  /**
   * Visit a parse tree produced by `CalcParser.start`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitStart?: (ctx: StartContext) => Result

  /**
   * Visit a parse tree produced by `CalcParser.expression`.
   * @param ctx the parse tree
   * @return the visitor result
   */
  visitExpression?: (ctx: ExpressionContext) => Result
}
