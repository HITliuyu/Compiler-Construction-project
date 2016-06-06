/*
 * Copyright (c) 2012, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     isDotcumentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to enisDotrse or promote products
 *     derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, CLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, DIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (CLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER  CONTRACT, STRICT LIABILITY, OR TORT
 * (CLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * */

package de.tuberlin.uebb.comp1.exercise.stlc

/** Simple source to source optimizer for the Simply Typed Lambda Calculus */
object Optimizer {
  import AbstractSyntax._

  private val maxPass = 5

  def optimize(e: Expr): Expr = {
    var count = 0
    var e1 = e
    var e2 = optimizeOnePass(e)
    while (count < maxPass && e1 != e2) {
      e1 = e2
      e2 = optimizeOnePass(e1)
      count = count + 1
    }
    return e2
  }

  private def optimizeOnePass(e: Expr): Expr = {
    val e1 = e.bottomUp(constantFolding)
    val e2 = e1.bottomUp(arithmeticSimplification)
    val e3 = e2.bottomUp(deadCodeElimination)
    val e4 = e3.bottomUp(constantCopyPropagation)
    val e5 = e4.bottomUp(inlineAbs)
    val e6 = e5.bottomUp(inlineLet)
    e6
  }
  private def constantFolding(expr: Expr): Expr =
    expr match {
      case AddPrim(Num(v1, _), Num(v2, _), src) => Num(v1 + v2, src)
      case SubPrim(Num(v1, _), Num(v2, _), src) => Num(v1 - v2, src)
      case MulPrim(Num(v1, _), Num(v2, _), src) => Num(v1 * v2, src)
      case DivPrim(Num(v1, _), Num(v2, _), src) =>
        if (v2 != 0) Num(v1 / v2, src) else expr
      case EqPrim(Num(v1, _), Num(v2, _), src) =>
        if (v1 == v2) Bool(true, src) else Bool(false, src)
      case LtPrim(Num(v1, _), Num(v2, _), src) =>
        if (v1 < v2) Bool(true, src) else Bool(false, src)
      case Select(Tuple(comps, _), idx, _) => comps(idx - 1)
      case _ => expr
    }

  private def arithmeticSimplification(e: Expr): Expr =
    e match {
      case AddPrim(Num(0, _), e2, _) => e2
      case AddPrim(e1, Num(0, _), _) => e1
      case SubPrim(e1, Num(0, _), _) => e1
      case MulPrim(Num(1, _), e2, _) => e2
      case MulPrim(e1, Num(1, _), _) => e1
      case DivPrim(e1, Num(1, _), _) => e1
      case e1 => e1
    }

  private def deadCodeElimination(e: Expr): Expr =
    e match {
      case Cond(Bool(true, _), i, t, _) => i
      case Cond(Bool(false, _), i, t, _) => t
      case _ => e
    }

  private def constantCopyPropagation(e: Expr): Expr =
    e match {
      case LetIn(v, rhs @ Num(_, _), body, _) => body.subst(v, rhs)
      case LetIn(v, rhs @ Bool(_, _), body, _) => body.subst(v, rhs)
      case LetIn(v, rhs @ Var(_, _), body, _) => body.subst(v, rhs)
      case e1 => e1
    }

  private def inlineLet(e: Expr): Expr =
    // Strictly speaking, this transformation may only be applied
    // if rhs terminates, otherwise it may tamper with
    // the termination behavior of the program if v is not
    // free in body or occurs in positions which are
    // not evaluated.
    e match {
      case e1 @ LetIn(v, rhs, body, _) =>
        // Only inline once to avoid code explosion
        if (body.occurrenceFV(v) < 10)
          body.subst(v, rhs)
        else
          e1
      case e1 => e1
    }

  private def inlineAbs(e: Expr): Expr =
    e match {
      case App(Abs(v, ty, body, _), arg, src) => LetIn(v, arg, body, src)
      case e1 => e1
    }
}
