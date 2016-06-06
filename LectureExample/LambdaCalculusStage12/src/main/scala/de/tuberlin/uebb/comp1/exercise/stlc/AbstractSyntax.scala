/*
 * Copyright (c) 2013, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

package de.tuberlin.uebb.comp1.exercise.stlc

import scala.util.parsing.input
import PrettyPrinter._
import scala.collection.immutable.ListSet

/** Abstract syntax of the Simply Typed Lambda Calculus */
object AbstractSyntax {
  abstract class Expr(val src: Range) {
    override def toString() = pretty(this)
    /** Free variables of an expression */
    def freeVars: ListSet[String]
    /** Substitute v by e in this */
    def subst(v: String, e: Expr): Expr
    /** Occurrence count of a given free variable */
    def occurrenceFV(v: String): Int
    /** Apply source to source transformation bottom-up */
    def bottomUp(transf: Expr => Expr): Expr =
      transf(this.transformSubexprs(_.bottomUp(transf)))
    /** Apply source to source transformation top-down */
    def topDown(transf: Expr => Expr): Expr =
      transf(this).transformSubexprs(_.topDown(transf))
    protected def transformSubexprs(transf: Expr => Expr): Expr
  }
  case class Abs(v: String, ty: Type, body: Expr, override val src: Range) extends Expr(src) {
    def freeVars = body.freeVars - v
    def subst(v1: String, e: Expr): Expr =
      if (v == v1)
        this
      else if (e.freeVars.contains(v)) {
        val vFresh = pickFreshVar(v, e.freeVars union ListSet(v))
        Abs(vFresh, ty, body.subst(v, Var(vFresh, noLocation)).subst(v1, e), src)
      } else
        Abs(v, ty, body.subst(v1, e), src)
    def occurrenceFV(v1: String): Int =
      if (v == v1) 0 else body.occurrenceFV(v1)
    protected def transformSubexprs(transf: Expr => Expr): Expr =
      Abs(v, ty, transf(body), src)
  }
  case class App(fun: Expr, arg: Expr, override val src: Range) extends Expr(src) {
    def freeVars = fun.freeVars union arg.freeVars
    def subst(v1: String, e: Expr): Expr =
      App(fun.subst(v1, e), arg.subst(v1, e), src)
    def occurrenceFV(v1: String): Int =
      fun.occurrenceFV(v1) + arg.occurrenceFV(v1)
    protected def transformSubexprs(transf: Expr => Expr): Expr =
      App(transf(fun), transf(arg), src)
  }
  case class Var(name: String, override val src: Range) extends Expr(src) {
    def freeVars = ListSet(name)
    def subst(v1: String, e: Expr): Expr =
      if (name == v1) e else this
    def occurrenceFV(v1: String): Int =
      if (name == v1) 1 else 0
    protected def transformSubexprs(transf: Expr => Expr): Expr = this
  }
  case class Fix(fun: String, tyArg: Type, tyRes: Type, arg: String, body: Expr, override val src: Range) extends Expr(src) {
    def freeVars = body.freeVars - fun - arg
    def subst(v1: String, e: Expr): Expr =
      if (v1 == fun || v1 == arg)
        this
      else {
        val funFresh =
          if (e.freeVars.contains(fun))
            pickFreshVar(fun, e.freeVars union ListSet(fun, arg))
          else
            fun
        val argFresh =
          if (e.freeVars.contains(arg))
            pickFreshVar(arg, e.freeVars union ListSet(funFresh, arg))
          else
            arg
        val body1 = body.subst(fun, Var(funFresh, noLocation))
        val body2 = body1.subst(arg, Var(argFresh, noLocation))
        Fix(funFresh, tyArg, tyRes, argFresh, body2.subst(v1, e), src)
      }
    def occurrenceFV(v1: String): Int =
      if (v1 == fun || v1 == arg) 0 else body.occurrenceFV(v1)
    protected def transformSubexprs(transf: Expr => Expr): Expr =
      Fix(fun, tyArg, tyRes, arg, transf(body), src)
  }
  case class Num(v: Int, override val src: Range) extends Expr(src) {
    def freeVars = ListSet.empty
    def subst(v1: String, e: Expr): Expr = this
    def occurrenceFV(v1: String): Int = 0
    protected def transformSubexprs(transf: Expr => Expr): Expr = this
  }
  case class Bool(v: Boolean, override val src: Range) extends Expr(src) {
    def freeVars = ListSet.empty
    def subst(v1: String, e: Expr): Expr = this
    def occurrenceFV(v1: String): Int = 0
    protected def transformSubexprs(transf: Expr => Expr): Expr = this
  }
  case class Tuple(comps: List[Expr], override val src: Range) extends Expr(src) {
    def freeVars = {
      val fvs = comps.map(_.freeVars)
      fvs.foldRight(ListSet.empty[String])((fv1, fv2) => fv1.union(fv2))
    }
    def subst(v1: String, e: Expr): Expr =
      Tuple(comps.map(_.subst(v1, e)), src)
    def occurrenceFV(v1: String): Int =
      comps.map(_.occurrenceFV(v1)).foldRight(0)(_ + _)
    protected def transformSubexprs(transf: Expr => Expr): Expr =
      Tuple(comps.map(transf), src)
  }
  case class Select(tup: Expr, idx: Int, override val src: Range) extends Expr(src) {
    def freeVars = tup.freeVars
    def subst(v1: String, e: Expr): Expr =
      Select(tup.subst(v1, e), idx, src)
    def occurrenceFV(v1: String): Int =
      tup.occurrenceFV(v1)
    protected def transformSubexprs(transf: Expr => Expr): Expr =
      Select(transf(tup), idx, src)
  }
  case class LetIn(v: String, rhs: Expr, body: Expr, override val src: Range) extends Expr(src) {
    def freeVars = rhs.freeVars union (body.freeVars - v)
    def subst(v1: String, e: Expr): Expr = {
      val rhs1 = rhs.subst(v1, e)
      if (v1 == v)
        LetIn(v, rhs1, body, src)
      else if (e.freeVars.contains(v1)) {
        val vFresh = pickFreshVar(v, e.freeVars union ListSet(v))
        LetIn(vFresh, rhs1, body.subst(v, Var(vFresh, noLocation)).subst(v1, e), src)
      } else
        LetIn(v, rhs1, body.subst(v1, e), src)
    }
    def occurrenceFV(v1: String): Int =
      if (v1 == v)
        rhs.occurrenceFV(v1)
      else
        rhs.occurrenceFV(v1) + body.occurrenceFV(v1)
    protected def transformSubexprs(transf: Expr => Expr): Expr =
      LetIn(v, transf(rhs), transf(body), src)
  }
  case class Cond(c: Expr, i: Expr, e: Expr, override val src: Range) extends Expr(src) {
    def freeVars = c.freeVars union i.freeVars union e.freeVars
    def subst(v1: String, e1: Expr): Expr =
      Cond(c.subst(v1, e1), i.subst(v1, e1), e.subst(v1, e1), src)
    def occurrenceFV(v1: String): Int =
      c.occurrenceFV(v1) + i.occurrenceFV(v1) + e.occurrenceFV(v1)
    protected def transformSubexprs(transf: Expr => Expr): Expr =
      Cond(transf(c), transf(i), transf(e), src)
  }
  case class AddPrim(x: Expr, y: Expr, override val src: Range) extends Expr(src) {
    def freeVars = x.freeVars union y.freeVars
    def subst(v1: String, e: Expr): Expr =
      AddPrim(x.subst(v1, e), y.subst(v1, e), src)
    def occurrenceFV(v1: String): Int =
      x.occurrenceFV(v1) + y.occurrenceFV(v1)
    protected def transformSubexprs(transf: Expr => Expr): Expr =
      AddPrim(transf(x), transf(y), src)
  }
  case class SubPrim(x: Expr, y: Expr, override val src: Range) extends Expr(src) {
    def freeVars = x.freeVars union y.freeVars
    def subst(v1: String, e: Expr): Expr =
      SubPrim(x.subst(v1, e), y.subst(v1, e), src)
    def occurrenceFV(v1: String): Int =
      x.occurrenceFV(v1) + y.occurrenceFV(v1)
    protected def transformSubexprs(transf: Expr => Expr): Expr =
      SubPrim(transf(x), transf(y), src)
  }
  case class MulPrim(x: Expr, y: Expr, override val src: Range) extends Expr(src) {
    def freeVars = x.freeVars union y.freeVars
    def subst(v1: String, e: Expr): Expr =
      MulPrim(x.subst(v1, e), y.subst(v1, e), src)
    def occurrenceFV(v1: String): Int =
      x.occurrenceFV(v1) + y.occurrenceFV(v1)
    protected def transformSubexprs(transf: Expr => Expr): Expr =
      MulPrim(transf(x), transf(y), src)
  }
  case class DivPrim(x: Expr, y: Expr, override val src: Range) extends Expr(src) {
    def freeVars = x.freeVars union y.freeVars
    def subst(v1: String, e: Expr): Expr =
      DivPrim(x.subst(v1, e), y.subst(v1, e), src)
    def occurrenceFV(v1: String): Int =
      x.occurrenceFV(v1) + y.occurrenceFV(v1)
    protected def transformSubexprs(transf: Expr => Expr): Expr =
      DivPrim(transf(x), transf(y), src)
  }
  case class EqPrim(x: Expr, y: Expr, override val src: Range) extends Expr(src) {
    def freeVars = x.freeVars union y.freeVars
    def subst(v1: String, e: Expr): Expr =
      EqPrim(x.subst(v1, e), y.subst(v1, e), src)
    def occurrenceFV(v1: String): Int =
      x.occurrenceFV(v1) + y.occurrenceFV(v1)
    protected def transformSubexprs(transf: Expr => Expr): Expr =
      EqPrim(transf(x), transf(y), src)
  }
  case class LtPrim(x: Expr, y: Expr, override val src: Range) extends Expr(src) {
    def freeVars = x.freeVars union y.freeVars
    def subst(v1: String, e: Expr): Expr =
      LtPrim(x.subst(v1, e), y.subst(v1, e), src)
    def occurrenceFV(v1: String): Int =
      x.occurrenceFV(v1) + y.occurrenceFV(v1)
    protected def transformSubexprs(transf: Expr => Expr): Expr =
      LtPrim(transf(x), transf(y), src)
  }

  sealed abstract class Type(val src: Range) {
    override def toString() = pretty(this)
    def ~=~(ty: Type): Boolean
  }
  case class TyNat(override val src: Range) extends Type(src) {
    def ~=~(ty: Type): Boolean =
      ty match {
        case TyNat(_) => true
        case TyUnknown(_) => true
        case _ => false
      }
  }
  case class TyBool(override val src: Range) extends Type(src) {
    def ~=~(ty: Type): Boolean =
      ty match {
        case TyBool(_) => true
        case TyUnknown(_) => true
        case _ => false
      }
  }
  case class TyArrow(arg: Type, res: Type, override val src: Range) extends Type(src) {
    def ~=~(ty: Type): Boolean =
      ty match {
        case TyArrow(ty1, ty2, _) => arg ~=~ ty1 && res ~=~ ty2
        case TyUnknown(_) => true
        case _ => false
      }
  }
  case class TyStar(comps: List[Type], override val src: Range) extends Type(src) {
    def ~=~(ty: Type): Boolean =
      ty match {
        case TyStar(comps2, _) =>
          if (comps.length == comps2.length)
            comps.zip(comps2).forall { case (ty1, ty2) => ty1 ~=~ ty2 }
          else
            false
        case TyUnknown(_) => true
        case _ => false
      }
  }
  case class TyUnknown(override val src: Range) extends Type(noLocation) {
    def ~=~(ty: Type): Boolean = true
  }

  /**
   * Pick a variable fresh wrt. the set of variables.
   *  The name of the fresh variable starts with v.
   */
  private def pickFreshVar(v: String, vars: ListSet[String]): String = {
    var count = 0
    while (vars.contains(v + count)) count = count + 1
    v + count
  }

  /** Position (line and column) in a source */
  case class Position(row: Int, col: Int) {
    def +(n: Int) = new Position(row, col + n)
    override def toString() = row + ":" + col
  }

  /** Dummy range for synthesized artifacts */
  val noLocation = Range(Position(-1, -1), Position(-1, -1))

  /** Range of positions in source */
  case class Range(from: Position, to: Position) {
    override def toString() = from + "--" + to
  }

  /** Transform from Scala Position to our own since the scanner uses the latter */
  object Position {
    def apply(p: input.Position) = new Position(p.line, p.column)
  }

  object Range {
    def apply(f: input.Position, t: input.Position) = new Range(Position(f), Position(t))
    def apply(f: input.Position, t: Position) = new Range(Position(f), t)
    def apply(f: Position, t: input.Position) = new Range(f, Position(t))
  }
}
