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
 * */

package de.tuberlin.uebb.comp1.exercise.ulc

import AbstractSyntax._

/**
 * Type checker for the Untyped Lambda Calculus using type
 *  inference for Simple Types
 */
object TypeChecker {
  import Mgu._

  private abstract class ExprTV(val tv: String)
  private case class AbsTV(v: String, tvArg: String, body: ExprTV, override val tv: String) extends ExprTV(tv)
  private case class AppTV(fun: ExprTV, arg: ExprTV, override val tv: String) extends ExprTV(tv)
  private case class VarTV(name: String, override val tv: String) extends ExprTV(tv)
  private case class NumTV(v: Int, override val tv: String) extends ExprTV(tv)
  private case class BoolTV(v: Boolean, override val tv: String) extends ExprTV(tv)
  private case class TupleTV(comps: List[ExprTV], override val tv: String) extends ExprTV(tv)
  private case class SelectTV(tup: ExprTV, idx: Int, override val tv: String) extends ExprTV(tv)
  private case class LetInTV(v: String, tvR: String, rhs: ExprTV, body: ExprTV, override val tv: String) extends ExprTV(tv)
  private case class CondTV(c: ExprTV, i: ExprTV, e: ExprTV, override val tv: String) extends ExprTV(tv)
  private case class AddPrimTV(x: ExprTV, y: ExprTV, override val tv: String) extends ExprTV(tv)
  private case class SubPrimTV(x: ExprTV, y: ExprTV, override val tv: String) extends ExprTV(tv)
  private case class MulPrimTV(x: ExprTV, y: ExprTV, override val tv: String) extends ExprTV(tv)
  private case class DivPrimTV(x: ExprTV, y: ExprTV, override val tv: String) extends ExprTV(tv)
  private case class EqPrimTV(x: ExprTV, y: ExprTV, override val tv: String) extends ExprTV(tv)
  private case class LtPrimTV(x: ExprTV, y: ExprTV, override val tv: String) extends ExprTV(tv)

  /** Type inference */
  def typeOf(e: Expr): Either[String, Type] =
    if (checkVariables(Set(), e)) {
      val eTV = annotateTVs(e)
      val eqns = genEqns(Map(), eTV)
      println(eqns.mkString("*** Equations:\n", "\n", ""))
      mgu(eqns) match {
        case Right(res) => {
          val subst = performComposition(res)
          println(subst.mkString("*** Substitution:\n", "\n", ""))
          println("*** Result type variable: " + eTV.tv)
          Right(apply(subst, TyVar(eTV.tv)))
        }
        case Left(msg) => Left(msg)
      }
    } else
      Left("the input contains some undeclared variables")

  /** Check that all variables are declared */
  private def checkVariables(names: Set[String], e: Expr): Boolean = e match {
    case Abs(v, b, s) => checkVariables(names + v, b)
    case App(f, a, s) => checkVariables(names, f) && checkVariables(names, a)
    case Var(n, s) => names.contains(n)
    case Num(v, s) => true
    case Bool(v, s) => true
    case Cond(c, i, e, s) => checkVariables(names, c) && checkVariables(names, i) && checkVariables(names, e)
    case LetIn(s, r, b, _) => checkVariables(names, r) && checkVariables(names + s, b)
    case Tuple(comps, s) => comps.foldLeft(true)({ case (z, c) => z && checkVariables(names, c) })
    case Select(tup, idx, s) => checkVariables(names, tup)
    case AddPrim(x, y, _) => checkVariables(names, x) && checkVariables(names, y)
    case SubPrim(x, y, _) => checkVariables(names, x) && checkVariables(names, y)
    case MulPrim(x, y, _) => checkVariables(names, x) && checkVariables(names, y)
    case DivPrim(x, y, _) => checkVariables(names, x) && checkVariables(names, y)
    case EqPrim(x, y, _) => checkVariables(names, x) && checkVariables(names, y)
    case LtPrim(x, y, _) => checkVariables(names, x) && checkVariables(names, y)
  }

  /** Generation of fresh type variables */
  private var count = 0

  private def nextTV = {
    count = count + 1
    "tau" + (count - 1)
  }

  /** Annotate expression with type variables */
  private def annotateTVs(e: Expr): ExprTV =
    e match {
      case Abs(v, body, _) => {
        val tv1 = nextTV
        val tv2 = nextTV
        AbsTV(v, tv2, annotateTVs(body), tv1)
      }
      case App(fun, arg, _) => {
        val tv1 = nextTV
        AppTV(annotateTVs(fun), annotateTVs(arg), tv1)
      }
      case Var(name, _) => {
        val tv1 = nextTV
        VarTV(name, tv1)
      }
      case Num(v, _) => {
        val tv1 = nextTV
        NumTV(v, tv1)
      }
      case Bool(v, _) => {
        val tv1 = nextTV
        BoolTV(v, tv1)
      }
      case Cond(c, i, e, _) => {
        val tv1 = nextTV
        CondTV(annotateTVs(c), annotateTVs(i), annotateTVs(e), tv1)
      }
      case LetIn(v, r, b, _) => {
        val tv1 = nextTV
        val tv2 = nextTV
        LetInTV(v, tv1, annotateTVs(r), annotateTVs(b), tv2)
      }
      case Tuple(comps, _) => {
        val tv1 = nextTV
        TupleTV(comps.map(annotateTVs), tv1)
      }
      case Select(tup, idx, _) => {
        val tv1 = nextTV
        SelectTV(annotateTVs(tup), idx, tv1)
      }
      case AddPrim(x, y, s) => {
        val tv1 = nextTV
        AddPrimTV(annotateTVs(x), annotateTVs(y), tv1)
      }
      case SubPrim(x, y, s) => {
        val tv1 = nextTV
        SubPrimTV(annotateTVs(x), annotateTVs(y), tv1)
      }
      case MulPrim(x, y, s) => {
        val tv1 = nextTV
        MulPrimTV(annotateTVs(x), annotateTVs(y), tv1)
      }
      case DivPrim(x, y, s) => {
        val tv1 = nextTV
        DivPrimTV(annotateTVs(x), annotateTVs(y), tv1)
      }
      case EqPrim(x, y, s) => {
        val tv1 = nextTV
        EqPrimTV(annotateTVs(x), annotateTVs(y), tv1)
      }
      case LtPrim(x, y, s) => {
        val tv1 = nextTV
        LtPrimTV(annotateTVs(x), annotateTVs(y), tv1)
      }
    }

  /** Generate equations. Assumes that all variables are declared. */
  private def genEqns(env: Map[String, Type], etv: ExprTV): List[=?] =
    etv match {
      case AbsTV(v, tvArg, body, tv1) =>
        (TyVar(tv1) =? TyArrow(TyVar(tvArg), TyVar(body.tv))) ::
          genEqns(env + (v -> TyVar(tvArg)), body)
      case AppTV(fun, arg, tv1) =>
        (TyVar(fun.tv) =? TyArrow(TyVar(arg.tv), TyVar(tv1))) ::
          (genEqns(env, fun) ++ genEqns(env, arg))
      case VarTV(name, tv) => (TyVar(tv) =? env(name)) :: Nil
      case NumTV(v, tv) => (TyVar(tv) =? TyNat) :: Nil
      case BoolTV(v, tv) => (TyVar(tv) =? TyBool) :: Nil
      case TupleTV(comps, tv) =>
        (TyVar(tv) =? TyStar(comps.map(x => TyVar(x.tv)))) ::
          comps.flatMap(genEqns(env, _))
      case LetInTV(v, tvR, r, b, tv) =>
        (TyVar(tv) =? TyVar(b.tv)) ::
          (TyVar(tvR) =? TyVar(r.tv)) ::
          genEqns(env, r) ++ genEqns(env + (v -> TyVar(tvR)), b)
      case CondTV(c, i, e, tv) =>
        (TyVar(c.tv) =? TyBool) ::
          (TyVar(i.tv) =? TyVar(e.tv)) ::
          (TyVar(tv) =? TyVar(i.tv)) ::
          genEqns(env, c) ++ genEqns(env, i) ++ genEqns(env, e)
      case SelectTV(tup, i, tv) => {
        val tvs = (1 to i - 1).toList.map(_ => TyVar(nextTV))
        (TyVar(tup.tv) =? TyStar(tvs ++ List(TyVar(tv), TyWildCard))) ::
          genEqns(env, tup)
      }
      case AddPrimTV(x, y, tv) =>
        (TyVar(tv) =? TyNat) ::
          (TyVar(x.tv) =? TyNat) ::
          (TyVar(y.tv) =? TyNat) ::
          genEqns(env, x) ++ genEqns(env, y)
      case SubPrimTV(x, y, tv) =>
        (TyVar(tv) =? TyNat) ::
          (TyVar(x.tv) =? TyNat) ::
          (TyVar(y.tv) =? TyNat) ::
          genEqns(env, x) ++ genEqns(env, y)
      case MulPrimTV(x, y, tv) =>
        (TyVar(tv) =? TyNat) ::
          (TyVar(x.tv) =? TyNat) ::
          (TyVar(y.tv) =? TyNat) ::
          genEqns(env, x) ++ genEqns(env, y)
      case DivPrimTV(x, y, tv) =>
        (TyVar(tv) =? TyNat) ::
          (TyVar(x.tv) =? TyNat) ::
          (TyVar(y.tv) =? TyNat) ::
          genEqns(env, x) ++ genEqns(env, y)
      case EqPrimTV(x, y, tv) =>
        (TyVar(tv) =? TyBool) ::
          (TyVar(x.tv) =? TyNat) ::
          (TyVar(y.tv) =? TyNat) ::
          genEqns(env, x) ++ genEqns(env, y)
      case LtPrimTV(x, y, tv) =>
        (TyVar(tv) =? TyBool) ::
          (TyVar(x.tv) =? TyNat) ::
          (TyVar(y.tv) =? TyNat) ::
          genEqns(env, x) ++ genEqns(env, y)
    }
}
