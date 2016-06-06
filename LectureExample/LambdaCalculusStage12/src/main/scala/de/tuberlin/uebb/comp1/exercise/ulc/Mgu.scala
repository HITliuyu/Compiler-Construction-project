
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

package de.tuberlin.uebb.comp1.exercise.ulc

/** Type equation solving using unification */
object Mgu {
  import AbstractSyntax._

  /** A type equation */
  case class =?(lhs: Type, rhs: Type) {
    override def toString() = lhs + " =? " + rhs
  }
  implicit class TypeEqnWithInfix(l: Type) { def =?(r: Type): =? = new =?(l, r) }

  /** Calculate most general unifier for a sequence of type equations */
  def mgu(eqns: List[=?]): Either[String, Sub] = mgu(Nil, eqns)

  private type Sub = List[(String, Type)]

  private def mgu(sub: Sub, eqns: List[=?]): Either[String, Sub] =
    (sub, eqns) match {
      case (sub, Nil) => Right(sub)
      case (sub, (l =? r) :: eq) => {
        val S1 = apply(sub, l)
        val T1 = apply(sub, r)
        mgu(sub, S1, T1, eq)
      }
    }

  private def mgu(sub: Sub, lhs: Type, rhs: Type, eqns: List[=?]): Either[String, Sub] =
    (lhs, rhs) match {
      case (TyVar(x1), TyVar(x2)) =>
        if (x1 == x2) mgu(sub, eqns)
        else mgu(sub :+ (x1 -> TyVar(x2)), eqns)
      case (TyVar(x), t) =>
        if (vars(t).contains(x)) Left("Occurs check failed " + vars(t) + " contains " + x)
        else mgu(sub :+ (x, t), eqns)
      case (l, TyVar(x)) => mgu(sub, TyVar(x), l, eqns)
      case (TyArrow(s1, s2), TyArrow(t1, t2)) =>
        mgu(sub, eqns :+ (s1 =? t1) :+ (s2 =? t2))
      case (TyNat, TyNat) => mgu(sub, eqns)
      case (TyBool, TyBool) => mgu(sub, eqns)
      case (TyStar(Nil), TyStar(Nil)) => mgu(sub, eqns)
      case (TyStar(TyWildCard :: Nil), TyStar(comps)) => mgu(sub, eqns)
      case (TyStar(comps), TyStar(TyWildCard :: Nil)) => mgu(sub, eqns)
      case (TyStar(l :: ls), TyStar(r :: rs)) =>
        mgu(sub, eqns :+ (l =? r) :+ (TyStar(ls) =? TyStar(rs)))
      case _ => Left("Unable to unify " + lhs + " and " + rhs)
    }

  /** Variables of a type */
  private def vars(t: Type): Set[String] = t match {
    case TyVar(x) => Set(x)
    case TyArrow(arg, res) => vars(arg) ++ vars(res)
    case TyStar(comps) => comps.foldLeft(Set[String]())({ case (se, t) => se ++ vars(t) })
    case _ => Set()
  }

  /** Compose a sequence of one-element substitutions */
  def performComposition(sub: Sub): Sub =
    sub match {
      case Nil => Nil
      case ((x, t) :: sub1) =>
        val t1 = apply(sub1, t);
        (x, t1) :: performComposition(sub1)
    }

  /** Apply a one-element substitution */
  private def apply(asub: (String, Type), ty: Type): Type =
    ty match {
      case TyArrow(arg, res) => TyArrow(apply(asub, arg), apply(asub, res))
      case TyStar(comps) => TyStar(comps.map(apply(asub, _)))
      case TyVar(n) if (n == asub._1) => asub._2
      case ty => ty
    }
  private def apply(ty: Type, asub: (String, Type)): Type = apply(asub, ty)

  /** Apply a substitution (i. e. a sequence of one-element substitutions) */
  def apply(sub: Sub, ty: Type): Type = sub.foldLeft(ty)(apply)
}
