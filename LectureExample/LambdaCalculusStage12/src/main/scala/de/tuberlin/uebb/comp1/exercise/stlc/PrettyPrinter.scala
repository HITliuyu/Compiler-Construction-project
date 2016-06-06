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
import scala.util.parsing._
import scala.language.implicitConversions
import AbstractSyntax._

/** Pretty printer for the Simply Lambda Calculus */
object PrettyPrinter extends org.kiama.output.PrettyPrinter {
  def pretty(e: Expr): String = super.pretty(showExpr(e))
  def pretty(ty: Type): String = super.pretty(showType(ty))

  /** Convert an expression to a document using parentheses only where necessary */
  implicit def showExpr(t: Expr): Doc =
    t match {
      case Abs(v, ty, b, _) => "\\" <> v <> ":" <> showType(ty) <> "." <> nest(softline <> showExpr(b))
      case Fix(fun, tyArg, tyRes, arg, body, _) => "fix" <+> fun <> ":" <> showType(tyArg) <+> "->" <+>
        showType(tyRes) <> "." <> arg <> "." <> nest(softline <> showExpr(body))
      case LetIn(v, e, b, _) => "let" <+> v <+> "=" <+> showExpr(e) <+> "in" <@> showExpr(b)
      case Cond(v, e, b, _) =>
        "if" <+> v <+> "then" <> nest(line <> showExpr(e)) <@> "else" <> nest(line <> showExpr(b))
      case App(f, e, _) => showExprWP(isAtomicOrApp, f) <+> showExprWP(isAtomic, e)
      case Var(i, _) => i
      case Tuple(comps, _) => list(comps, "", showExpr)
      case Select(tup, idx, _) => showExprWP(isAtomic, tup) <> "." <> value(idx)
      case Num(v, _) => value(v)
      case Bool(v, _) => value(v)
      case AddPrim(x, y, _) => "add" <+> showExprWP(isAtomic, x) <+> showExprWP(isAtomic, y)
      case SubPrim(x, y, _) => "sub" <+> showExprWP(isAtomic, x) <+> showExprWP(isAtomic, y)
      case MulPrim(x, y, _) => "mul" <+> showExprWP(isAtomic, x) <+> showExprWP(isAtomic, y)
      case DivPrim(x, y, _) => "div" <+> showExprWP(isAtomic, x) <+> showExprWP(isAtomic, y)
      case EqPrim(x, y, _) => "eq" <+> showExprWP(isAtomic, x) <+> showExprWP(isAtomic, y)
      case LtPrim(x, y, _) => "lt" <+> showExprWP(isAtomic, x) <+> showExprWP(isAtomic, y)
    }

  /** Check if expression is atomic (like a variable or a constant) */
  def isAtomic(e: Expr): Boolean =
    e.isInstanceOf[Var] || e.isInstanceOf[Tuple] || e.isInstanceOf[Select] ||
      e.isInstanceOf[Num] || e.isInstanceOf[Bool]

  /** Atomic or application */
  def isAtomicOrApp(e: Expr): Boolean =
    e.isInstanceOf[App] || isAtomic(e)

  /** Enclose in parentheses only if p(e) is false */
  def showExprWP(p: Expr => Boolean, e: Expr): Doc =
    if (p(e))
      showExpr(e)
    else
      parens(showExpr(e))

  /** Convert a type to a document using parentheses only where necessary */
  implicit def showType(ty: Type): Doc =
    ty match {
      case TyNat(_) => "Nat"
      case TyBool(_) => "Bool"
      case TyArrow(arg, res, _) => showTypeWP(isAtomicType, arg) <+> "->" <+> showTypeWP(isAtomicTypeOrArrow, res)
      case TyStar(comps, _) => showTyStar(comps)
      case TyUnknown(_) => "?"
    }

  /** Convert the component types of a tuple to documents separated by * */
  def showTyStar(tys: List[Type]): Doc =
    tys match {
      case Nil => empty
      case ty1 :: Nil => showType(ty1)
      case ty1 :: ty2 :: tys3 => showType(ty1) <> "*" <> showTyStar(ty2 :: tys3)
    }

  /** Check if type is atomic (like Nat or tuple type) */
  def isAtomicType(ty: Type): Boolean =
    ty.isInstanceOf[TyNat] || ty.isInstanceOf[TyBool] || ty.isInstanceOf[TyStar]

  /** Atomic or arrow type */
  def isAtomicTypeOrArrow(ty: Type): Boolean =
    isAtomicType(ty) || ty.isInstanceOf[TyArrow]

  /** Enclose in parentheses only if p(e) is false */
  def showTypeWP(p: Type => Boolean, ty: Type): Doc =
    if (p(ty))
      showType(ty)
    else
      parens(showType(ty))
}
