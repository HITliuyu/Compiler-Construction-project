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
 * */

package de.tuberlin.uebb.comp1.exercise.stlc

/** Type checker for the Simply Typed Lambda Calculus */
object CombinatorTypeChecker {
  import AbstractSyntax._
  import TypeCheckerCombinators._

  def typeCheck(e: Expr): Either[String, Type] =
    typeOf(Map(), e) match {
      case Ok(ty) => Right(ty)
      case Fail(_, errors) => Left(errors.mkString("\n"))
    }

  private type Env = Map[String, Type]

  private def typeOf(env: Env, e: Expr): TypeChecker[Type, String] =
    e match {
      case Abs(v, ty, body, src) =>
        for (ty2 <- typeOf(env + (v -> ty), body))
          yield TyArrow(ty, ty2, noLocation)
      case Fix(fun, tyArg, tyRes, arg, body, src) => {
        val tyFun = TyArrow(tyArg, tyRes, noLocation)
        for (
          ty2 <- typeOf(env + (fun -> tyFun) + (arg -> tyArg), body);
          res <- if (ty2 ~=~ tyRes)
            ok(tyFun)
          else
            fail("function body not of declared type at " + src)
        ) yield res
      }
      case App(fun, arg, src) => for (
        ty1 <- typeOf(env, fun);
        ty2 <- typeOf(env, arg);
        result <- ty1 match {
          case TyArrow(ty11, ty12, _) =>
            if (ty11 ~=~ ty2)
              ok(ty12)
            else
              fail("argument type mismatch at " + arg.src + " (" + ty2 + " /= " + ty11 + ")")
          case TyUnknown(_) => continue
          case _ => fail("expected a function at " + fun.src)
        }
      ) yield result
      case Var(name, src) =>
        if (!env.contains(name))
          fail("unknown variable at " + src)
        else
          ok(env(name))
      case Num(_, _) => ok(TyNat(noLocation))
      case Bool(_, _) => ok(TyBool(noLocation))
      case Tuple(comps, src) => for (
        tys <- typesOf(env, comps)
      ) yield TyStar(tys, noLocation)
      case Select(tup, idx, src) =>
        for (
          ty <- typeOf(env, tup);
          res <- ty match {
            case TyStar(tys, _) =>
              if (idx <= tys.length && idx > 0)
                ok(tys(idx - 1))
              else
                fail("selection index out of range at " + src)
            case TyUnknown(_) => continue
            case _ => fail("selection from something no a tuple at " + src)
          }
        ) yield res
      case LetIn(v, rhs, body, src) => for (
        ty1 <- typeOf(env, rhs);
        ty2 <- typeOf(env + (v -> ty1), body)
      ) yield ty2
      case Cond(c, i, e, src) => for (
        ty1 <- typeOf(env, c);
        ty2 <- typeOf(env, i);
        ty3 <- typeOf(env, e);
        res <- ty1 match {
          case TyBool(_) =>
            if (ty2 ~=~ ty3)
              ok(ty2)
            else
              fail("then and else branch have different types at " + src + " (" + ty2 + " /= " + ty3 + ")")
          case TyUnknown(_) => continue
          case _ => fail("condition must be Bool at " + src)
        }
      ) yield res
      case AddPrim(x, y, src) => checkPrim(env, x, y, src, TyNat(noLocation))
      case SubPrim(x, y, src) => checkPrim(env, x, y, src, TyNat(noLocation))
      case MulPrim(x, y, src) => checkPrim(env, x, y, src, TyNat(noLocation))
      case DivPrim(x, y, src) => checkPrim(env, x, y, src, TyNat(noLocation))
      case EqPrim(x, y, src) => checkPrim(env, x, y, src, TyBool(noLocation))
      case LtPrim(x, y, src) => checkPrim(env, x, y, src, TyBool(noLocation))
    }

  private def checkPrim(env: Env, x: Expr, y: Expr, src: Range, ty: Type): TypeChecker[Type, String] =
    for (
      ty1 <- typeOf(env, x);
      ty2 <- typeOf(env, y);
      res <- if (ty1 ~=~ TyNat(noLocation) && ty2 ~=~ TyNat(noLocation))
        ok(ty)
      else
        fail("arguments to arithmetic primitive must be Nat at " + src)
    ) yield res

  def typesOf(env: Env, es: List[Expr]): TypeChecker[List[Type], String] =
    es match {
      case Nil => ok(Nil)
      case e :: es =>
        for (
          ty1 <- typeOf(env, e);
          tys2 <- typesOf(env, es);
          res <- ok(ty1 :: tys2)
        ) yield res
    }

  private def fail(msg: String): TypeChecker[Type, String] =
    TypeCheckerCombinators.fail(TyUnknown(noLocation), msg)

  private def continue: TypeChecker[Type, String] =
    TypeCheckerCombinators.continue(TyUnknown(noLocation))

  private def ok(ty: Type): TypeChecker[Type, String] =
    TypeCheckerCombinators.ok(ty)

  private def ok(tys: List[Type]): TypeChecker[List[Type], String] =
    TypeCheckerCombinators.ok(tys)
}
