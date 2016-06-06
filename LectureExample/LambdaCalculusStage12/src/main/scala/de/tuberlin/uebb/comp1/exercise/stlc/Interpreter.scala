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

/** Interpreter for the Simply Typed Lambda Calculus */
object Interpreter {
  import de.tuberlin.uebb.comp1.exercise.stlc.{ Values => V }
  import Values.Val
  import AbstractSyntax._

  def eval(e: Expr): V.Val = eval(Map(), e)

  private def eval(env: Map[String, Val], e: Expr): Val = e match {
    case Abs(v, ty, body, _) => V.Closure(v, body, env)
    case Fix(fun, tyArg, tyRes, arg, body, _) => V.RecClosure(fun, arg, body, env)
    case App(fun, arg, _) => apply(eval(env, fun), eval(env, arg))
    case Var(n, _) => env(n)
    case Num(v, _) => V.Num(v)
    case Bool(v, _) => V.Bool(v)
    case Tuple(c, _) => V.Tuple(c.map { eval(env, _) })
    case Select(tup, idx, _) => eval(env, tup).asInstanceOf[V.Tuple].comps(idx - 1)
    case LetIn(v, rhs, body, _) => eval(env + (v -> eval(env, rhs)), body)
    case Cond(c, i, e, _) => {
      val cc = eval(env, c)
      cc match {
        case V.Bool(true) => eval(env, i)
        case V.Bool(false) => eval(env, e)
      }
    }
    case AddPrim(x, y, _) => (eval(env, x), eval(env, y)) match {
      case (V.Num(x), V.Num(y)) => V.Num(x + y)
    }
    case SubPrim(x, y, _) => (eval(env, x), eval(env, y)) match {
      case (V.Num(x), V.Num(y)) => V.Num(x - y)
    }
    case MulPrim(x, y, _) => (eval(env, x), eval(env, y)) match {
      case (V.Num(x), V.Num(y)) => V.Num(x * y)
    }
    case DivPrim(x, y, _) => (eval(env, x), eval(env, y)) match {
      case (V.Num(x), V.Num(y)) => V.Num(x / y)
    }
    case EqPrim(x, y, _) => (eval(env, x), eval(env, y)) match {
      case (V.Num(x), V.Num(y)) => V.Bool(x == y)
    }
    case LtPrim(x, y, _) => (eval(env, x), eval(env, y)) match {
      case (V.Num(x), V.Num(y)) => V.Bool(x < y)
    }
  }

  private def apply(c: Val, arg: Val): Val = c match {
    case V.Closure(v, b, env) => eval(env + (v -> arg), b)
    case clos @ V.RecClosure(f, a, b, env) => eval(env + (a -> arg) + (f -> clos), b)
  }
}
