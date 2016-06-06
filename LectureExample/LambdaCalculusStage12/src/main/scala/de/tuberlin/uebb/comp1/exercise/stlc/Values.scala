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

import de.tuberlin.uebb.comp1.exercise.stlc.AbstractSyntax.Expr

/** Values of the Simply Typed Lambda Calculus */
object Values {
  abstract class Val
  case class Num(v: Int) extends Val {
    override def toString() = v.toString
  }
  case class Bool(b: Boolean) extends Val {
    override def toString() = b.toString
  }
  case class Closure(v: String, body: Expr, env: Map[String, Val]) extends Val {
    override def toString() =
      "Closure(" + v + "," + body + "," + env + ")"
  }
  case class RecClosure(fun: String, arg: String, body: Expr, env: Map[String, Val]) extends Val {
    override def toString() =
      "RecClosure(" + fun + "," + arg + "," + body + "," + env + ")"
  }
  case class Tuple(comps: List[Val]) extends Val {
    override def toString() = comps.mkString("(", ",", ")")
  }
}
