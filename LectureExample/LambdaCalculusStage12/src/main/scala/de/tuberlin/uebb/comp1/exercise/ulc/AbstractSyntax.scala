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

import scala.util.parsing.input
import PrettyPrinter._

/** Abstract syntax of the Untyped Lambda Calculus */
object AbstractSyntax {
  abstract class Expr(val src: Range) {
    override def toString() = pretty(this)
  }
  case class Abs(v: String, body: Expr, override val src: Range) extends Expr(src)
  case class App(fun: Expr, arg: Expr, override val src: Range) extends Expr(src)
  case class Var(name: String, override val src: Range) extends Expr(src)
  case class Num(v: Int, override val src: Range) extends Expr(src)
  case class Bool(v: Boolean, override val src: Range) extends Expr(src)
  case class Tuple(comps: List[Expr], override val src: Range) extends Expr(src)
  case class Select(tup: Expr, idx: Int, override val src: Range) extends Expr(src)
  case class LetIn(v: String, rhs: Expr, body: Expr, override val src: Range) extends Expr(src)
  case class Cond(c: Expr, i: Expr, e: Expr, override val src: Range) extends Expr(src)
  case class AddPrim(x: Expr, y: Expr, override val src: Range) extends Expr(src)
  case class SubPrim(x: Expr, y: Expr, override val src: Range) extends Expr(src)
  case class MulPrim(x: Expr, y: Expr, override val src: Range) extends Expr(src)
  case class DivPrim(x: Expr, y: Expr, override val src: Range) extends Expr(src)
  case class EqPrim(x: Expr, y: Expr, override val src: Range) extends Expr(src)
  case class LtPrim(x: Expr, y: Expr, override val src: Range) extends Expr(src)

  /** Position (line and column) in a source */
  case class Position(row: Int, col: Int) {
    def +(n: Int) = new Position(row, col + n)
    override def toString() = row + ":" + col
  }

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

  /** Types (used only in the type checker, no surface syntax) */
  sealed abstract class Type() {
    override def toString() = pretty(this)
  }
  case object TyWildCard extends Type // Necessary to generate equations for select (wildcard ranges over list of types)
  case object TyNat extends Type
  case object TyBool extends Type
  case class TyArrow(fun: Type, res: Type) extends Type
  case class TyStar(comps: List[Type]) extends Type
  case class TyVar(name: String) extends Type
}
