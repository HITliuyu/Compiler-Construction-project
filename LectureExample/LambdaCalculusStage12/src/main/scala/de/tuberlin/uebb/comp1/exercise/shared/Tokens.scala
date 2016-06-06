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

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS """AS IS""" AND
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

package de.tuberlin.uebb.comp1.exercise.shared

import scala.util.parsing.input.Positional

/**
 * Tokens of the Simply Typed Lambda Calculus and the Untyped
 *  Lambda calculus. The latter does not use all of them.
 */
object Tokens {
  abstract class Token extends Positional {
    def isVar(): Boolean = this.isInstanceOf[Var]
    def isNum(): Boolean = this.isInstanceOf[Num]
  }
  case object Open extends Token {
    override def toString = "("
  }
  case object Close extends Token {
    override def toString = ")"
  }
  case object Lambda extends Token {
    override def toString = "\\"
  }
  case object Fix extends Token {
    override def toString = fixLex
  }
  case object Dot extends Token {
    override def toString = "."
  }
  case class Var(name: String) extends Token {
    override def toString = name
  }
  case object Comma extends Token {
    override def toString = commaLex
  }
  case object If extends Token {
    override def toString = ifLex
  }
  case object Then extends Token {
    override def toString = thenLex
  }
  case object Else extends Token {
    override def toString = elseLex
  }
  case object True extends Token {
    override def toString = trueLex
  }
  case object False extends Token {
    override def toString = falseLex
  }
  case object MulPrim extends Token {
    override def toString = mulPrimLex
  }
  case object AddPrim extends Token {
    override def toString = addPrimLex
  }
  case object DivPrim extends Token {
    override def toString = divPrimLex
  }
  case object SubPrim extends Token {
    override def toString = subPrimLex
  }
  case object EqPrim extends Token {
    override def toString = eqPrimLex
  }
  case object LtPrim extends Token {
    override def toString = ltPrimLex
  }
  case class Num(value: Int) extends Token {
    override def toString = value.toString
  }
  case object Eof extends Token {
    override def toString = "<EOF>"
  }
  case object Let extends Token {
    override def toString = letLex
  }
  case object In extends Token {
    override def toString = inLex
  }
  case object Eq extends Token {
    override def toString = eqLex
  }
  case object Colon extends Token {
    override def toString = colonLex
  }
  case object Arrow extends Token {
    override def toString = arrowLex
  }
  case object Star extends Token {
    override def toString = "*"
  }
  case object Nat extends Token {
    override def toString = natLex
  }
  case object Bool extends Token {
    override def toString = boolLex
  }

  val openLex = """\("""
  val closeLex = """\)"""
  val lambdaLex = """\\"""
  val fixLex = """fix"""
  val dotLex = """\."""
  val commaLex = ""","""
  val ifLex = """if"""
  val thenLex = """then"""
  val elseLex = """else"""
  val trueLex = """true"""
  val falseLex = """false"""
  val addPrimLex = """add"""
  val subPrimLex = """sub"""
  val mulPrimLex = """mul"""
  val divPrimLex = """div"""
  val eqPrimLex = """eq"""
  val ltPrimLex = """lt"""
  val letLex = """let"""
  val inLex = """in"""
  val eqLex = """="""
  val colonLex = """:"""
  val arrowLex = """->"""
  val starLex = """\*"""
  val natLex = """Nat"""
  val boolLex = """Bool"""
}
