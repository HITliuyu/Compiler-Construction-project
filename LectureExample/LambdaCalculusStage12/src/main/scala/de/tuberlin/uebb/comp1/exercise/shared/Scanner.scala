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

import scala.util.parsing.combinator.RegexParsers
import de.tuberlin.uebb.comp1.exercise.shared.Tokens._
import scala.util.matching.Regex
import scala.util.parsing.combinator.Parsers

/**
 * Scanner for the Simply Typed Lambda Calculus and the Untyped
 *  Lambda Calculus. It creates some tokens not part of the Untyped
 *  Calculus.
 */
object Scanner extends RegexParsers with Parsers {
  override protected val whiteSpace = """(\s|--.*\n)+""".r

  def scan(in: String): Either[String, List[Token]] =
    parseAll(scanToks, in) match {
      case Error(msg, in) => {
        val source = in.source
        val offset = in.offset
        val start = handleWhiteSpace(source, offset)
        val found =
          if (start == source.length()) "end of input"
          else "`" + source.charAt(start) + "'"
        Left(Failure("expected valid token, but " + found + " found", in).toString)
      }
      case Success(r, _) => Right(r :+ Eof)
      case Failure(msg, in) => {
        val source = in.source
        val offset = in.offset
        val start = handleWhiteSpace(source, offset)
        val found =
          if (start == source.length()) "end of input"
          else "`" + source.charAt(start) + "'"
        Left(Failure("expected valid token, but " + found + " found", in).toString)
      }
    }

  def scanToks: Parser[List[Token]] =
    rep(scanTok) ^^ { l =>
      val e = Eof
      if (!l.isEmpty)
        e.setPos(l.last.pos)
      l :+ e
    }

  def scanTok: Parser[Token] = scanOpen |
    scanClose |
    scanLambda |
    scanDot |
    scanComma |
    scanIf |
    scanThen |
    scanElse |
    scanTrue |
    scanFalse |
    scanNum |
    scanFix |
    scanLet |
    scanIn |
    scanEq |
    scanColon |
    scanArrow |
    scanStar |
    scanTyNat |
    scanTyBool |
    scanAddPrim |
    scanMulPrim |
    scanDivPrim |
    scanSubPrim |
    scanEqPrim |
    scanLtPrim |
    scanVar

  def scanOpen = positioned(openLex.r ^^ { _ => Open })
  def scanAddPrim = positioned((addPrimLex + delimiter).r ^^ { _ => AddPrim })
  def scanSubPrim = positioned((subPrimLex + delimiter).r ^^ { _ => SubPrim })
  def scanMulPrim = positioned((mulPrimLex + delimiter).r ^^ { _ => MulPrim })
  def scanDivPrim = positioned((divPrimLex + delimiter).r ^^ { _ => DivPrim })
  def scanEqPrim = positioned(eqPrimLex.r ^^ { _ => EqPrim })
  def scanLtPrim = positioned(ltPrimLex.r ^^ { _ => LtPrim })
  def scanClose = positioned(closeLex.r ^^ { _ => Close })
  def scanLambda = positioned(lambdaLex.r ^^ { _ => Lambda })
  def scanDot = positioned(dotLex.r ^^ { _ => Dot })
  def scanComma = positioned(commaLex.r ^^ { _ => Comma })
  def scanIf = positioned((ifLex + delimiter).r ^^ { _ => If })
  def scanThen = positioned((thenLex + delimiter).r ^^ { _ => Then })
  def scanElse = positioned((elseLex + delimiter).r ^^ { _ => Else })
  def scanTrue = positioned((trueLex + delimiter).r ^^ { _ => True })
  def scanFalse = positioned((falseLex + delimiter).r ^^ { _ => False })
  def scanNum = positioned(numRegex ^^ { n => Num(n.toInt) })
  def scanVar = positioned(varRegex ^^ { n => Var(n) })
  def scanFix = positioned((fixLex + delimiter).r ^^ { _ => Fix })
  def scanLet = positioned((letLex + delimiter).r ^^ { _ => Let })
  def scanIn = positioned((inLex + delimiter).r ^^ { _ => In })
  def scanEq = positioned(eqLex.r ^^ { _ => Eq })
  def scanColon = positioned(colonLex.r ^^ { _ => Colon })
  def scanArrow = positioned(arrowLex.r ^^ { _ => Arrow })
  def scanStar = positioned(starLex.r ^^ { _ => Star })
  def scanTyNat = positioned((natLex + delimiter).r ^^ { _ => Nat })
  def scanTyBool = positioned((boolLex + delimiter).r ^^ { _ => Bool })

  def delimiter: Regex = """(?=\s|\.|\)|,|\*|-|\(|\\|:)""".r
  val numRegex: Regex = """\d+""".r
  val varRegex: Regex = """[a-z][a-zA-Z0-9]*""".r
}
