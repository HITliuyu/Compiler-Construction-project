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
 * */

package de.tuberlin.uebb.comp1.moc

import scala.language.implicitConversions
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

/** Scanner for μ-Opal based on Scala's combinators */

object Scanner extends RegexParsers with Parsers {
  /**
   * Starts scanner
   * @param in The input string
   * @param opts [[Options]] given as arguments to comiler
   * @return either an error message [[Diag]] or a list of tokens [[Token]]
   */
  def scan(in: String, opts: Options): Either[Diag, List[Token]] =
    parseAll(scanToks, new ParserString(in)) match {
      case Success(r, _) =>
        if (opts.debug) {
          val tokStrings = r.map(tok => tok.toString() + " (" + tok.getPosition + ")")
          println("Tokens:\n" + tokStrings.mkString("\n"))
        }; Right(r)
      case failure: NoSuccess => Left(Diag(failure.msg, Global))
    }

  private def scanToks: Parser[List[Token]] =
    (scanComment*) ~ (scanTok*) ~ scanEof ^^ {
      case _ ~ l ~ eof => {
        l :+ eof
      }
    }

  private def scanEof: Parser[Token] =
    (scanComment*) ~> positioned("""\z""".r ^^ { _ => EofT() })

  private def scanTok: Parser[Token] = (scanComment*) ~ (
    scanOpen |
    scanClose |
    scanComma |
    scanColon |
    scanDefAs |
    scanMain |
    scanDef |
    //
    scanIf |
    scanThen |
    scanElse |
    scanFi |
    //
    scanNum |
    scanTrue |
    scanFalse |
    //
    scanTBool |
    scanTNat |
    scanVar) ^^ { case _ ~ t => t }

  private def scanComment = commentRegex
  private def scanOpen = positioned(OpenLex.r ^^ { _ => OpenT() })
  private def scanClose = positioned(CloseLex.r ^^ { _ => CloseT() })
  private def scanComma = positioned(CommaLex.r ^^ { _ => CommaT() })
  private def scanColon = positioned(ColonLex.r ^^ { _ => ColonT() })
  private def scanDefAs = positioned(DefAsLex.r ^^ { _ => DefAsT() })
  private def scanMain = positioned(keyword(MainLex) ^^ { _ => MainT() })
  private def scanDef = positioned(keyword(DefLex) ^^ { _ => DefT() })

  private def scanIf = positioned(keyword(IfLex) ^^ { _ => IfT() })
  private def scanThen = positioned(keyword(ThenLex) ^^ { _ => ThenT() })
  private def scanElse = positioned(keyword(ElseLex) ^^ { _ => ElseT() })
  private def scanFi = positioned(keyword(FiLex) ^^ { _ => FiT() })

  // TODO: better error msg: regex `\z` expected means: identifiers must start with small letter
  private def scanVar: Parser[Token] = positioned(varRegex ^^ { n => VarT(n) })

  private def scanNum = positioned(numRegex ^^ { n => NumT(n.toInt) })
  private def scanTrue = positioned(keyword(TrueLex) ^^ { _ => TrueT() })
  private def scanFalse = positioned(keyword(FalseLex) ^^ { _ => FalseT() })

  private def scanTBool = positioned(keyword(TBoolLex) ^^ { _ => BoolT() })
  private def scanTNat = positioned(keyword(TNatLex) ^^ { _ => NatT() })

  private val commentRegex: Regex = """--[^\n]*([\n]|\z)""".r
  private val numRegex: Regex = """\d+""".r
  private val varRegex: Regex = """[a-zA-Z][a-zA-Z0-9]*""".r

  private def keyword(lex: String): Regex =
    (lex + """(?![a-zA-Z0-9])""").r // Negative look-ahead assertion to avoid recognizing DEFun as a keyword and an identifier
  
  private val OpenLex = """\("""
  private val CloseLex = """\)"""
  private val CommaLex = ""","""
  private val ColonLex = """\:"""
  private val DefAsLex = """=="""
  private val MainLex = """MAIN"""
  private val DefLex = """DEF"""
  private val IfLex = """IF"""
  private val ThenLex = """THEN"""
  private val ElseLex = """ELSE"""
  private val FiLex = """FI"""
  private val TrueLex = """true"""
  private val FalseLex = """false"""
  private val TBoolLex = """bool"""
  private val TNatLex = """nat"""
}
