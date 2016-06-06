/*
 * Copyright (c) 2012, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following
 *   disclaimer in the documentation and/or other materials provided
 *   with the distribution.
 * - Neither the name of the TU Berlin nor the names of its
 *   contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package de.tuberlin.uebb.comp1.covm

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex
import instructions._

/**
 * This class provides a parser for [[instructions.Instruction]]s.
 *
 * Each [[instructions.Instruction]] is parsed exactly as given by the toString method of the [[instructions.Instruction]]s.
 * In between [[instructions.Instruction]] are arbitrary white space characters allowed.
 *
 * [[instructions.Label]] and [[instructions.LabelledAddress]] are not recognized by this parser, since the [[COVM]] can't handle them.
 */
class InstructionParser extends RegexParsers with Parsers {

  /**
   * Parses a String and returns either a List of [[instructions.Instruction]]s
   * or a String containing an error.
   */
  def parseInstructions(in: String): Either[String, List[Instruction]] =
    parseAll(parseAllInst, in) match {
      case Success(result, _) => Right(result)
      case NoSuccess(err, next) => Left(err)
    }
  private def parseAllInst = rep1(parseInst, parseInst)
  private def parseInst = parseCall |
    parseRet |
    parsePushInt |
    parsePushAddr |
    parsePush |
    parseSlide |
    parseSwap |
    parseAdd |
    parseSub |
    parseMul |
    parseDiv |
    parsePack |
    parseUnpack |
    parseJmp |
    parseJz |
    parseJlt |
    parseJgt |
    parseAbort |
    parseStop

  private def parseCall = "call" ^^ { _ => Call }
  private def parseRet = "ret" ^^ { _ => Ret }
  private def parsePushInt = "pushint" ~> intRegex ^^ { x => PushInt(x.toInt) }
  private def parsePushAddr = "pushaddr" ~> nnIntRegex ^^ { x => PushAddr(Pointer(x.toInt)) }
  private def parsePush = "push" ~> nnIntRegex ^^ { x => Push(x.toInt) }
  private def parseSlide = "slide" ~> nnIntRegex ^^ { x => Slide(x.toInt) }
  private def parseSwap = "swap" ^^ { _ => Swap }
  private def parseAdd = "add" ^^ { _ => Add }
  private def parseSub = "sub" ^^ { _ => Sub }
  private def parseMul = "mul" ^^ { _ => Mul }
  private def parseDiv = "div" ^^ { _ => Div }
  private def parsePack = "pack" ~> nnIntRegex ^^ { x => Pack(x.toInt) }
  private def parseUnpack = "unpack" ~> nnIntRegex ^^ { x => Unpack(x.toInt) }
  private def parseJmp = "jmp" ~> nnIntRegex ^^ { x => Jmp(Pointer(x.toInt)) }
  private def parseJz = "jz" ~> nnIntRegex ^^ { x => Jz(Pointer(x.toInt)) }
  private def parseJlt = "jlt" ~> nnIntRegex ^^ { x => Jlt(Pointer(x.toInt)) }
  private def parseJgt = "jgt" ~> nnIntRegex ^^ { x => Jgt(Pointer(x.toInt)) }
  private def parseStop = "stop" ^^ { _ => Stop }
  private def parseAbort = "abort" ~> stringRegex ^^ { x => Abort(x) }

  private val intRegex: Regex = """-?\d+""".r
  private val nnIntRegex: Regex = """\d+""".r
  private def stringRegex: Parser[String] = """"(\\"|[^"])*"""".r ^^ { (s: String) => s.substring(1, s.length() - 1) }
  protected override val whiteSpace: Regex = "( |\n|\r|\t|--.*\n)*".r
}
