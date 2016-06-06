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

package de.tuberlin.uebb.comp1.moc

/**
 * Option class represents the options for the current run of the μ-Opal-Compiler
 *
 *  @param debug  if set, debug information is printed
 *  @param interp  if set, interpreter is started and no code generation
 *  @param output if set, the code is written in [source].S file
 *  @param source name of source file
 */
case class Options(debug: Boolean, interp: Boolean, output: Boolean, source: String)

object Options {
  var debug: Boolean = false
  var inter: Boolean = false
  var output: Boolean = false

  def generateOpts(args: Array[String]): Either[Diag, Options] = {

    if (args.length == 0) {
      Left(Diag("no file found\n\n" + Main.usageString, Global))
    } else {
      if (args(0) == "-d") {
        debug = true
        generateOpts(args.drop(1))
      } else if (args(0) == "-S") {
        output = true
        generateOpts(args.drop(1))
      } else if (args(0) == "-i") {
        inter = true
        generateOpts(args.drop(1))
      } else {
        if (args.length == 1) {
          val parts = args(0).split("""\.""")
          if (parts.last == "mo") {
            Right(Options(debug, inter, output, parts.init.mkString(".")))
          } else {
            Left(Diag("source file must end with .mo\n\n" + Main.usageString, Global))
          }
        } else {
          Left(Diag("unknown option: " ++ args(0) + "\n\n" + Main.usageString, Global))
        }
      }
    }
  }
}
