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
import scala.io.Source._

/**
 * Parses the file with [[instructions.Instruction]]s and executes them within a [[COVM]].
 * If stacksize and/or heapsize are not given on the command line, the defaults are used.
 */

object Main {
  private val versionNumber = "0.1.0"

  /** Representation of command line options */
  private case class Options(
    input: String = "",
    heapSize: Int = 4096,
    stackSize: Int = 512,
    debugLevel: DebugLevel = DebugNone)

  /** Command line options parser */
  private val optParser = new scopt.OptionParser[Options]("<covm>") {
    head("COVM", versionNumber)
    opt[Int]('h', "heap-size") action {
      (n, c) => c.copy(heapSize = n)
    } validate {
      n => if (n > 4) success else failure("heap size must be at least 4")
    } text ("set heap size")
    opt[Int]('s', "stack-size") action {
      (n, c) => c.copy(stackSize = n)
    } validate {
      n => if (n > 2) success else failure("stack size must be at least 2")
    } text ("set stack size")
    opt[String]('d', "debug-level") action {
      (str, c) =>
        str match {
          case "none" => c.copy(debugLevel = DebugNone)
          case "dump" => c.copy(debugLevel = DebugDump)
          case "trace" => c.copy(debugLevel = DebugTrace)
        }
    } validate {
      str =>
        str match {
          case "none" => success
          case "dump" => success
          case "trace" => success
          case _ => failure("debug level must be `none', `trace', or `dump'")
        }
    } text ("set debug level (none, trace, dump)")

    arg[String]("<file>") action {
      (f, c) => c.copy(input = f)
    } text ("input file")
  }

  def main(args: Array[String]): Unit = {
    optParser.parse(args, Options()) map { opts =>
      val machine = new COVM(opts.stackSize, opts.heapSize, opts.debugLevel)
      val parser = new InstructionParser()
      val inputString = fromFile(opts.input).mkString
      parser.parseInstructions(inputString) match {
        case Right(code) => {
          try {
            machine.start(code)
            machine.printResult()
            System.exit(0)
          } catch {
            case t: Throwable => {
              t.printStackTrace
              System.exit(1)
            }
          }
        }
        case Left(err) => println(err)
      }
    } getOrElse {
      System.exit(1)
    }
  }
}
