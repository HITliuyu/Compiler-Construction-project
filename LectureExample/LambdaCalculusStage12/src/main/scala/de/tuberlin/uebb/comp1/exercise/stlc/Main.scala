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
 *     isDotcumentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to enisDotrse or promote products
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

import scala.io.Source._
import de.tuberlin.uebb.comp1.exercise.shared.Scanner
import de.tuberlin.uebb.comp1.covm.COVM
import de.tuberlin.uebb.comp1.covm.instructions.Instruction
import de.tuberlin.uebb.comp1.covm.DebugTrace
import de.tuberlin.uebb.comp1.covm.DebugNone

/** Compiler driver */
object Main {
  private val debug = DebugNone // Debug level for COVM

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Nothing to do")
      return
    }
    val inputString = fromFile(args(0)).mkString
    Scanner.scan(inputString) match {
      case Left(msg) => println("Scanner error: " + msg)
      case Right(toks) => {
        CombinatorParser.parse(toks) match {
          case Right(e) => {
            println(e + "\n")
            CombinatorTypeChecker.typeCheck(e) match {
              case Left(msg) =>
                println("TYPE ERRORS\n" + msg)
              case Right(ty) =>
                println("*** Type: " + ty + "\n")
                val v = Interpreter.eval(e)
                println("*** Interpreter result: " + v + "\n")
                val instrs = CodeGenerator.codeGen(e)
                println("*** COVM code:\n" + showInstructions(instrs) + "\n")
                val covm = new COVM(debugLevel = debug)
                covm.start(instrs)
                print("*** COVM result: ")
                covm.getResult match {
                  case Left(msg) => println("ERROR: " + msg + "\n")
                  case Right(msg) => println(msg + "\n")
                }
                val eOpt = Optimizer.optimize(e)
                println("*** Optimized program:\n" + eOpt + "\n")
                CombinatorTypeChecker.typeCheck(eOpt) match {
                  case Left(msg) => println("ERROR IN OPTIMIZER\n" ++ msg)
                  case Right(ty) => {
                    println("*** Type: " + ty + "\n")
                    val vOpt = Interpreter.eval(eOpt)
                    println("*** Interpreter result: " + v + "\n")
                    val instrsOpt = CodeGenerator.codeGen(eOpt)
                    println("*** COVM code:\n" + showInstructions(instrsOpt) + "\n")
                    val covm = new COVM(debugLevel = debug)
                    covm.start(instrsOpt)
                    print("*** COVM result: ")
                    covm.getResult match {
                      case Left(msg) => println("ERROR: " + msg)
                      case Right(msg) => println(msg)
                    }
                  }
                }
            }
          }
          case Left(msg) => println("PARSE ERROR:\n" + msg)
        }
      }
    }
  }

  private def showInstructions(instrs: List[Instruction]): String = {
    val idxInstrs = (0 to instrs.length - 1).zip(instrs)
    idxInstrs.map({ case (n, i) => n + "\t" + i }).mkString("\n")
  }
}
