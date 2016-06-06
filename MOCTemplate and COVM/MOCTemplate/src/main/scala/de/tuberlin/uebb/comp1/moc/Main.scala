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

import scala.io.Source._
import scala.util.parsing.input.Positional
import de.tuberlin.uebb.comp1.covm.COVM
import java.io.FileNotFoundException

/** Main object for the μ-Opal-Compiler */

object Main {
  /** String containing the description, how to use this compiler */
  val usageString = "usage: <moc> [ -d ] [ -i ] [-S ] <source>.mo\n" +
    "  -d  enables the debug option\n" +
    "  -i  enables the interpreter option (the compiler will only start the interpreter)\n" +
    "  -S  the coder writes the code for the machine in a the file <source>.S"

  /**
   * Starts the compiler for the given file with the given options
   *
   * usage: <moc> [ -d ] [ -i ] [-S ] <source>
   *
   * <source> is source file to compile
   *
   * -d enables the debug option
   *
   * -i enables the interpreter option (the compiler will only start the interpreter)
   *
   * -S the coder writes the code for the machine in a the file moc.S
   */
  def main(args: Array[String]): Unit = {
    Options.generateOpts(args) match {
      case Left(diag) => println(diag)
      case Right(opts) =>
        if (compile(opts)) {
          System.exit(0)
        } else {
          System.exit(1)
        }
    }
  }

  private def compile(opts: Options): Boolean = {
    val fileName = opts.source + ".mo"
    try {
      val inputString = fromFile(fileName).mkString

      val scanRes = Scanner.scan(inputString, opts)
      scanRes match {
        case Left(err) => {
          println(err)
          false
        }
        case Right(ts) => Parser.parse(ts, opts) match {
          case Left(err) => {
            println(err)
            false
          }
          case Right(prog) => ContextChecker.check(prog, opts) match {
            case Some(err) => {
              println(err.mkString("\n"))
              false
            }
            case None => if (opts.interp) {
              println("Interpretation started ...")
              Interpreter.interpret(prog, opts) match {
                case Left(err) => {
                  println("Error in interpretation:\n" + err)
                  false
                }
                case Right(v) => {
                  println("MAIN = " + v)
                  true
                }
              }
            } else {
              println("Code generation started ...")
              CodeGenerator.compile(prog, opts) match {
                case Left(err) => {
                  println("Error in code generation:\n" + err)
                  false
                }
                case Right(code) => if (opts.output) {
                  val pw = new java.io.PrintWriter(new java.io.File(opts.source + ".S"))
                  try {
                    pw.write(code.mkString("\n"))
                    println("Wrote file " + opts.source + ".S")
                  } finally {
                    pw.close()
                  }
                  true
                } else {
                  println("Execution started ...")
                  val machine = new COVM(100, 100)
                  machine.start(code)
                  val res = machine.getResult match { case Left(msg) => msg; case Right(msg) => msg }
                  println("Main = " + res)
                  true
                }
              }
            }
          }
        }
      }
    } catch {
      case e: FileNotFoundException => {
        println("File " + fileName + " not found")
        false
      }
      case e: Throwable => throw e
    }
  }
}
