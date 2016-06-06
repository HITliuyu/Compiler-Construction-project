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

package de.tuberlin.uebb.comp1.moc.tests

import org.scalatest.FunSpec
import de.tuberlin.uebb.comp1.moc._
import scalax.file.{ Path, FileSystem, PathSet }
import scalax.io._
import scala.sys.SystemProperties
import java.io.File
import org.scalatest.Matchers

class InterpreterTest extends FunSpec with Matchers {
  val pathStr = TestSuiteDir.getDir + "/interpreter"
  val path: Path = Path.fromString(pathStr)
  val files = path ** ("*.mo")

  def interpretertest(p: Path) = {
    val expFile = Path.fromString(pathStr ++ "/" ++ p.name ++ ".expected")
    val expLines = expFile.string.lines
    val exp = expLines.next

    if (exp == "SUCCESS") {
      val expVal = expLines.next
      it("Interpreting " + p.name + " should succeed") {
        try {
          val res = SucceedInterpreter.start(p)
          assert(expVal == res.toString, "Wrong result, expected " + expVal + " but got " + res)
        } catch {
          case e: TestException => {
            fail("Received the error\n" + e.getMessage)
          }
          case e: Throwable => {
            fail("Caught an exception\n" + e.getMessage)
          }
        }
      }
    } else {
      it("Interpreting " + p.name + " should raise a runtime error") {
        try {
          val res = SucceedInterpreter.start(p)
          fail("No runtime error occured")
        } catch {
          case e: TestException => {
            val expAll = for (line <- expFile.string.lines) yield line
            if (expAll.mkString("\n") != e.getMessage) {
              info("DIFFERENCE in outputs")
              info("  Expected: " + exp)
              info("  Result:   " + e.getMessage)
            }
          }
          case e: Throwable =>
            fail("Caught an exception\n" + e.getMessage)
        }
      }
    }
  }

  describe("MOpal Interpreter") {
    files foreach { interpretertest(_) }
  }
}
