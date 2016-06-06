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

import de.tuberlin.uebb.comp1.moc._
import de.tuberlin.uebb.comp1.covm._

import scalax.file.{ Path, FileSystem, PathSet }

class TestException(msg: String) extends RuntimeException(msg)

object DefaultOptions extends Options(false, false, false, "default")

object TestSuiteDir {
  def getDir: String =
    scala.util.Properties.envOrElse("MOC_TESTSUITE_DIR", "src/test/resources")
}

object SucceedParser {
  def parse(path: Path) = {
    val inStr = path.string
    val parseRes =
      Scanner.scan(inStr, DefaultOptions).fold(Left(_), Parser.parse(_, DefaultOptions))
    parseRes match {
      case Right(res) => res
      case Left(diag) => throw new TestException(diag.toString)
    }
  }
}

object SucceedChecker {
  def check(path: Path) = {
    val pRes = SucceedParser.parse(path)
    ContextChecker.check(pRes, DefaultOptions) match {
      case None => pRes
      case Some(diags) => throw new TestException(diags.mkString("\n"))
    }
  }
}

object SucceedInterpreter {
  def start(path: Path): Value = {
    Interpreter.interpret(SucceedChecker.check(path), DefaultOptions) match {
      case Left(diag) => throw new TestException(diag.toString)
      case Right(value) => value
    }
  }
}

object SucceedCodeGenerator {
  def start(path: Path): CodeGenerator.Code = {
    CodeGenerator.compile(SucceedChecker.check(path), DefaultOptions) match {
      case Left(diag) => throw new TestException(diag.toString)
      case Right(code) => code
    }
  }
}

object SucceedExec {
  def start(path: Path): Int = {
    val machine = new COVM(10000, 1000)
    val code = SucceedCodeGenerator.start(path)
    machine.start(code)
    machine.getResult match {
      case Right(res) => res.toInt
      case _ => throw new TestException("should succeed")
    }
  }
}

object FailExec {
  def start(path: Path): Unit = {
    val covm = new COVM(10000, 1000)
    val code = SucceedCodeGenerator.start(path)
    covm.start(code)
  }
}
