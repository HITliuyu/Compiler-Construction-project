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
 */

package de.tuberlin.uebb.comp1.exercise.stlc

/** Combinators for type checkers to collect several error messages conveniently */
object TypeCheckerCombinators {
  /**
   * A type checker returning a valid result A or errors of type E.
   *  for expressions can be used for type checkers
   */
  sealed abstract class TypeChecker[A, E] {
    def map[B](f: A => B): TypeChecker[B, E]
    def flatMap[B](f: A => TypeChecker[B, E]): TypeChecker[B, E]
  }

  /** A type checker returning a valid result A */
  case class Ok[A, E](res: A) extends TypeChecker[A, E] {
    def map[B](f: A => B): TypeChecker[B, E] =
      Ok(f(res))
    def flatMap[B](f: A => TypeChecker[B, E]): TypeChecker[B, E] =
      f(res)
    def &[B](f: A => TypeChecker[B, E]): TypeChecker[B, E] =
      f(res)
  }

  /**
   * A type checker returning a list of error of type E. It also returns
   *  a result of type A. It is in the responsibility of the client code
   *  to treat this result not as a valid result.
   */
  case class Fail[A, E](res: A, errors: List[E]) extends TypeChecker[A, E] {
    def map[B](f: A => B): TypeChecker[B, E] =
      Fail(f(res), errors)
    def flatMap[B](f: A => TypeChecker[B, E]): TypeChecker[B, E] =
      f(res) match {
        case Ok(res1) => Fail(res1, errors)
        case Fail(res1, errors1) => Fail(res1, errors ++ errors1)
      }
    def &[B](f: A => TypeChecker[B, E]): TypeChecker[B, E] =
      f(res) match {
        case Ok(res1) => Fail(res1, errors)
        case Fail(res1, errors1) => Fail(res1, errors ++ errors1)
      }
  }

  /** Fail with given (invalid) result and an error */
  def fail[A, E](res: A, error: E): TypeChecker[A, E] =
    Fail(res, error :: Nil)

  /** Succeed with a valid result */
  def ok[A, E](res: A): TypeChecker[A, E] =
    Ok(res)

  /** Fail with the given (invalid) result but do not add another error */
  def continue[A, E](res: A): TypeChecker[A, E] =
    Fail(res, Nil)
}
