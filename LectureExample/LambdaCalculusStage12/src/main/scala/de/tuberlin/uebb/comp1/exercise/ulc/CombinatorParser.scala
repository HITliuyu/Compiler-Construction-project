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

package de.tuberlin.uebb.comp1.exercise.ulc

import de.tuberlin.uebb.comp1.exercise.shared.Tokens._
import de.tuberlin.uebb.comp1.parsing.ParserCombinators._
import de.tuberlin.uebb.comp1.parsing.ParserCombinators
import de.tuberlin.uebb.comp1.exercise.ulc.{ AbstractSyntax => AS }
import de.tuberlin.uebb.comp1.exercise.ulc.AbstractSyntax.Expr
import de.tuberlin.uebb.comp1.exercise.ulc.AbstractSyntax.Range

/* Parser for the Untyped Lambda Calculus */
object CombinatorParser {
  // Original EBNF
  // S ::= E '#'
  // E ::= '\' 'var' '.' E (1)
  //    |  'let' 'var' '=' E 'in' E (2)
  //    |  'if' E 'then' E 'else' E (3)
  //    |  E E (4)
  //    |  E '.' 'num' (5)
  //    |  '(' E ',' E (',' E)* ')' (6)
  //    |  'true' (7)
  //    |  'false' (8)
  //    |  'num' (9)
  //    |  'var' (10)
  //    |  'add' E E (11)
  //    |  'sub' E E (12)
  //    |  'mul' E E (13)
  //    |  'div' E E (14)
  //    |  'eq' E E (15)
  //    |  'lt' E E (16)
  //
  // Equivalent BNF
  // S ::= E '#'
  // E ::= '\' 'var' '.' E (1)
  //    |  'let' 'var' '=' E 'in' E (2)
  //    |  'if' E 'then' E 'else' E (3)
  //    |  E E (4)
  //    |  E '.' 'num' (5)
  //    |  '(' E , E C ')' (6)
  //    |  'true' (7)
  //    |  'false' (8)
  //    |  'num' (9)
  //    |  'var' (10)
  //    |  'add' E E (11)
  //    |  'sub' E E (12)
  //    |  'mul' E E (13)
  //    |  'div' E E (14)
  //    |  'eq' E E (15)
  //    |  'lt' E E (16)
  // C ::= eps (17)
  //    |  ',' E C (18)
  //
  // Precedences and associativities: application is left-associative, selection
  // binds tighter than application.
  // S   ::= E '#'
  // E  ::= '\' 'var' '.' E (1)
  //     |  'let' 'var' '=' E 'in' E (2)
  //     |  'if' E 'then' E 'else' E (3)
  //     |  E1
  // E1 ::= E1 E2 (4)
  //     |  E2
  //     |  'add' E2 E2 (11)
  //     |  'sub' E2 E2 (12)
  //     |  'mul' E2 E2 (13)
  //     |  'div' E2 E2 (14)
  //     |  'eq' E2 E2 (15)
  //     |  'lt' E2 E2 (16)
  // E2 ::= E2 '.' 'num' (5)
  //     |  '(' E ',' E C ')' (6)
  //     |  'true' (7)
  //     |  'false' (8)
  //     |  'num' (9)
  //     |  'var' (10)
  //     |  '(' E ')'
  // C  ::= eps (17)
  //     |  ',' E C (18)
  //
  // Eliminate left recursion in E1 and E2
  // S   ::= E '#'
  // E   ::= '\' 'var' '.' E (1)
  //      |  'let' 'var' '=' E 'in' E (2)
  //      |  'if' E 'then' E 'else' E (3)
  //      |  E1
  // E1  ::= E2 E1a
  //      |  'add' E2 E2 (11) E1a
  //      |  'sub' E2 E2 (12) E1a
  //      |  'mul' E2 E2 (13) E1a
  //      |  'div' E2 E2 (14) E1a
  //      |  'eq' E2 E2 (15) E1a
  //      |  'lt' E2 E2 (16) E1a
  // E1a ::= E2 (4) E1a
  //      |  eps
  // E2  ::= '(' E ',' E C ')' (6) E2a
  //      |  'true' (7) E2a
  //      |  'false' (8) E2a
  //      |  'num' (9) E2a
  //      |  'var' (10) E2a
  //      |  '(' E ')' E2a
  // E2a ::= '.' 'num' (5) E2a
  //      |  eps
  // C   ::= eps (17)
  //      |  ',' E C (18)
  //
  // Perform left-factoring to remove common prefixes in rhs of E2
  //                                        nullable    Dir
  // S   ::= E '#'
  // E   ::=                                [-]
  //      |  '\' 'var' '.' E (1)                        {'\'}
  //      |  'let' 'var' '=' E 'in' E (2)               {'let'}
  //      |  'if' E 'then' E 'else' E (3)               {'if'}
  //      |  E1                                         Fst(E1) = {'(','true','false','num','var','add','sub','mul','div,'eq','lt'}
  // E1  ::=                                [-]
  //         E2 E1a                                     Fst(E2) = {'(','true','false','num','var'}
  //      |  'add' E2 E2 (11) E1a                       {'add'}
  //      |  'sub' E2 E2 (12) E1a                       {'sub'}
  //      |  'mul' E2 E2 (13) E1a                       {'mul'}
  //      |  'div' E2 E2 (14) E1a                       {'div'}
  //      |  'eq' E2 E2 (15) E1a                        {'eq'}
  //      |  'lt' E2 E2 (16) E1a                        {'lt'}
  // E1a ::=                                [x]
  //         E2 (4) E1a                                 Fst(E2) = {'(','true','false','num','var'}
  //      |  eps                                        Flw(E1a) = {'#','in','then','else',')',','}
  // E2  ::=                                [-]
  //      |  '(' E E2b                                  {'('}
  //      |  'true' (7) E2a                             {'true'}
  //      |  'false' (8) E2a                            {'false'} 
  //      |  'num' (9) E2a                              {'num'}
  //      |  'var' (10) E2a                             {'var'}
  // E2a ::=                                [x]
  //         '.' 'num' (5) E2a                          {'.'}
  //      |  eps                                        Flw(E2a} = {'(','true','false','num','var','#','in','then','else',')',','}
  // E2b ::=                                [-]
  //         ',' E C ')' (6) E2a                        {','}
  //      |  ')' E2a                                    {')'}
  // C  ::=                                 [x]                                  
  //        eps (17)                                    Flw(C) = {')'}
  //     |  ',' E C (18)                                {','}
  //
  // Calculation of First and Follow sets
  // Fst(E) = {'\','let','if'} cup Fst(E1) = {'\','let','if','(','true','false','num','var','add','sub','mul','div','eq','lt'}
  // Fst(E1) = Fst(E2) cup {'add','sub','mul','div','eq','lt'} = {'(','true','false','num','var','add','sub','mul','div','eq','lt'}
  // Fst(E1a) = Fst(E2) cup Flw(E1a) = {'(','true','false','num','var','#'} 
  // Fst(E2) = {'(','true','false','num','var'}
  // Fst(E2a) = {'.'} cup Flw(E2a)
  // Fst(E2b) = Fst(E) cup {')'} = {'\','let','if','(','true','false','num','var','add','sub','mul','div','eq','lt',')'}
  // Fst(C) = Flw(C) cup {','} = {')',','}
  // Flw(E) = {'#','in','then','else',')',','}
  // Flw(E1) = Flw(E) = {'#',in','then','else',')',','}
  // Flw(E1a) = Flw(E1a) cup Flw(E1) = {'#','in','then','else',')',','}
  // Flw(E2) = Fst(E1a) cup Flw(E1a) = {'(','true','false','num','var','#','in','then','else',')',','}
  // Flw(E2a) = Flw(E2a) cup Flw(E2b) cup Flw(E2) = Flw(E2b) cup Flw(E2) = Flw(E2b) cup {'(','true','false','num','var','#'} = {'(','true','false','num','var','#','in','then','else',')',','}
  // Flw(E2b) = Flw(E2) = {'(','true','false','num','var','#'}
  // Flw(C) = Flw(C) cup {')'} = {')'}

  type P[A] = Parser[Token, A]

  def parse(inp: List[Token]): Either[String, Expr] =
    run(inp, parseS) match {
      case Fail(ParseErrorMessage(msg)) => Left(msg)
      case Okay(e) => Right(e)
      case _ => throw new RuntimeException("compiler bug")
    }

  // The parsing functions, derived from the LL(1) grammar

  private def parseS: P[Expr] = parseE ~< skip(Eof)

  private def parseE: P[Expr] = (
    shift(Lambda) ~ shift(_.isVar, "variable") ~< skip(Dot) ~ parseE ~* makeAbs
    |^ shift(Let) ~ shift(_.isVar, "variable") ~< skip(Eq) ~ parseE ~< skip(In) ~ parseE ~* makeLetIn
    |^ shift(If) ~ parseE ~< skip(Then) ~ parseE ~< skip(Else) ~ parseE ~* makeCond
    |^ parseE1
    |^ fail("expected `\\', `let', `if', primitive, number, `true', `false', variable, or `('"))

  private def parseE1: P[Expr] = (
    shift(AddPrim) ~ parseE2 ~ parseE2 ~* makeAddPrim
    |^ shift(SubPrim) ~ parseE2 ~ parseE2 ~* makeSubPrim
    |^ shift(MulPrim) ~ parseE2 ~ parseE2 ~* makeMulPrim
    |^ shift(DivPrim) ~ parseE2 ~ parseE2 ~* makeDivPrim
    |^ shift(EqPrim) ~ parseE2 ~ parseE2 ~* makeEqPrim
    |^ shift(LtPrim) ~ parseE2 ~ parseE2 ~* makeLtPrim
    |^ parseE2 ~& parseE1a
    |^ fail("expected primitive, number, `true', `false', variable, or `('"))

  private def parseE1a(e: Expr): P[Expr] = (
    parseE2 ~* makeApp(e) ~& parseE1a
    |^ followE1a(e)
    |^ fail("expected <EOF>, number, `true', `false', variable, `in', `then', `else', `,' `(', or `)'"))

  private def parseE2: P[Expr] = (
    shift(Open) ~> parseE ~& parseE2b
    |^ shift(True) ~* makeTrue ~& parseE2a
    |^ shift(False) ~* makeFalse ~& parseE2a
    |^ shift(_.isNum, "number") ~* makeNum ~& parseE2a
    |^ shift(_.isVar, "variable") ~* makeVar ~& parseE2a
    |^ fail("expected `(', `true', `false', number, or `variable'"))

  private def parseE2a(e: Expr): P[Expr] = (
    skip(Dot) ~> shift(_.isNum, "number") ~* makeSelect(e) ~& parseE2a
    |^ followE2a(e)
    |^ fail("expected `.', `(', `true', `false', number, variable, `in', `then', `else', `,', `)', or <EOF>"))

  private def parseE2b(e: Expr): P[Expr] = (
    skip(Comma) ~> parseE ~& parseC ~< skip(Close) ~* makeTuple(e) ~& parseE2a
    |^ skip(Close) ~> parseE2a(e)
    |^ fail("expected `)' or `,'"))

  private def parseC(e: Expr): P[List[Expr]] = (
    followC(e :: Nil)
    |^ skip(Comma) ~> parseE ~& parseC ~* makeList(e)
    |^ fail("expected `)' or `,'"))

  // The follow assertions for epsilon productions

  private def followE1a(e: Expr): P[Expr] =
    peek(tok => tok == Eof || tok == In || tok == Then ||
      tok == Else || tok == Close || tok == Comma,
      "`in', `then', `else', `,', `)', or <EOF>") ~> eps(e)

  private def followE2a(e: Expr): P[Expr] =
    peek(tok => tok == Open || tok == True || tok == False ||
      tok.isNum || tok.isVar || tok == Eof || tok == In || tok == Then ||
      tok == Else || tok == Close || tok == Comma,
      "`(', true, false, number, variable, `in', `then', `else', `,', `)', or <EOF>") ~> eps(e)

  private def followC(es: List[Expr]): P[List[Expr]] =
    peek(_ == Close, "`)'") ~> eps(es)

  // Semantic actions

  private def makeAbs(x: ((Token, Token), Expr)): Expr =
    x match {
      case ((lambdaTok, Var(name)), e) => AS.Abs(name, e, Range(lambdaTok.pos, e.src.to))
    }

  private def makeLetIn(x: (((Token, Token), Expr), Expr)): Expr =
    x match {
      case (((letTok, Var(name)), e1), e2) => AS.LetIn(name, e1, e2, Range(letTok.pos, e2.src.to))
    }

  private def makeCond(x: (((Token, Expr), Expr), Expr)): Expr =
    x match {
      case (((ifTok, e1), e2), e3) => AS.Cond(e1, e2, e3, Range(ifTok.pos, e3.src.to))
    }

  private def makePrim(c: (Expr, Expr, Range) => Expr, x: ((Token, Expr), Expr)): Expr =
    x match {
      case ((tok, e1), e2) => c(e1, e2, Range(tok.pos, e2.src.to))
    }

  private def makeAddPrim(x: ((Token, Expr), Expr)): Expr = makePrim(AS.AddPrim, x)
  private def makeSubPrim(x: ((Token, Expr), Expr)): Expr = makePrim(AS.SubPrim, x)
  private def makeMulPrim(x: ((Token, Expr), Expr)): Expr = makePrim(AS.MulPrim, x)
  private def makeDivPrim(x: ((Token, Expr), Expr)): Expr = makePrim(AS.DivPrim, x)
  private def makeEqPrim(x: ((Token, Expr), Expr)): Expr = makePrim(AS.EqPrim, x)
  private def makeLtPrim(x: ((Token, Expr), Expr)): Expr = makePrim(AS.LtPrim, x)

  private def makeApp(e1: Expr): Expr => Expr =
    e2 => AS.App(e1, e2, Range(e1.src.from, e2.src.to))

  private def makeTrue(tok: Token): Expr =
    AS.Bool(true, Range(tok.pos, tok.pos))

  private def makeFalse(tok: Token): Expr =
    AS.Bool(false, Range(tok.pos, tok.pos))

  private def makeNum(tok: Token): Expr =
    tok match {
      case Num(value) => AS.Num(value, Range(tok.pos, tok.pos))
    }

  private def makeVar(tok: Token): Expr =
    tok match {
      case Var(name) => AS.Var(name, Range(tok.pos, tok.pos))
    }

  private def makeSelect(e: Expr): Token => Expr =
    { case tok @ Num(value) => AS.Select(e, value, Range(e.src.from, tok.pos)) }

  private def makeTuple(e: Expr): List[Expr] => Expr =
    es => AS.Tuple(e :: es, Range(es.head.src.from, es.last.src.to))

  private def makeList(e: Expr): List[Expr] => List[Expr] =
    es => e :: es

  // Parser primitives adapted to the LambdaCalculus implementation

  private def fail[A](msg: String): P[A] =
    ParserCombinators.peek() ~&
      (tok => ParserCombinators.fail(ParseErrorMessage(msg + " at " + tok.pos)))

  private def shift(tok: Token): P[Token] =
    ParserCombinators.shift(
      _ == tok,
      tok1 => ParseErrorMessage("expected `" + tok + "' at " + tok1.pos))

  private def shift(p: Token => Boolean, str: String): P[Token] =
    ParserCombinators.shift(
      p,
      tok1 => ParseErrorMessage("expected `" + str + "' at " + tok1.pos))

  private def skip(tok: Token): P[Unit] =
    ParserCombinators.skip(
      _ == tok,
      (tok1 => ParseErrorMessage("expected `" + tok + "' at " + tok1.pos)))

  private def peek(p: Token => Boolean, str: String): P[Token] =
    ParserCombinators.peek(
      p,
      tok1 => ParseErrorMessage("expected `" + str + "' at " + tok1.pos))

  private case class ParseErrorMessage(msg: String) extends Message
}
