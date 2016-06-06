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

package de.tuberlin.uebb.comp1.moc

/** Abstract syntax of Î¼-Opal */

  /**
   * ### Initial EBNF Grammar ###
   * 
   * Prog	::=	Def+ #
   * Def	::=	DEF Lhs == Expr
   * Lhs	::=	MAIN : Type
   * 		|	id ( [id : Type (,id : Type)*]) : Type
   * Type	::= nat | bool
   * Expr	::= number | true | false
   * 		|	id [([Expr (,Expr)*])]
   *   		IF Expr THEN Expr [ELSE Expr] FI
   *     
   * ### BNF Grammar ###
   * 
   *     Prog -> Def A EOF
   *     A -> Def A 
   *     	| ε
   *     Def -> DEF Lhs == Expr
   *     Lhs -> MAIN : Type 
   *     	| id ( B ) : Type
   *     B -> id : Type C 
   *     	| ε
   *     C -> , id : Type C 
   *     	| ε
   *     Type -> nat | bool
   *     Expr -> number 
   *     	| true 
   *      	| false 
   *       	| id D 
   *        | IF Expr THEN Expr E FI
   *     D -> ( G )
   *     	| ε
   *     G -> Expr F
   *     	| ε
   *     E -> ELSE Expr 
   *     	| ε
   *     F -> , Expr F 
   *     	| ε
   *     
   * --------------------------    
   */
object AbstractSyntax {
  
  	case class Prog(d : List[Def])

  	case class Def(lhs : Lhs, expr : Expr, pos: Position)
	
	  abstract class Lhs()
  		case class MainDecl(t : Type, pos: Position) extends Lhs
  		case class Decl(id : String, args : List[Param], t : Type, pos: Position) extends Lhs
  		
  	case class Param(id : String, t : Type)
  	
  	abstract class Type(){
      def ~=~(t: Type) : Boolean = {
        (this, t) match {
          case (Unknown, _) => true
          case (_, Unknown) => true
          case (t1, t2) => if (t1 == t2) true 
            else 
              false
        }
      }
    }
  		case object Nat extends Type
  		case object Bool extends Type	
      case object Unknown extends Type
  		
  	abstract class Expr()
  		case class Number(v : Int) extends Expr
  		case object True extends Expr
  		case object False extends Expr
  		case class Variable(id : String, pos: Position) extends Expr
  		case class Function(id : String, args : List[Expr], pos: Position) extends Expr
  		case class IfThen(e1 : Expr, e2 : Expr, pos: Position) extends Expr
  		case class IfThenElse(e1 : Expr, e2: Expr, e3 : Expr, pos: Position) extends Expr
}
