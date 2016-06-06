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

/** Parser for μ-Opal-Compiler*/

object Parser {

  import AbstractSyntax._
  import de.tuberlin.uebb.comp1.moc.Token
  
  private type Toks = List[Token]
  
  def parse(s: Toks, opts: Options): Either[Diag, Prog] = 
	parseProg(s)._1
	
  def parseProg(s: Toks): (Either[Diag, Prog], Toks)  = {
    
	val ft = s.head	// lookahead
	  
	if(ft == DefT()){
		val (d1, s1) = parseDef(s)
		
		val (ds2, s2) = parseA(s1)
		
		val (t3, s3) = parseEof(s2)
    
		// Checking results and creating Prog
		(d1, ds2) match {
	      case (Right(d), Right(ds)) => (Right(Prog(d::ds)), s3)
	      case (Left(msg), _) => (Left(msg), s1)
	      case (_, Left(msg)) => (Left(msg), s2)
	    }
	}
	// Specific comment for empty file
	else if(ft == EofT())
	   (Left(Diag("The program doesn't contain any definition.", Global)), s)
	else
	  (Left(Diag("No Definition found. Found '" + ft + "' instead. ", ft.getPosition)), s)
  }
  
  private def parseDef(s: Toks): (Either[Diag, Def], Toks) = {
    
    val (t1, s1) = parseDefToken(s)
    val (d2, s2) = parseLhs(s1)
    
    val (t3, s3) = parseEqualToken(s2)
    val (d4, s4) = parseExpr(s3)
   
    // Checking results and creating Def
    
    t3 match {
      case Right(_) => {
        (d2, d4) match {
          case (Right(lhs), Right(expr)) => (Right(Def(lhs, expr, s.head.getPosition)), s4)
          case (Left(msg), _) => (Left(msg), s2)
          case (_, Left(msg)) => (Left(msg), s4)
        }
      }
      case Left(msg) => (Left(msg), s3)
    }
    
    
  }
  
  private def parseA(s: Toks): (Either[Diag, List[Def]], Toks) = {
    
    val ft = s.head	// lookahead
    
    if (ft == DefT()) {
      val (d1, s1) = parseDef(s)      
      val (ds2, s2) = parseA(s1)
      
      // Checking results
      (d1, ds2) match {
				case (Right(d), Right(ds)) => (Right(d::ds), s2)
				case (Left(msg), _) => (Left(msg), s1)
				case (_, Left(msg)) => (Left(msg), s2)
      }
    } else if (ft == EofT()) {
      (Right(List()), s)	// epsilon production
    } else
      (Left(Diag("Expecting a definition. Found '" + ft + "' instead.", ft.getPosition)), s)
  }
  
  private def parseLhs(s: Toks): (Either[Diag, Lhs], Toks) = {
    
    val ft = s.head	// lookahead
    
    
    ft match {
      		case MainT() => {
      		  val (t1, s1) = parseMainToken(s)
      		  val (t2, s2) = parseColonToken(s1)
      		  
      		  val (d3, s3) = parseType(s2)
      		  
      		  // Checking results and creating Lhs
      		  (t2, d3) match {
      		    case (Right(t), Right(d)) => (Right(MainDecl(d, ft.getPosition)), s3)
				case (Left(msg), _) => (Left(msg), s2)
				case (_, Left(msg)) => (Left(msg), s3)
      		  }

      		}
      		case VarT(id) => {
      		  
      		  val (t1, s1) = parseVarToken(s)
      		  val (t2, s2) = parseOpenToken(s1)
      		  
      		  val (d3, s3) = parseB(s2)
      		  
      		  val (t4, s4) = parseCloseToken(s3)
      		  val (t5, s5) = parseColonToken(s4)
      		  
      		  val (d6, s6) = parseType(s5)
      		  
      		  // Checking results and creating Lhs
      		 t2 match {
              case Right(_) => {
                (d3, d6) match {
                case (Right(par), Right(tp)) => (Right(Decl(id, par, tp, ft.getPosition)), s6)
  				      case (Left(msg), _) => (Left(msg), s3)
  				      case (_, Left(msg)) => (Left(msg), s6)
                }
              }
              case Left(msg) => (Left(msg),s2)
			  }
      		  
      		} 
      		case _ => (Left(Diag("Expecting function name. Found '" + ft + "' instead.", ft.getPosition)), s)
  }
    	
  }
   
  private def parseB(s: Toks): (Either[Diag, List[Param]], Toks) = {
    
    val ft = s.head	// lookahead
    
    ft match {
      case VarT(id) => {
        
        val (t1, s1) = parseVarToken(s)
        val (t2, s2) = parseColonToken(s1)
        
        val (d3, s3) = parseType(s2)
        
        val (ps4, s4) = parseC(s3)
        
        // Checking results and creating List[Param]
        t2 match {
          case Right(_) => {
            (d3, ps4) match {
            case (Right(p), Right(ps)) => (Right(Param(id, p)::ps), s4)
				    case (Left(msg), _) => (Left(msg), s3)
				    case (_, Left(msg)) => (Left(msg), s4)
            }
          }
          case Left(msg) => (Left(msg),s2)
        }
      }
      case CloseT() => (Right(List()), s)
      case _ => (Left(Diag("Expecting a parameter. Found '" + ft + "' instead.", ft.getPosition)), s)
        
    }
  }
  
  private def parseC(s: Toks): (Either[Diag, List[Param]], Toks) = {
    
    val ft = s.head	// lookahead
    
    ft match {
      case CommaT() => {
        
        val (t1, s1) = parseCommaToken(s)
        
        val (t2, s2) = parseVarToken(s1)
        
        val (t3, s3) = parseColonToken(s2)
        
        val (p4, s4) = parseType(s3)

        val (ps5, s5) = parseC(s4)
        
        // Checking results and creating List[Param]
       
        (t3,t2) match {
          case (Right(_), Right(t)) => {
              (p4, ps5) match {
    				case (Right(p), Right(ps)) => (Right(Param(t.toString, p)::ps), s5)
              
    				case (Left(msg), _) => (Left(msg), s4)
    				case (_, Left(msg)) => (Left(msg), s5)
            }
          }
         case (Left(msg),_) => (Left(msg),s3)
         case (_,Left(msg)) => (Left(msg),s2)
        }          
      }
      case CloseT() => (Right(List()), s)
      case _ => (Left(Diag("Expecting a parameter. Found '" + ft + "' instead.", ft.getPosition)), s)
    }
  }
    
  private def parseType(s: Toks): (Either[Diag, Type], Toks) = {
    
    val ft = s.head	// lookahead
    
    ft match {
		case NatT() => (Right(Nat), s.tail)
		case BoolT() => (Right(Bool), s.tail)
		case _ => (Left(Diag("Unknown type found: '" + ft + "'.", ft.getPosition)), s)
	}
  }
  
  private def parseExpr(s: Toks): (Either[Diag, Expr], Toks) = {
    
    val ft = s.head	// lookahead
    
    ft match {
      case NumT(v) => (Right(Number(v)), s.tail)
      case TrueT() => (Right(True), s.tail)
      case FalseT() => (Right(False), s.tail)
      case VarT(id) => parseD(s.tail, id)
      case IfT() => {
        
		val (t1,s1) = parseIfToken(s)
		
		val (e2,s2) = parseExpr(s1)
		
		val (t3,s3) = parseThenToken(s2)
		
		val (e4,s4) = parseExpr(s3)
		
		// Checking results and possible ELSE statement
  		t3 match {
      
        case (Right(t3)) => {
            (e2,e4) match {
        			case (Right(e1),Right(e2)) => {
        			  
        				val (e5,s5) = parseE(s4, e1, e2)
        				
        				e5 match {
        					case Right(e) => (Right(e), s5)
        					case Left(msg) => (Left(msg), s5)
        				}						
        			}
        			case (Left(msg), _) => (Left(msg), s2)
        			case (_, Left(msg)) => (Left(msg), s4)				
              }
              
        }
        case (Left(msg)) => (Left(msg), s4)
        
      }
    }
   case _ => (Left(Diag("Unexpected expression found: " + ft + "'.", ft.getPosition)), s)
  }
  }
  
  private def parseD(s: Toks, id : String): (Either[Diag, Expr], Toks) = {
    
    val ft = s.head	// lookahead
    
    if (ft == OpenT())
    {
        val (t1, s1) = parseOpenToken(s)
        
        val (e2, s2) = parseG(s1, id)
        val (t3, s3) = parseCloseToken(s2)
          
        // Checking result and creating Function
        (t3,e2) match {
				case (Right(_),Right(args)) => (Right(Function(id, args, ft.getPosition)), s3)
				case (_,Left(msg)) => (Left(msg), s2)
				case (Left(msg),_) => (Left(Diag("No ')' found. Found: '" + ft + "' instead.", ft.getPosition)), s3)
        }
        
        // Creating Variable
    } else if (ft == EofT() || ft == ElseT() || ft == CommaT() || ft == CloseT() || ft == FiT() || ft == ThenT() || ft == DefT()) 
      (Right(Variable(id, ft.getPosition)), s)
      
      else
      (Left(Diag("Unexpected token found: '" + ft + "'.", ft.getPosition)), s)
  }
  
  private def parseG(s: Toks, id : String): (Either[Diag, List[Expr]], Toks) = {
    
    val ft = s.head	// lookahead
    
    ft match {
      case NumT(_) | TrueT() | FalseT() | VarT(_) | IfT() => {

        val (e1, s1) = parseExpr(s)
        
        val (es2, s2) = parseF(s1)
        
        // Checking result and creating Function
		(e1, es2) match {
			case (Right(e), Right(es)) => (Right(e::es), s2)
			case (Left(msg), _) => (Left(msg), s1)
			case (_, Left(msg)) => (Left(msg), s2)
		}
      
      } 
      case CloseT() => (Right(List()), s)
      
      case _ =>  (Left(Diag("Unexpected token found: '" + ft + "'.", ft.getPosition)), s)
    }
  }

  private def parseE(s: Toks, e1 : Expr, e2: Expr): (Either[Diag, Expr], Toks) = {
    
    val ft = s.head	// lookahead
    
    if (ft == ElseT())
    {
        val (t1, s1) = parseElseToken(s)
        
        val (el2, s2) = parseExpr(s1)
        val (t3, s3) = parseFiToken(s2)
        
        // Checking result and creating IfThenElse Expr
        (t3,el2) match {
          case (Right(_),Right(e3)) => (Right(IfThenElse(e1, e2, e3, ft.getPosition)), s3)
          case (_, Left(msg)) => (Left(msg), s2)
          case (Left(msg), _) => (Left(Diag("No FI found. Found: '" + ft + "' instead.", ft.getPosition)), s3)
        }
        // Otherwise creating IfThen Expr
    } else if (ft == FiT()) (Right(IfThen(e1, e2, ft.getPosition)), s.tail)
      
      else
      (Left(Diag("Expecting 'ELSE' or 'FI'. Found: '" + ft + "' instead.", ft.getPosition)), s)
    
  }  

  private def parseF(s: Toks): (Either[Diag, List[Expr]], Toks) = {
    
	val ft = s.head	// lookahead
    
    if (ft == CommaT())
    {
        val (t1, s1) = parseCommaToken(s)
        
        val (e2, s2) = parseExpr(s1)
        val (es3, s3) = parseF(s2)
      
        // Checking result and creating List[Expr]
        (e2, es3) match {
				case (Right(e), Right(es)) => (Right(e::es), s3)
				case (Left(msg), _) => (Left(msg), s2)
				case (_, Left(msg)) => (Left(msg), s3)
			}	
    } else if (ft == CloseT()) 
      (Right(List()), s)
      
      else
      (Left(Diag("Unexpected argument found: '" + ft + "'.", ft.getPosition)), s)
    
  }
  
  private def parseEof(s: Toks): (Either[Diag, Token], Toks) = {
    
    val ft = s.head
    
    if (ft == EofT()) 
    	(Right(ft), s.tail)
    else 
    	(Left(Diag("Expcting 'EOF'. Found '" + ft + "' instead.", ft.getPosition)), s)
  }
  
  
  // Token Parsers
  private def parseMainToken(s: Toks): (Either[Diag, Token], Toks) = shift(_ == MainT(), s)
  private def parseDefToken(s: Toks): (Either[Diag, Token], Toks) = shift(_ == DefT(), s)
  private def parseVarToken(s: Toks): (Either[Diag, Token], Toks) = shift(_.isVar, s)
  private def parseEqualToken(s: Toks): (Either[Diag, Token], Toks) = shift(_ == DefAsT(), s)
  private def parseColonToken(s: Toks): (Either[Diag, Token], Toks) = shift(_ == ColonT(), s)
  private def parseOpenToken(s: Toks): (Either[Diag, Token], Toks) = shift(_ == OpenT(), s)
  private def parseCloseToken(s: Toks): (Either[Diag, Token], Toks) = shift(_ == CloseT(), s)
  private def parseNumToken(s: Toks): (Either[Diag, Token], Toks) = shift(_.isNum, s)
  private def parseCommaToken(s: Toks): (Either[Diag, Token], Toks) = shift(_ == CommaT(), s)
  private def parseIfToken(s: Toks): (Either[Diag, Token], Toks) = shift(_ == IfT(), s)
  private def parseThenToken(s: Toks): (Either[Diag, Token], Toks) = shift(_ == ThenT(), s)
  private def parseElseToken(s: Toks): (Either[Diag, Token], Toks) = shift(_ == ElseT(), s)
  private def parseFiToken(s: Toks): (Either[Diag, Token], Toks) = shift(_ == FiT(), s)
  
  private def shift(pred: Token => Boolean, s: Toks): (Either[Diag, Token], Toks) = {
    val ft = s.head
    if (pred(ft)) {
      (Right(ft), s.tail)
    } else 
    	(Left(Diag("Unexpected token found: '" + ft + "'.", ft.getPosition)), s)
  }
  
}
