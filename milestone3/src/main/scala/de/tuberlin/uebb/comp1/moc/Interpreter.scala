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

/** μ-Opal interpreter */

object Interpreter {
  import AbstractSyntax._

  case class Env(vars : Map[String, Value], funcs : Map[String, Def])
  
  /**
   * Interprets the given list of definitions [[Def]] starting with main function
   * @param prog the context checked program [[Prog]]
   * @param opts [[Options]] given as arguments to compiler
   * @return Either an error message [[Diag]] or a [[Value]]
   */
  def interpret(prog: Prog, opts: Options): Either[Diag, Value] = {
    
    // Evaluates the output of the program and returns either a Diag or the Value found
    val output = eval(prog)
    output match {
      case Undef(diag) => Left(diag)
      case _ => Right(output)
    }
  }
  
  def eval(prog: Prog): Value = {
    // Extracts the expression of the MAIN function for the evaluation
    // We can assume that there is a MAIN since it has been checked before
    val main_decl_expr = (prog.d.filter { d => d.lhs.isInstanceOf[MainDecl] }).head.expr
    
    // Extracts all the declarations to be added in the current environment for the evaluation
    val decls = prog.d.filter { d => d.lhs.isInstanceOf[Decl]}
    
    /* Retrieves all the id's of the declarations and 
     zips them with their definitions to be added in the environment for evaluation */
    val e = Env(Map(),Map() ++ (decls.map( d => d.lhs.asInstanceOf[Decl].id ) zip decls))
    
    // Starts the evaluation
    eval(main_decl_expr, e)
  }
  
  
  def eval(e: Expr, env: Env): Value = {
    e match {
      case Number(v) => NumV(v)
      case True => BoolV(true)
      case False => BoolV(false)
      case Variable(id, _) => env.vars(id)
      
      // If the conditional If-Then has the value false, it returns an error
      case IfThen(e1, e2, pos) => 
        eval(e1, env) match {
          case Undef(diag) => Undef(diag)
          case BoolV(true) => eval(e2, env)
          case BoolV(false) => Undef(Diag("Error: Conditional is false!", pos))
          case (_) => ???
        }
      
      // If the conditional If-Then-Else has the value true, evaluate then-expression
      // Otherwise, evaluate else-expression
      case IfThenElse(e1, e2, e3, pos) =>
         eval(e1, env) match {
          case Undef(diag) => Undef(diag)
          case BoolV(true) => eval(e2, env)
          case BoolV(false) => eval(e3, env)
          case (_) => ???
        }
        
      case Function(id, args, pos) =>
        {
          // Evaluates each argument provided in the invokation of the function
          val arg_values = args.map { a => eval(a, env) }
          
          // Checks the id of the function and acts accordingly
          id match {
            // Primitive functions
            case "add" =>
              (arg_values(0), arg_values(1)) match {
                case (NumV(a), NumV(b)) => 
                  // Checking for overflow 
                  // From: https://stackoverflow.com/questions/9874999/common-practice-how-to-deal-with-integer-overflows
                  val res = a.toLong + b.toLong
                  if(res > Int.MaxValue)
                    Undef(Diag("Error: Overflow detected!", pos))
                  else
                    NumV(a+b)
                case (Undef(diag), _) => Undef(diag)
                case (_, Undef(diag)) => Undef(diag)
                case (_, _) => ???
              }
            case "sub" =>
              (arg_values(0), arg_values(1)) match {
                case (NumV(a), NumV(b)) => 
                  // Checking for underflow 
                  val res = a.toLong - b.toLong
                  if(res < 0)
                    Undef(Diag("Error: Underflow detected!", pos))
                  else
                    NumV(a-b)
                case (Undef(diag), _) => Undef(diag)
                case (_, Undef(diag)) => Undef(diag)
                case (_, _) => ???
              }
            case "mul" =>
              (arg_values(0), arg_values(1)) match {
                case (NumV(a), NumV(b)) => 
                  // Checking for overflow 
                  // From: https://stackoverflow.com/questions/9874999/common-practice-how-to-deal-with-integer-overflows
                  val res = a.toLong * b.toLong
                  if(res > Int.MaxValue)
                    Undef(Diag("Error: Overflow detected!", pos))
                  else
                    NumV(a*b)
                case (Undef(diag), _) => Undef(diag)
                case (_, Undef(diag)) => Undef(diag)
                case (_, _) => ???
              }
            case "div" =>
              (arg_values(0), arg_values(1)) match {
                case (NumV(a), NumV(0)) => Undef(Diag("Error: Division by 0!", pos))
                case (NumV(a), NumV(b)) => NumV(a/b)
                case (Undef(diag), _) => Undef(diag)
                case (_, Undef(diag)) => Undef(diag)
                case (_, _) => ???
              }
            case "eq" => 
              (arg_values(0), arg_values(1)) match {
                case (NumV(a), NumV(b)) => BoolV(a == b)
                case (Undef(diag), _) => Undef(diag)
                case (_, Undef(diag)) => Undef(diag)
                case (_, _) => ???
              }
            case "lt" =>
              (arg_values(0), arg_values(1)) match {
                case (NumV(a), NumV(b)) => BoolV(a < b)
                case (Undef(diag), _) => Undef(diag)
                case (_, Undef(diag)) => Undef(diag)
                case (_, _) => ???
              }
            case "and" => 
              (arg_values(0), arg_values(1)) match {
                case (BoolV(true), BoolV(true)) => BoolV(true)
                case (BoolV(_), BoolV(_)) => BoolV(false)
                case (Undef(diag), _) => Undef(diag)
                case (_, Undef(diag)) => Undef(diag)
                case (_, _) => ???
              }
            case "or" => 
              (arg_values(0), arg_values(1)) match {
                case (BoolV(true), BoolV(_)) => BoolV(true)
                case (BoolV(_), BoolV(true)) => BoolV(true)
                case (BoolV(_), BoolV(_)) => BoolV(false)
                case (Undef(diag), _) => Undef(diag)
                case (_, Undef(diag)) => Undef(diag)
                case (_, _) => ???
              }
            case "not" => 
              (arg_values(0)) match {
                case (BoolV(true)) => BoolV(false)
                case (BoolV(false)) => BoolV(true)
                case (Undef(diag)) => Undef(diag)
                case (_) => ???
              }
            // User-defined functions
            case _ =>
              // Get current function
              val func = env.funcs(id)
            
              // Maps the id of the arguments with their values (evaluated at the beginning of this method)
              val args_map = func.lhs.asInstanceOf[Decl].args.map { a => a.id } zip arg_values
              
              // Evaluates the expression in the function using the old environment jointed with the arguments
              eval(func.expr, Env(env.vars ++ args_map, env.funcs))
            
          }
        }
    }
  }
}
