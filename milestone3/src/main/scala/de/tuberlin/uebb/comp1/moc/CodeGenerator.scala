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

import de.tuberlin.uebb.comp1.covm.instructions._

/** μ-Opal code generator */

object CodeGenerator {
  import AbstractSyntax._

  private type Env = Map[String, Int]
  type Code = List[Instruction]
  
  private var lbl_counter = -1
  private val emptyEnv = Map[String, Int]()
  
  
  /**
   * Generates [[Code]] for the given list of definitions
   * @param prog the context checked program [[Prog]]
   * @param opts [[Options]] given as arguments to compiler
   * @return Either an error message [[Diag]] or the code for the stack machine
   */
  def compile(prog: Prog, opts: Options): Either[Diag, Code] = {
    // Similar to the Interpreter
    val main_decl_expr = (prog.d.filter { d => d.lhs.isInstanceOf[MainDecl] }).head.expr
    
    val code = labelledCodeGen(main_decl_expr, emptyEnv)
    
    if(opts.debug)
      prettyPrinter(code, 1)
    
    Right(patchLabels(code ++ List(Stop)))
  }
  
  // Inspired by 'LambaCalculusStage09' example
  private def labelledCodeGen(e: Expr, env: Env): Code = {
    e match {
      case Number(v) => List(PushInt(v))
      case True => List(PushInt(1))
      case False => List(PushInt(0))
      case Variable(id, _) => List(Push(env(id)))
      case IfThen(e1, e2, _) => {
        val code_e1 = labelledCodeGen(e1, env)
        val code_e2 = labelledCodeGen(e2, env)
        val trueLabel = freshLabel
        val falseLabel = freshLabel
        code_e1 ++ List(Jz(LabelledAddress(falseLabel))) ++
        code_e2 ++ List(Jmp(LabelledAddress(trueLabel))) ++
        List(Label(falseLabel)) ++ List(Abort("Assertion is False")) ++ List(Label(trueLabel))
      }
      case IfThenElse(e1, e2, e3, _) => {
        val code_e1 = labelledCodeGen(e1, env)
        val code_e2 = labelledCodeGen(e2, env)
        val code_e3 = labelledCodeGen(e3, env)
        val trueLabel = freshLabel
        val falseLabel = freshLabel
        code_e1 ++ List(Jz(LabelledAddress(falseLabel))) ++
        code_e2 ++ List(Jmp(LabelledAddress(trueLabel))) ++
        List(Label(falseLabel)) ++ code_e3 ++ List(Label(trueLabel))
      }
      case Function(id, args, pos) =>
        id match {
            // Primitive functions
            case "add" => {
              val code = labelledCodeGen(List(args(0), args(1)), env)
              code ++ List(Add)
            }
            case "sub" => {
              val code = labelledCodeGen(List(args(0), args(1)), env)
              val okLabel = freshLabel
              code ++ List(Sub,Jgt(LabelledAddress(okLabel)), Abort("Underflow Detected"),
              Label(okLabel))
                
            }
            case "mul" => {
              val code = labelledCodeGen(List(args(0), args(1)), env)
              code ++ List(Mul)
            }
            case "div" => {
              val code = labelledCodeGen(List(args(0), args(1)), env)
              code ++ List(Div)
            }
            case "eq" => {
              val code = labelledCodeGen(List(args(0), args(1)), env)
              val falseLabel = freshLabel
              val trueLabel = freshLabel
              code ++ List(Sub, Jz(LabelledAddress(trueLabel)), PushInt(0),
              Jmp(LabelledAddress(falseLabel)),
              Label(trueLabel), PushInt(1), Label(falseLabel))
            }
            case "lt" => {
              val code = labelledCodeGen(List(args(0), args(1)), env)
              val falseLabel = freshLabel
              val trueLabel = freshLabel
              code ++ List(Sub, Jlt(LabelledAddress(trueLabel)), PushInt(0),
              Jmp(LabelledAddress(falseLabel)),
              Label(trueLabel), PushInt(1), Label(falseLabel))
            }
            case "and" => {
              val code = labelledCodeGen(List(args(0), args(1)), env)
              // AND operator can be expressed as (a * b)
              code ++ List(Mul)
              
            }
            case "or" => {
              val code = labelledCodeGen(List(args(0), args(1)), env)
              // OR operator can be expressed as (a + b -(a * b))
              code ++ List(Add) ++ code ++ List(Mul) ++ List(Sub)
            }
            case "not" => {
              val code = labelledCodeGen(List(args(0)), env)
               // NOT operator can be expressed as (1 - a)
              List(PushInt(1)) ++ code ++ List(Sub) 
            }
            
            // User-defined functions
            case _ => {
              val fun_call_code = List(PushAddr(LabelledAddress(id))) ++ List(Call)
              ???
            }
            
        }
    }
    
    
  }
  
    /**
   * Generate code for a sequence of expressions.
   *  The environment is shifted as necessary.
   */
  // Inspired by 'LambaCalculusStage09' example
  private def labelledCodeGen(es: List[Expr], env: Env): Code =
    es match {
      case Nil => Nil
      case e :: es => {
        val code1 = labelledCodeGen(e, env)
        val code2 = labelledCodeGen(es, shift(env))
        code1 ++ code2
      }
    }
      
  // Inspired by 'LambaCalculusStage09' example
  private def patchLabels(instrs: Code): Code = {
    val (instrs1, env1) = removeLabels(instrs, 0, emptyEnv)
    patchAddrs(instrs1, env1)
  }
    
  // Inspired by 'LambaCalculusStage09' example
  private def patchAddrs(instrs: Code, env: Env): Code =
    instrs.map(patchAddr(env))
    
  // Inspired by 'LambaCalculusStage09' example
  private def patchAddr(env: Env)(instr: Instruction): Instruction =
    instr match {
        case Jz(LabelledAddress(lbl)) => Jz(Pointer(env(lbl)))
        case Jlt(LabelledAddress(lbl)) => Jlt(Pointer(env(lbl)))
        case Jgt(LabelledAddress(lbl)) => Jgt(Pointer(env(lbl)))
        case Jmp(LabelledAddress(lbl)) => Jmp(Pointer(env(lbl)))
        case PushAddr(LabelledAddress(lbl)) => PushAddr(Pointer(env(lbl)))
        case instr => instr
    }
  
  // Inspired by 'LambaCalculusStage09' example
  private def removeLabels(instrs: Code, count: Int, env: Env): (Code, Env) =
    instrs match {
      case Nil => (Nil, env)
      case Label(name) :: instrs1 =>
        removeLabels(instrs1, count, env + (name -> count))
      case instr1 :: instrs1 =>
        val (instrs2, env2) = removeLabels(instrs1, count + 1, env)
        (instr1 :: instrs2, env2)
    }
  
  private def prettyPrinter(code: Code, pos: Int): Unit = {
      if(!code.isEmpty)
      {
        code.head match {
          case Label(name) => println(pos + "\t" + name + ":")
          case instr => println(pos + "\t" + instr)
        }
        prettyPrinter(code.tail, pos+1)
      }
      else
        println(pos + "\tStop")
  }
    /** Shift all slots in the environment by one */
  private def shift(env: Env): Env =
    env.mapValues(_ + 1)
    
  private def freshLabel(): String = {
    lbl_counter += 1
    "Label_" + lbl_counter
  }
}
