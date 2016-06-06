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

import de.tuberlin.uebb.comp1.covm.instructions._

/** Code generator for the Simply Typed Lambda Calculus */
object CodeGenerator {
  import AbstractSyntax._

  private var nextLabelNum = 0

  private type Env = Map[String, Int]
  private val emptyEnv = Map[String, Int]()
  private type Code = (List[Instruction], List[Instruction])

  def codeGen(e: Expr): List[Instruction] = {
    val (codeL, codeR) = labelledCodeGen(emptyEnv, e)
    patchLabels(codeR ++ List(Stop) ++ codeL)
  }

  private def patchLabels(instrs: List[Instruction]): List[Instruction] = {
    val (instrs1, env1) = removeLabels(instrs, 0, emptyEnv)
    patchAddrs(instrs1, env1)
  }

  private def removeLabels(instrs: List[Instruction], count: Int, env: Env): (List[Instruction], Env) =
    instrs match {
      case Nil => (Nil, env)
      case Label(name) :: instrs1 =>
        removeLabels(instrs1, count, env + (name -> count))
      case instr1 :: instrs1 =>
        val (instrs2, env2) = removeLabels(instrs1, count + 1, env)
        (instr1 :: instrs2, env2)
    }

  private def patchAddrs(instrs: List[Instruction], env: Env): List[Instruction] =
    instrs.map(patchAddr(env))

  private def patchAddr(env: Env)(instr: Instruction): Instruction =
    instr match {
      case Jz(LabelledAddress(name)) => Jz(Pointer(env(name)))
      case Jlt(LabelledAddress(name)) => Jlt(Pointer(env(name)))
      case Jgt(LabelledAddress(name)) => Jgt(Pointer(env(name)))
      case Jmp(LabelledAddress(name)) => Jmp(Pointer(env(name)))
      case PushAddr(LabelledAddress(name)) => PushAddr(Pointer(env(name)))
      case instr => instr
    }

  /** Generate code with labels */
  private def labelledCodeGen(env: Env, e: Expr): Code =
    e match {
      case e @ Abs(v, ty, body, _) => {
        val fvs = e.freeVars.toList
        val funLabel = freshLabel

        // Code that creates the closure
        val env1 = shift(env)
        val codeClosR =
          List(PushAddr(LabelledAddress(funLabel))) ++ // a : ws
            codeGenPushFvs(env1, fvs) ++ // fvn : ... : fv1 : a : ws
            List(Pack(fvs.size + 1)) // <a, fv1, ..., fvn> : ws

        // Code for the left part of the code segment (code to call)
        // Stack on entry to function as prepared at the call site:
        // aret : <a,fv1,...,fvn> : warg : ws

        // Code that extracts the free variables from the closure.
        // Stack after this code:
        // fvn : ... : fv1 : aret : <a,fv1,...,fvn> : warg : ws
        val fvsIdx = fvs.zip(1 to fvs.length) // (fv1,1) :: ... :: (fvn,n) :: Nil
        val codePushFVR =
          fvsIdx.flatMap({ case (fv, idx) => List(Push(idx), Unpack(idx)) })

        // Add free variable slots and the argument to the environment.
        val envBodyFvs = fvsIdx.map({ case (fv, idx) => (fv, fvs.length - idx) }).toMap
        val envBody = envBodyFvs + (v -> (fvs.length + 2))

        // Generate code for the body.
        val (codeBodyL, codeBodyR) = labelledCodeGen(envBody, body)

        (codeBodyL ++ // Other functions created during compilation of body
          List(Label(funLabel)) ++ // Label for this function
          codePushFVR ++ // Push free variables onto the stack
          codeBodyR ++ // Code of the body
          List(Slide(fvs.length), Ret), // Remove free variables from stack and return
          codeClosR) // Closure creation
      }
      case App(fun, arg, _) => {
        val code = labelledCodeGen(env, List(arg, fun)) // <a,fv1,...,fvn> : warg : ws
        addRight(code, List(Push(0), Unpack(0), Call, Slide(2)))
      }
      case e @ Fix(fun, _, _, arg, body, _) => {
        val fvs = e.freeVars.toList
        val funLabel = freshLabel

        // Code that creates the closure
        val env1 = shift(env)
        val codeClosR =
          List(PushAddr(LabelledAddress(funLabel))) ++ // a : ws
            codeGenPushFvs(env1, fvs) ++ // fvn : ... : fv1 : a : ws
            List(Pack(fvs.size + 1)) // <a, fv1, ..., fvn> : ws

        // Code for the left part of the code segment (code to call)
        // Stack on entry to function as prepared at the call site:
        // aret : <a,fv1,...,fvn> : warg : ws

        // Code that extracts the free variables from the closure.
        // Stack after this code:
        // fvn : ... : fv1 : aret : <a,fv1,...,fvn> : warg : ws
        val fvsIdx = fvs.zip(1 to fvs.length) // (fv1,1) :: ... :: (fvn,n) :: Nil
        val codePushFVR =
          fvsIdx.flatMap({ case (fv, idx) => List(Push(idx), Unpack(idx)) })

        // Add free variable slots and the argument to the environment.
        val envBodyFvs = fvsIdx.map({ case (fv, idx) => (fv, fvs.length - idx) }).toMap
        val envBody = envBodyFvs + (arg -> (fvs.length + 2)) + (fun -> (fvs.length + 1))

        // Generate code for the body.
        val (codeBodyL, codeBodyR) = labelledCodeGen(envBody, body)

        (codeBodyL ++ // Other functions created during compilation of body
          List(Label(funLabel)) ++ // Label for this function
          codePushFVR ++ // Push free variables onto the stack
          codeBodyR ++ // Code of the body
          List(Slide(fvs.length), Ret), // Remove free variables from stack and return
          codeClosR) // Closure creation
      }
      case Var(name, _) => {
        (Nil, List(Push(env(name))))
      }
      case Num(v, _) => (Nil, List(PushInt(v)))
      case Bool(v, _) =>
        if (v)
          (Nil, List(PushInt(1)))
        else
          (Nil, List(PushInt(0)))
      case Tuple(comps, _) => {
        val code = labelledCodeGen(env, comps)
        addRight(code, List(Pack(comps.length)))
      }
      case Select(tup, idx, _) => {
        val code = labelledCodeGen(env, tup)
        addRight(code, List(Unpack(idx - 1)))
      }
      case LetIn(v, rhs, body, _) => {
        val codeRhs = labelledCodeGen(env, rhs)
        val codeBody = labelledCodeGen(shift(env) + (v -> 0), body)
        add(codeRhs, addRight(codeBody, List(Slide(1))))
      }
      case Cond(c, i, e, _) => {
        val (codeCL, codeCR) = labelledCodeGen(env, c)
        val (codeIL, codeIR) = labelledCodeGen(env, i)
        val (codeEL, codeER) = labelledCodeGen(env, e)
        val trueLabel = freshLabel
        val falseLabel = freshLabel
        (codeCL ++ codeIL ++ codeEL,
          codeCR ++ List(Jz(LabelledAddress(falseLabel))) ++
          codeIR ++ List(Jmp(LabelledAddress(trueLabel))) ++
          List(Label(falseLabel)) ++ codeER ++ List(Label(trueLabel)))
      }
      case AddPrim(x, y, _) => {
        val code = labelledCodeGen(env, List(x, y))
        addRight(code, List(Add))
      }
      case SubPrim(x, y, _) => {
        val code = labelledCodeGen(env, List(x, y))
        addRight(code, List(Sub)) // TODO: underflow check!
      }
      case MulPrim(x, y, _) => {
        val code = labelledCodeGen(env, List(x, y))
        addRight(code, List(Mul))
      }
      case DivPrim(x, y, _) => {
        val code = labelledCodeGen(env, List(x, y))
        addRight(code, List(Div))
      }
      case EqPrim(x, y, _) => {
        val code = labelledCodeGen(env, List(x, y))
        val falseLabel = freshLabel
        val trueLabel = freshLabel
        addRight(code,
          List(Sub, Jz(LabelledAddress(trueLabel)), PushInt(0),
            Jmp(LabelledAddress(falseLabel)),
            Label(trueLabel), PushInt(1), Label(falseLabel)))
      }
      case LtPrim(x, y, _) => {
        val code = labelledCodeGen(env, List(x, y))
        val falseLabel = freshLabel
        val trueLabel = freshLabel
        addRight(code,
          List(Sub, Jlt(LabelledAddress(trueLabel)), PushInt(0),
            Jmp(LabelledAddress(falseLabel)),
            Label(trueLabel), PushInt(1), Label(falseLabel)))
      }
    }

  /**
   * Generate code for a sequence of expressions.
   *  The environment is shifted as necessary.
   */
  private def labelledCodeGen(env: Env, es: List[Expr]): Code =
    es match {
      case Nil => (Nil, Nil)
      case e :: es => {
        val code1 = labelledCodeGen(env, e)
        val code2 = labelledCodeGen(shift(env), es)
        add(code1, code2)
      }
    }

  /** Generate code to push the free variables from a closure onto the stack */
  private def codeGenPushFvs(env: Env, fvs: List[String]): List[Instruction] =
    fvs match {
      case Nil => Nil
      case fv1 :: fvs1 => Push(env(fv1)) :: codeGenPushFvs(shift(env), fvs1)
    }

  /** Shift all slots in the environment by one */
  private def shift(env: Env): Env =
    env.mapValues(_ + 1)

  /** Add an instruction list to the right part of the code sequence */
  private def addRight(code: Code, instrs: List[Instruction]): Code =
    (code._1, code._2 ++ instrs)

  /** Add to code sequences */
  private def add(code1: Code, code2: Code): Code =
    (code1._1 ++ code2._1, code1._2 ++ code2._2)

  /** Return a fresh label */
  private def freshLabel: String = {
    val lab = "label" + nextLabelNum
    nextLabelNum = nextLabelNum + 1
    lab
  }
}
