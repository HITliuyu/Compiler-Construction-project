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
 *     isDotcumentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to enisDotrse or promote products
 *     derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, CLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, DIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (CLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER  CONTRACT, STRICT LIABILITY, OR TORT
 * (CLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * */

package de.tuberlin.uebb.comp1.covm.instructions

/**
 * The following instructions are described as transition between machine states.
 *
 * A machine state is a triple of an instruction pointer ''a'', a code segment ''c...c''
 * and a value stack ''ws'':
 *
 * state ⊆ ''a''×''c...c''×''ws''
 *
 * Valid state transitions are speciﬁed by the transition relation ''state,,1,,'' → ''state,,2,,'' :
 *
 * →⊆ ''state'' × ''state''
 *
 * The transition relation is defined by the following axioms that use a shorthand to indicate
 * that the instruction pointer points to a specific instruction.
 *
 * For ''C'' = ''c,,0,,''...''c,,m,,'', we have
 *
 * ''C,,ip,,''[''c''] = ''c,,0,,...c,,ip−1,, c,,ip,, c,,ip+1,,...c,,m,,''.
 *
 * The initial state for a code segment ''c,,0,,'' ...''c,,m,,'' is
 *
 * (0, ''c,,0,,''...''c,,m,,'' , []).
 */
sealed abstract class Instruction()

/**
 * A Call jumps to the code instruction which is referenced to by the top of the stack,
 * and saves the current instruction pointer on the stack.
 *
 * (''ip'', ''C,,ip,,''[''call''], ''a'' ∶ ''ws'') → (''a'', ''C'', ''ip'' ∶ ''ws'')
 */
case object Call extends Instruction {
  override def toString() = "call"
}

/**
 * A Ret jumps back to the code instruction which is referenced by the return address (second top-most word of the stack)
 * and removes the return address from the stack.
 *
 * (''ip'', ''C,,ip,,''[''ret''], ''w'' ∶ ''a'' ∶ ''ws'') → (''a'', ''C'', ''w'' ∶ ''ws'')
 */
case object Ret extends Instruction {
  override def toString() = "ret"
}

/**
 * PushInt pushes a number constant onto the stack.
 *
 * (''ip'', ''C,,ip,,''[''pushint n''], ''ws'') → (''ip'' + 1, ''C'', n ∶ ''ws'')
 */
case class PushInt(n: Int) extends Instruction {
  override def toString() = "pushint " + n
}

/**
 * PushAddr ''a'' pushes an address constant onto the stack.
 *
 * ''a'' is only allowed to be a [[Pointer]].
 * If ''a'' is a [[LabelledAddress]], the machine stops with an error.
 *
 * (''ip'', ''C,,ip,,''[''pushaddr a''], ''ws'') → (''ip'' + 1, ''C'', a ∶ ''ws'')
 */
case class PushAddr(a: Address) extends Instruction {
  override def toString() = "pushaddr " + a
}

/**
 * Push ''i'' copies the ''i''-th word to the top of the stack.
 *
 * (''ip'', ''C,,ip,,''[''push i''], ''w,,0,,'' ∶ ... ∶ ''w,,i,,'' ∶ ... ∶ ''w,,m,,'' ∶ ''ws'') → (''ip'' + 1, ''C'', ''w,,i,,'' ∶ ''w,,0,,'' ∶ ... ∶ ''w,,i,,'' ∶ ... ∶ ''w,,m,,'' ∶ ''ws'')
 */
case class Push(i: Int) extends Instruction {
  override def toString() = "push " + i
}

/**
 * Slide ''i'' removes ''i'' words from the stack, starting from the second top-most word.
 *
 * (''ip'', ''C,,ip,,''[''slide i''], ''w,,0,,'' ∶ ... ∶ ''w,,i,,'' ∶ ''ws'') → (''ip'' + 1, ''C'', ''w,,0,,'' ∶ ''ws'')
 */
case class Slide(i: Int) extends Instruction {
  override def toString = "slide " + i
}

/**
 * Swap swaps the first two words of the stack.
 *
 * (''ip'', ''C,,ip,,''[''swap''], ''w,,0,,'' ∶ w 1 ∶ ''ws'') → (''ip'' + 1, ''C'', w 1 ∶ ''w,,0,,'' ∶ ''ws'')
 */
case object Swap extends Instruction {
  override def toString() = "swap"
}

/**
 * Add pops the first two numbers from the stack, and pushes their sum onto the stack.
 *
 * In case of and over- or underflow, the machine stops with an error.
 *
 * (''ip'', ''C,,ip,,''[''add''], ''n,,0,,'' ∶ ''n,,1,,'' ∶ ''ws'') → (''ip'' + 1, ''C'', ''n,,1,,'' + ''n,,0,,'' ∶ ''ws'')
 */
case object Add extends Instruction {
  override def toString() = "add"
}

/**
 * Sub pops the first two numbers from the stack, and pushes their difference onto the stack.
 *
 * In case of and over- or underflow, the machine stops with an error.
 *
 * (''ip'', ''C,,ip,,''[''sub''], ''n,,0,,'' ∶ ''n,,1,,'' ∶ ''ws'') → (''ip'' + 1, ''C'', ''n,,1,,'' − ''n,,0,,'' ∶ ''ws'')
 */
case object Sub extends Instruction {
  override def toString() = "sub"
}

/**
 * Mul pops the first two numbers from the stack, and pushes their product onto the stack.
 *
 * In case of and over- or underflow, the machine stops with an error.
 *
 * (''ip'', ''C,,ip,,''[''mul''], ''n,,0,,'' ∶ ''n,,1,,'' ∶ ''ws'') → (''ip'' + 1, ''C'', ''n,,1,,'' · ''n,,0,,'' ∶ ''ws'')
 */
case object Mul extends Instruction {
  override def toString() = "mul"
}

/**
 * Div pops the first two numbers from the stack, and pushes their quotient onto the stack.
 *
 * In case of division by zero, the machine stops with an error.
 *
 * (''ip'', ''C,,ip,,''[''div''], ''n,,0,,'' ∶ ''n,,1,,'' ∶ ''ws'') → (''ip'' + 1, ''C'', ''n,,1,,'' /''n,,0,,'' ∶ ''ws'')
 */
case object Div extends Instruction {
  override def toString() = "div"
}

/**
 * Pack ''i'' pops the first ''i'' words from the stack and pushes them in a tuple of the size ''i'' onto the stack.
 *
 * (''ip'', ''C,,ip,,''[''Pack i''], ''w,,0,,'' ∶ ... ∶ ''w,,i-1,,'' ∶ ''ws'') → (''ip'' + 1, ''C'', 〈''w,,i-1,,'' , ..., ''w,,0,,''〉 ∶ ''ws'')
 */
case class Pack(i: Int) extends Instruction {
  override def toString() = "pack " + i
}

/**
 * Unpack ''i'' pops a tuple ''t'' from the stack and pushes the ''i''-th component of ''t'' onto the stack.
 * If the stack top is not a tuple, the machine stops with an error.
 *
 * (''ip'', ''C,,ip,,''[''Unpack i''], 〈''w,,0,,'' , ..., ''w,,i,,'' , ..., ''w,,m,,'' 〉 ∶ ''ws'') → (''ip'' + 1, ''C'', ''w,,i,,'' ∶ ''ws'')
 */
case class Unpack(i: Int) extends Instruction {
  override def toString() = "unpack " + i
}

/**
 * Jmp ''a'' jumps to the code instruction on position ''a''.
 *
 * ''a'' is only allowed to be a [[Pointer]].
 * If ''a'' is a [[LabelledAddress]], the machine stops with an error.
 *
 * (''ip'', ''C,,ip,,''[''Jmp a''], ''ws'') → (''a'', ''C'', ''ws'')
 */
case class Jmp(a: Address) extends Instruction {
  override def toString() = "jmp " + a
}

/**
 * Jz ''a'' jumps to the code instruction on position ''a'', if the top of the stack is zero.
 * It always removes the top of the stack.
 *
 * If the top of the stack is not a number, the machine stops with an error.
 *
 * ''a'' is only allowed to be a [[Pointer]].
 * If ''a'' is a [[LabelledAddress]], the machine stops with an error.
 *
 * (''ip'', ''C,,ip,,''[''Jz a''], 0 ∶ ''ws'') → (''a'', ''C'', ''ws'')
 * (''ip'', ''C,,ip,,''[''Jz a''], n ∶ ''ws'') → (''ip'' + 1, ''C'', ''ws'') if n ≠ 0
 */
case class Jz(a: Address) extends Instruction {
  override def toString() = "jz " + a
}

/**
 * Jlt ''a'' jumps to the code instruction on position ''a'', if the top of the stack is lesser zero.
 * It always removes the top of the stack.
 *
 * If the top of the stack is not referencing an number, the machine stops with an error.
 *
 * ''a'' is only allowed to be a [[Pointer]].
 * If ''a'' is a [[LabelledAddress]], the machine stops with an error.
 *
 *
 * (''ip'', ''C,,ip,,''[''Jlt a''], n ∶ ''ws'') → (''ip'' + 1, ''C'', ''ws'') if n ≥ 0
 * (''ip'', ''C,,ip,,''[''Jlt a''], n ∶ ''ws'') → (''a'', ''C'', ''ws'')      if n < 0
 */
case class Jlt(a: Address) extends Instruction {
  override def toString() = "jlt " + a
}

/**
 * Jgt ''a'' jumps to the code instruction on position ''a'', if the top of the stack is greater zero.
 * It always removes the top of the stack.
 *
 * If the top of the stack is not referencing an number, the machine stops with an error.
 *
 * ''a'' is only allowed to be a [[Pointer]].
 * If ''a'' is a [[LabelledAddress]], the machine stops with an error.
 *
 *
 * (''ip'', ''C,,ip,,''[''Jgt a''], n ∶ ''ws'') → (''a'', ''C'', ''ws'')      if n > 0
 * (''ip'', ''C,,ip,,''[''Jgt a''], n ∶ ''ws'') → (''ip'' + 1, ''C'', ''ws'') if n ≤ 0
 */
case class Jgt(a: Address) extends Instruction {
  override def toString() = "jgt " + a
}

/**
 * Labels are used to mark positions in a code sequence.
 * They are part of the instruction set to make code generation easier.
 * The [[COVM]] can not handle Labels
 * and will stop with an error if one is encountered.
 */
case class Label(s: String) extends Instruction {
  override def toString() = s + ":"
}

/**
 * Stop signals the valid ending of a code sequence. The machine stops without an error.
 */
case object Stop extends Instruction {
  override def toString() = "stop"
}

/**
 * Abort ''s'' signals an error. The machine stops with the error ''s''.
 */
case class Abort(s: String) extends Instruction {
  override def toString = "abort \"" + s + "\""
}

/**
 * An Address represent a position in a code sequence.
 * The [[COVM]] only works with [[Pointer]]
 * and stops with an error if a [[LabelledAddress]] is encountered.
 */
sealed abstract class Address
/**
 * LabelledAddress refer to [[Label]]s in a code sequence.
 * They should make code generation easier.
 * The [[COVM]] can not handle LabelledAddress
 * and will stop with an error if one is encountered.
 */
case class LabelledAddress(s: String) extends Address
/**
 * Pointer ''i'' refers to the ''i'' position in a code sequence.
 */
case class Pointer(p: Int) extends Address {
  override def toString() = p.toString
}
