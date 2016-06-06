/*
 * Copyright (c) 2012, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following
 *   disclaimer in the documentation and/or other materials provided
 *   with the distribution.
 * - Neither the name of the TU Berlin nor the names of its
 *   contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package de.tuberlin.uebb.comp1.covm

import instructions._

/** Debug level during machine execution */
sealed abstract class DebugLevel
/** No debug output */
case object DebugNone extends DebugLevel
/** The stack is traced */
case object DebugTrace extends DebugLevel
/** Stack and heap are fully dumped */
case object DebugDump extends DebugLevel

/**
 * A machine which executes [[instructions.Instruction]]s.
 *
 * This implementation is [[heap]]-based and all values (numbers and tuples) are stored in the [[heap]].
 * The [[stack]] contains only pointers into the [[heap]] or into the code segment. Every [[instructions.Instruction]]
 * occupies exactly one word in the code segment, regardless of the possible space requirement
 * of an operand.
 *
 * On run time errors like overflow, underflow, division by zero, stack over-/underflow, invalid
 * address, or memory exhaustion, the machine is stuck. The machine aborts with an error message.
 *
 * @constructor Create a new machine with a stackSize and heapSize.
 * @param stackSize size of the stack (default 16)
 * @param heapSize size of the heap (default 32)
 * @param debugLevel debug level
 */
class COVM(
  stackSize: Int = 512,
  heapSize: Int = 4096,
  debugLevel: DebugLevel = DebugNone) extends MemoryManagement(stackSize, heapSize) {

  /**
   * Starts execution of a [[instructions.Instruction]] sequence, beginning at position 0.
   */
  def start(c: List[Instruction]): Unit = {
    run(c)
    if (sp != 1) {
      die("Expected stack to have size 1")
    }
  }

  /**
   *  Prints the dereferenced pointer on top of the stack.
   *  This method should only be called,
   *  after the machine finished executing code successfully.
   */
  def printResult(): Unit = {
    getResult() match {
      case Left(msg) => println("ERROR: " + msg)
      case Right(msg) => println("Result: " + msg)
    }
  }

  /**
   * Get result of machine run.
   *  Returns Right if the stack is of size 1, Left otherwise
   */
  def getResult(): Either[String, String] = {
    if (sp != 1) {
      Left("Stack size " + sp + " /= 1, not valid result")
    } else {
      Right(showStackWord(sp - 1))
    }
  }

  /**
   * Return a string containing the current instruction pointer
   *  and the current instruction.
   */
  def showCurrentInstruction(c: List[Instruction], ip: Int): String = {
    val ipString = "ip = " + ip

    ipString + (" " * (10 - ipString.length)) + " { " + c(ip) + " }"
  }
  /**
   * Trace the machine's state: print the current instruction
   *  pointer and stack.
   */
  private def traceState(c: List[Instruction], ip: Int): Unit = {
    val stack = showStack()
    val instr = showCurrentInstruction(c, ip)
    println(instr + " " * (30 - instr.length) + stack + "\n")
  }

  /**
   * Dump the machine state: print the current instruction, the
   *  stack and the heap.
   */
  private def dumpState(c: List[Instruction], ip: Int): Unit = {
    val stack = showStackPlain()
    val instr = showCurrentInstruction(c, ip)
    val heap = showHeapPlain()
    println(instr + "\n\n" + stack + "\n" + heap)
    println("~" * 80)
  }

  /**
   * Executes a [[instructions.Instruction]] sequence starting at position 0.
   * The machine adds the [[heapSize]] to every pointer into the code segment, to differentiate it from [[heap]] refs.
   * So if the ref is smaller than the [[heapSize]], it's a [[heap]] ref, else a code ref.
   */
  private def run(c: List[Instruction]): Unit = {
    var ip = 0
    while (ip < c.size && ip >= 0) {
      debugLevel match {
        case DebugNone => ()
        case DebugTrace => traceState(c, ip)
        case DebugDump => dumpState(c, ip)
      }
      c(ip) match {
        case Call => {
          val a = pop()
          // Adding heapSize to return address to mark it as a pointer into the code segment
          push(ip + 1 + heapSize)
          // Subtracting heapSize, to get real code ref
          ip = a - heapSize
        }
        case Ret => {
          val w = pop()
          val a = pop()
          push(w)
          // Subtracting heapSize, to get real code ref
          ip = a - heapSize
        }
        case PushInt(n) => {
          val a = allocInt(n)
          push(a)
          ip = ip + 1
        }
        case PushAddr(a) =>
          // Adding heapSize to mark it as a pointer into the code segment
          push(unwrapPointer(a) + heapSize); ip = ip + 1
        case Push(i) => {
          push(stack(sp - 1 - i))
          ip = ip + 1
        }
        case Slide(i) => {
          if (sp - 1 - i < 0)
            die("Slide " + i + " from " + sp + " not possible")
          stack(sp - 1 - i) = stack(sp - 1)
          sp = sp - i
          ip = ip + 1
        }
        case Swap => {
          val w0 = pop()
          val w1 = pop()
          push(w0)
          push(w1)
          ip = ip + 1
        }
        case Add => {
          val y = popInt()
          val x = popInt()
          val nL = x.toLong + y.toLong
          if (nL < Int.MinValue) die("Add underflow: " + x + " + " + y)
          if (nL > Int.MaxValue) die("Add overflow: " + x + " + " + y)
          val n = nL.toInt
          val a = allocInt(n)
          push(a)
          ip = ip + 1
        }
        case Sub => {
          val y = popInt()
          val x = popInt()
          val nL = x.toLong - y.toLong
          if (nL < Int.MinValue) die("Sub underflow: " + x + " - " + y)
          if (nL > Int.MaxValue) die("Sub overflow: " + x + " - " + y)
          val n = nL.toInt
          val a = allocInt(n)
          push(a)
          ip = ip + 1
        }
        case Mul => {
          val y = popInt()
          val x = popInt()
          val nL = x.toLong * y.toLong
          if (nL < Int.MinValue) die("Mul underflow: " + x + "  * " + y)
          if (nL > Int.MaxValue) die("Mul overflow: " + x + "  * " + y)
          val n = nL.toInt
          val a = allocInt(n)
          push(a)
          ip = ip + 1
        }
        case Div => {
          val y = popInt()
          val x = popInt()
          if (y == 0) die("Division by zero")
          val n = x / y
          val a = allocInt(n)
          push(a)
          ip = ip + 1
        }
        case Jmp(a: Address) => ip = unwrapPointer(a)
        case Jz(a: Address) =>
          if (popInt() == 0)
            ip = unwrapPointer(a)
          else ip = ip + 1
        case Jlt(a: Address) =>
          if (popInt() < 0)
            ip = unwrapPointer(a)
          else ip = ip + 1
        case Jgt(a: Address) =>
          if (popInt() > 0)
            ip = unwrapPointer(a)
          else ip = ip + 1
        case Pack(i: Int) => {
          val a = allocTuple(i)
          for (j <- i to 1 by -1) {
            val b = pop()
            heap(a + j) = b
          }
          push(a)
          ip = ip + 1
        }
        case Unpack(i: Int) => {
          val ptr = pop()
          val tag = heap(ptr) & 0xFF // Tag part
          if (tag != TUPLE_TAG) die("Expected Ref pointing to tuple")
          val count = (heap(ptr) - TUPLE_TAG) >> 8
          if (i > count) die("Can not unpack " + i + "-th element of a tuple with size " + count)
          push(heap(ptr + i + 1))
          ip = ip + 1
        }
        case Stop => return ()
        case Label(s) => die("Expected executable instruction, but found Label: " + s)
        case Abort(s) => die("Abort: " + s)
      }
    }
    die("Instruction pointer not valid: " + ip)
  }

  /**
   * Gets an [[Address]] ''ad'' and returns a [[de.tuberlin.uebb.comp1.machine.Ref]]
   * if ''ad'' is a [[de.tuberlin.uebb.comp1.machine.Pointer]],
   * dies if it's a [[LabeledAddress]]. This [[Machine]] can't work with [[LabeledAddress]].
   */
  private def unwrapPointer(ad: Address): Ref = ad match {
    case Pointer(i) => i
    case _ => die("Expected Pointer, but found: " + ad)
  }

  /**
   * Pops the top element ''x'' from the stack.
   * If ''x'' is a valid reference to an int on the heap, returns the int,
   * else throws an error.
   */
  private def popInt(): Int = {
    val a = pop()
    if (a < 0 || heapSize < a)
      die("Expected valid Ref: " + a)
    if (heap(a) == INT_TAG)
      heap(a + 1)
    else {
      die("Expected Ref pointing to a int")
    }
  }
}
