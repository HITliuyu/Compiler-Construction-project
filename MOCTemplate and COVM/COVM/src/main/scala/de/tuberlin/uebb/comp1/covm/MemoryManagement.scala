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

/**
 * This implements a stack, a heap and a stop-the-world mark-and-sweep garbage collector.
 *
 * @param stackSize size of the stack
 * @param heapSize size of the heap
 */
abstract class MemoryManagement(val stackSize: Int, val heapSize: Int) {
  /** A reference */
  type Ref = Int
  /** The size of a memory chunk */
  type Count = Int

  /** Represents invalid data */
  val NIL = 0
  /** Indicates that a memory chunk in the [[heap]] is free */
  val FREE_TAG = 0x1
  /**
   * Indicates that a memory chunk in the [[heap]] contains an int
   * An int in heap consist of 2 words: the INT_TAG and the int value.
   */
  val INT_TAG = 0x2
  /**
   * Indicates that a memory chunk in the [[heap]] contains a tuple
   * A tuple in memory contains a word with the tag and the width: {{{heap(ptr) = TUPLE_TAG + (width << 8)}}}
   * followed by the references of the tuple elements.
   */
  val TUPLE_TAG = 0x4

  /**
   * Heap of size heapSize
   * Every chunk in the heap is at least 2 words big.
   * The size of a chunk is always a multiple of 2,
   * since the smallest manageable element in the free list allocates 2 words.
   */
  val heap = new Array[Int](heapSize)
  /**
   * Pointer to the head of the free list.
   * Initialized to 2, since we use 0 as an invalid address,
   * to indicate that there is no free memory
   * and mark the end of the free list.
   * An element of the free list consist of at least 2 words.
   * The first word contains the [[FREE_TAG]] and the size: {{{ heap(fp) = FREE_TAG + (size << 8) }}}
   * The second word contains the pointer to the next element in the free list.
   */
  private var fp: Ref = 2
  heap(fp) = FREE_TAG + (heapSize - 2 << 8)
  heap(fp + 1) = NIL

  /** Stack of size stackSize */
  val stack = new Array[Int](stackSize)
  /**
   * This stack pointer always points to the first free position on the stack.
   * So, stack(sp - 1) is top element of the stack.
   */
  var sp: Ref = 0

  /** Pushes a ref onto the [[stack]] and increments [[sp]]. */
  protected def push(ptr: Ref): Unit = {
    if (sp == stackSize) {
      die("Stack overflow")
    }
    stack(sp) = ptr
    sp = sp + 1
  }

  /** Returns a ref from the top of the [[stack]] and decrements [[sp]]. */
  protected def pop(): Ref = {
    if (sp <= 0)
      die("pop on empty stack")
    val ptr: Ref = stack(sp - 1)
    sp = sp - 1
    stack(sp) = NIL
    ptr
  }

  /** Allocate an int on the [[heap]]. */
  protected def allocInt(v: Int): Ref = {
    val ptr = alloc(1)
    heap(ptr) = INT_TAG
    heap(ptr + 1) = v
    return ptr
  }

  /** Allocate a tuple on the [[heap]]. */
  protected def allocTuple(width: Count): Ref = {
    val ptr = alloc(width)
    heap(ptr) = TUPLE_TAG + (width << 8)
    return ptr
  }

  /**
   * Allocate memory. Returns the reference to the allocated space.
   *
   * The allocated space is at least ''size'' + 1.
   * Every memory chunk size is a multiple of 2,
   * because and entry in the free list needs 2 words.
   */
  protected def alloc(size: Count): Ref = {
    var ptr: Ref = -1
    var realSize: Count = -1

    if (size % 2 == 0)
      realSize = size + 2
    else
      realSize = size + 1

    if (fp == NIL) {
      gc()
      ptr = reserve(realSize)
      if (ptr == NIL)
        die("Out of heap memory")
    } else {
      ptr = reserve(realSize)
      if (ptr == NIL) {
        gc()
        ptr = reserve(realSize)
        if (ptr == NIL)
          die("Out of heap memory")
      }
    }
    return ptr
  }

  /**
   * Get a memory chunk of sufficient size from the free list and
   *  removes it from the free list
   */
  protected def reserve(size: Count): Ref = {
    var ptr: Ref = -1
    var prev: Ref = -1

    // First chunk fits perfectly, fp is advanced
    if ((heap(fp) >> 8) == size) {
      ptr = fp
      fp = heap(fp + 1)
      return ptr
    }

    // First chunk is split, fp is advanced
    if ((heap(fp) >> 8) > size) {
      ptr = fp
      fp = fp + size
      heap(fp) = FREE_TAG + ((heap(ptr) >> 8) - size << 8)
      heap(fp + 1) = heap(ptr + 1)
      return ptr
    }

    // First chunk too small, find matching one
    if ((heap(fp) >> 8) < size) {
      ptr = heap(fp + 1)
      prev = fp
      var next: Boolean = true

      while (ptr != NIL && next) {
        if ((heap(ptr) >> 8) == size) { // Current chunk fits perfectly
          heap(prev + 1) = heap(ptr + 1)
          next = false // break
        }

        if ((heap(ptr) >> 8) > size) { // Split current chunk
          heap(prev + 1) = ptr + size
          heap(ptr + size) = FREE_TAG + ((heap(ptr) >> 8) - size << 8)
          heap(ptr + size + 1) = heap(ptr + 1)
          next = false
        }

        if ((heap(ptr) >> 8) < size) { // Look for next one
          prev = ptr
          ptr = heap(prev + 1)
        }
      }
      return ptr
    }
    return -1 // Should not happen, here for the compiler
  }

  /** A stop-the-world mark-and-sweep garbage collector. */
  def gc(): Unit = {
    mark()
    sweep()
  }

  /**  Pushes the root set onto the stack and marks the root set. */
  protected def mark(): Unit = {
    var i: Ref = -1
    var sp_old: Ref = -1
    var ptr: Ref = -1
    var tag: Int = -1
    var width: Count = -1
    sp_old = sp

    ptr = 0
    while (ptr < sp_old) { // Push the root set onto the stack
      push(stack(ptr))
      ptr = ptr + 1
    }

    while (sp > sp_old) {
      ptr = pop()
      // If the refs points into the code segment, we skip it.
      // The machine adds the heapSize to every pointer into the code segment, to subtract it from heap refs.
      // If the ref is less than the heapSize, it is a heap ref, a code ref otherwise.
      if (ptr < heapSize) {
        tag = heap(ptr) & 0xFF // Tag part of tag/size word
        if ((tag & 0x80) == 0) { // Data tag without mark bit
          tag match {
            case INT_TAG => {
              heap(ptr) = heap(ptr) | 0x80 // Mark this chunk
            }
            case TUPLE_TAG =>
              {
                heap(ptr) = heap(ptr) | 0x80 // Mark this chunk
                width = heap(ptr) >> 8 // Width of the tuple
                for (i <- 1 to width) { // Push all chunks the tuple
                  push(heap(ptr + i)) // references onto the stack
                }
              }
            case x =>
              die("Memory corruption in mark " + x)
          }
        }
      }
    }
  }

  /**
   * Adds every unmarked [[heap]] element to the free list.
   * Adjacent elements in the free list are merged.
   */
  protected def sweep(): Unit = {
    var ptr: Ref = -1
    var tag: Int = -1
    var marked: Int = -1
    var width: Int = -1
    var size: Int = -1
    var old_fp: Ref = -1
    var lf = 0 // Last freed block
    var lfSize = 0 // Size of last freed block

    ptr = 2 // Scan entire heap

    while (ptr < heapSize) {
      tag = heap(ptr) & 0xFF
      marked = tag & 0x80

      (tag & 0x7F) match { // Switch on tag without mark bit
        case FREE_TAG => // Free chunks are only skipped
          size = heap(ptr) >> 8
          ptr += size
          lf = ptr
          lfSize = size
        case INT_TAG =>
          if (marked != 0) {
            heap(ptr) = INT_TAG // Unmark
          } else {
            // This chunk and lf are not adjacent
            if (lf == 0 || lf + lfSize != ptr) {
              // Add to free list
              heap(ptr) = FREE_TAG + (2 << 8)
              heap(ptr + 1) = fp
              fp = ptr
              // Set this chunk to last freed 
              lf = fp
              lfSize = 2
            } else // This chunk and lf are adjacent, so we merge them
            {
              heap(lf) = FREE_TAG + ((lfSize + 2) << 8) // lf chunk gets bigger by size of int
              lfSize = lfSize + 2
            }
          }
          ptr += 2
        case TUPLE_TAG =>
          width = heap(ptr) >> 8 // Width of the tuple          
          size = if (width % 2 == 0) width + 2 else width + 1 // Size of the chunk
          if (marked != 0) {
            heap(ptr) = TUPLE_TAG + (width << 8) // Unmark
          } else {
            // This chunk and lf are not adjacent
            if (lf == 0 || lf + lfSize != ptr) {
              heap(ptr) = FREE_TAG + ((size) << 8) // Add to free list
              heap(ptr + 1) = fp
              fp = ptr
              // Set this chunk to last freed 
              lf = fp
              lfSize = size
            } else // This chunk and lf are adjacent, so we merge them
            {
              heap(lf) = FREE_TAG + ((lfSize + size) << 8) // lf chunk gets bigger by size of tuple
              lfSize = lfSize + size
            }
          }
          ptr += size
        case _ =>
          die("Memory corruption in sweep")
      }
    }
  }

  /**
   * This exception represents run time errors of the [[MemoryManagement]] or the [[COVM]].
   */
  case class MachineRuntimeException(err: String) extends Exception(err)

  /**
   * Stops the [[COVM]] with a [[MachineRuntimeException]].
   */
  protected def die(s: String) = {
    throw new MachineRuntimeException(s)
  }

  /**
   * Convert the stack to a string with top of the stack
   *  at the left
   */
  protected def showStack(): String = {
    val strings = for (p <- sp - 1 to 0 by -1) yield showStackWord(p)
    (strings :+ "[]").mkString(" : ")
  }

  /** Convert the indicated stack word to a string */
  protected def showStackWord(p: Int): String = {
    if (p < 0 || p >= sp) {
      throw new RuntimeException("Bug in trace mode: invalid stack reference " + p)
    } else {
      val ptr = stack(p)
      showDerefPointer(ptr)
    }
  }

  /**
   * Convert the dereferenced pointer to a string. Both, heap
   *  and code pointers are handled.
   */
  protected def showDerefPointer(ptr: Int): String = {
    if (ptr < heapSize && ptr >= 0) {
      val tag = heap(ptr) & 0xFF
      tag match {
        case INT_TAG => heap(ptr + 1).toString
        case TUPLE_TAG => {
          val count = (heap(ptr) - TUPLE_TAG) >> 8
          val ptrs = heap.slice(ptr + 1, ptr + 1 + count)
          val strings = ptrs.map(showDerefPointer)
          "<" + strings.mkString(",") + ">"
        }
        case FREE_TAG => {
          val size = heap(ptr) >> 8
          val next = heap(ptr + 1)
          "free " + size + ", next " + next
        }
        case _ => "???"
      }
    } else if (ptr >= heapSize) {
      "@" + (ptr - heapSize)
    } else {
      throw new RuntimeException("Trace mode: hit negative pointer")
    }
  }

  /**
   * Converts the [[heap]] to a multi line string.
   */
  def showHeapPlain() = {
    "- Heap -------------------------\n" +
      "- Free pointer = " + fp + "\n" +
      "--------------------------------\n" +
      (for (i <- 0 until heapSize)
        yield i + "\t" + heap(i) + "\t0x" + Integer.toHexString(heap(i))
        + "\t" + showDerefPointer(i)).mkString("\n") + "\n" +
      "- End of heap ------------------\n"
  }

  /**
   * Converts the [[stack]] to a multi line string.
   */
  def showStackPlain() = {
    "- Stack ------------------------\n" +
      "- Stack pointer = " + sp + "\n" +
      "--------------------------------\n" +
      (for (i <- 0 until stackSize)
        yield i + "\t" + stack(i)).mkString("\n") + "\n" +
      "- End of stack -----------------\n"
  }
}
