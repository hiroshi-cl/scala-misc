package jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.monad

/*
Copyright (c) 2014, Hiroshi Yamaguchi (Core Software Group)
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros._
import monad.Monads._
import monad.Monad._
import cps.Reflect
import Reflect._
import org.scalatest.FunSuite

class ListMonadTest extends FunSuite {

  val plus = for (a <- List(1, 2, 3, 4, 5); b <- List(10, 20, 30, 40, 50)) yield a + b
  val plus2 = for (a <- List(1, 2, 3, 4, 5); b <- List(10, 20, 30, 40, 50)) yield a + b + a

  test("reflect 1") {
    assert(listMonad.reify {
      val a: Int = listMonad.reflect(1 +: 2 +: 3 +: 4 +: 5 +: Nil)
      val b: Int = listMonad.reflect(10 +: 20 +: 30 +: 40 +: 50 +: Nil)
      a + b
    } == plus)
  }
  test("reflect 2") {
    assert(listMonad.reify {
      val a: Int = listMonad.reflect(List(1, 2, 3, 4, 5))
      val b: Int = listMonad.reflect(List(10, 20, 30, 40, 50))
      a + b
    } == plus)
  }
  test("inline") {
    assert(listMonad.reify {
      listMonad.reflect(List(1, 2, 3, 4, 5)) + listMonad.reflect(List(10, 20, 30, 40, 50))
    } == plus)
  }
  test("for-yield") {
    assert(listMonad.reify {
      for (a <- List(1, 2, 3, 4, 5);
           b <- List(10, 20, 30, 40, 50)) yield
        a + b
    } == List(plus))
  }
  test("ordering 1") {
    assert(listMonad.reify {
      val a: Int = listMonad.reflect(List(1, 2, 3, 4, 5))
      val b: Int = listMonad.reflect(List(10, 20, 30, 40, 50))
      b + a
    } == plus)
  }
  test("ordering 2") {
    assert(listMonad.reify {
      val b: Int = listMonad.reflect(List(10, 20, 30, 40, 50))
      val a: Int = listMonad.reflect(List(1, 2, 3, 4, 5))
      a + b
    } != plus)
  }
  test("not-by-name 1") {
    assert(listMonad.reify {
      val a: Int = listMonad.reflect(List(1, 2, 3, 4, 5))
      val b: Int = listMonad.reflect(List(10, 20, 30, 40, 50))
      a + b + a
    } == plus2)
  }
  test("not-by-name 2") {
    assert(listMonad.reify {
      val a: Int = listMonad.reflect(List(1, 2, 3, 4, 5))
      val b: Int = listMonad.reflect(List(10, 20, 30, 40, 50))
      a + a + b
    } == plus2)
  }
  test("nil") {
    assert(listMonad.reify {
      val a: Int = List(1, 2, 3).reflect()
      val b: Int = Nil.reflect()
      a + b + a
    } == Nil)
  }
}
