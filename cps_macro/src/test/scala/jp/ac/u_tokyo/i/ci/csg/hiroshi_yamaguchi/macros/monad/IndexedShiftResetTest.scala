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
import monad.IndexedShiftMonad._
import monad.IndexedMonad._
import cps.Reflect
import Reflect._
import debug._
import org.scalatest.FunSuite

class IndexedShiftResetTest extends FunSuite {
  test("shift") {
    assert(shiftMonad[Int, Int].reify {
      val a = shift { k: (Int => Int) =>
        k(k(1))
      } + 1 //(1)
      a * 2 //(2)
    }.ctl(p => p) == 10)
  }
  test("reset") {
    assert(reset[Int, Int] {
      val a = shift { k: (Int => Int) =>
        k(k(1))
      } + 1 //(1)
      a * 2 //(2)
    } == 10)
  }
  test("inline") {
    assert(reset[Int, Int] {
      (shift { k: (Int => Int) =>
        k(k(1))
      } + 1) * 2
    } == 10)
  }

  test("complex") {
    assert {
      val sb = StringBuilder.newBuilder
      sb.append(reset[Int, Int] {
        val a = shift { k: (Int => Int) =>
          k(k(1))
        } + 1 //(1)
        sb.append(a).append(' ')
        a * shift { k: (Int => Int) =>
          k(k(1))
        } + 2 //(2)
      })
      sb.mkString == "2 11 145"
    }
  }

  test("shift/reset tutorial") {
    assert(reset[Int, Int] {
      3 + shift { k: (Int => Int) => 5 * 2} - 1
    } == 10)
  }
  test("Okmij 1") {
    assert(
      10 + reset[Int, Int] {
        2 + shift {
          k: (Int => Int) =>
            100 + k(k(3))
        }
      } == 117)
  }
  test("Okmij 2") {
    assert(
      10 * reset[Int, Int] {
        2 * shift {
          g: (Int => Int) =>
            reset[Int, Int] {
              5 * shift {
                f: (Int => Int) =>
                  f(1) + 1
              }
            }
        }
      } == 60)
  }
  test("Okmij 3") {
    assert {
      val f = (x: Int) => shift {
        k: (Int => Int) =>
          k(k(x))
      }
      1 + reset[Int, Int] {
        10 + f(100)
      } == 121
    }
  }

  test("Answer Type Modification 1") {
    assert {
      reset[String, Int] {
        3 + shift { k: (Int => Int) => "piyo"} - 1
      } == "piyo"
    }
  }

  test("Answer Type Modification 2") {
    assert {
      val at = reset[String => String, String] {
        "@@@@ " + shift {
          k: (String => String) =>
            k(_)
        } + " @@@@"
      }
      at("hoge") == "@@@@ hoge @@@@"
    }
  }

  def string[R]() = shift {
    k: (String => R) =>
      k(_)
  }

  test("Answer Type Modification fail (generic lambda)") {
    assert(expectException(parse(
      """reset[String => String, String] {
        |  "@@@@ " + string() + " @@@@"
        |}""")))
  }

  test("Answer Type Modification 3") {
    assert {
      val at = reset[String => String, String] {
        "@@@@ " + string[String]() + " @@@@"
      }
      at("hoge") == "@@@@ hoge @@@@"
    }
  }

  test("Answer Type Modification 4") {
    assert {
      val at = reset[String => String => String, String] {
        "@@@@ " + string[String => String]() + " @@@@ " + string[String]()
      }
      at("hoge")("piyo") == "@@@@ hoge @@@@ piyo"
    }
  }
}
