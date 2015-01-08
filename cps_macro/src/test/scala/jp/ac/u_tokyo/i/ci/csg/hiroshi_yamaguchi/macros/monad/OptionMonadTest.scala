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
import debug._
import cps.Reflect
import Reflect._
import org.scalatest.FunSuite

class OptionMonadTest extends FunSuite {

  test("pure") {
    assert(optionMonad.reify(10) == optionMonad.unit(10))
    assert(optionMonad.reify(println(10)) == optionMonad.unit(println(10)))
  }

  test("None") {
    assert {
      val s = StringBuilder.newBuilder
      val n = optionMonad.reify {
        val piyo: Int = None.reflect()
        s.append(piyo)
      }
      n == None && s.isEmpty
    }
    assert {
      val s = StringBuilder.newBuilder
      val n = optionMonad.reify {
        s.append(Some(1).reflect)
        s.append(None.reflect) // skip
        s.append(2) // skip
      }
      n == None && s.mkString == "1"
    }
    assert {
      val s = StringBuilder.newBuilder
      val n = optionMonad.reify {
        s.append(Some(3).reflect)
        s.append(None.reflect) // skip
        s.append(Some(4).reflect) // skip
      }
      n == None && s.mkString == "3"
    }
  }

  test("rget") {
    import OptionMonadTest._

    assert {
      val s = StringBuilder.newBuilder
      val n = optionMonad.reify {
        val p = Some(50).rget()
        s.append(p * 1)
        s.append(None.rget())
      }
      n == None && s.mkString == "50"
    }

    // require dummy plus
    assert {
      val s = StringBuilder.newBuilder
      val n = optionMonad.reify {
        val p = Some(50).rget()
        s.append(p + 1)
        s.append(None.rget())
      }
      n == None && s.mkString == "51"
    }
    assert {
      val s = StringBuilder.newBuilder
      val n = optionMonad.reify {
        val p: Int = Some(50).rget()
        s.append(p + 1)
        s.append(None.rget())
      }
      n == None && s.mkString == "51"
    }
  }

  test("if") {
    assert {
      val s = StringBuilder.newBuilder
      val n = optionMonad.reify {
        if (true)
          s.append(Some(5).reflect)
        else
          s.append(Some(6).reflect)
        s.append(7)
        s.append(8)
      }
      n.map(_.mkString) == Some("578")
    }

    assert {
      val s = StringBuilder.newBuilder
      val n = optionMonad.reify {
        s.append(Some(5).reflect)
        if (true)
          s.append(5)
        else
          s.append(6)
        s.append(7)
        s.append(8)
      }
      n.map(_.mkString) == Some("5578")
    }

    // type error (Any)
    assert(expectException(parse(
      """  val s = StringBuilder.newBuilder
        |  val hoge = if (true)
        |    Some(5).reflect
        |  else
        |    6
        |  s.append(hoge)
        |  s.append(hoge + 10)""")))

    // solution of above
    assert {
      val s = StringBuilder.newBuilder
      val n = optionMonad.reify {
        val hoge: Int = if (true)
          Some(5).reflect
        else
          6
        s.append(hoge)
        s.append(hoge + 10)
      }
      n.map(_.mkString) == Some("515")
    }

    assert {
      val s = StringBuilder.newBuilder
      val n = optionMonad.reify {
        if (true)
          s.append(None.reflect)
        else
          s.append(6)
        s.append(7)
        s.append(8)
      }
      n == None && s.isEmpty
    }

    assert {
      val s = StringBuilder.newBuilder
      val n = optionMonad.reify {
        if (true)
          s.append(5)
        else
          s.append(None.reflect)
        s.append(7)
        s.append(8)
      }
      n.map(_.mkString) == Some("578")
    }

  }

  test("misc") {

    // type error (return type of thunk is Reflect)
    assert {
      expectException(applied(optionMonad)("reify") {
        Some(5).reflect
      })
    }

    // solution of above
    assert {
      optionMonad.reify {
        Some(5).reflect: Int
      } == Some(5)
    }

    assert {
      val s = StringBuilder.newBuilder
      val n = optionMonad.reify {
        s.append(Some(5).reflect)
      }
      n.map(_.mkString) == Some("5")
    }

    assert {
      val s = StringBuilder.newBuilder
      val n = optionMonad.reify {
        if (true)
          s.append(5)
        else
          s.append(6)
        s.append(Some(5).reflect)
        s.append(7)
        s.append(8)
      }
      n.map(_.mkString) == Some("5578")
    }

    assert {
      val s = StringBuilder.newBuilder
      val n = {
        def f6 = optionMonad.reify {
          if (true)
            s.append(5)
          else
            s.append(6)
          s.append(Some(5).reflect)
          s.append(7)
          s.append(8)
        }

        f6
      }
      n.map(_.mkString) == Some("5578")
    }

    assert {
      val s = StringBuilder.newBuilder
      val n1 =
        optionMonad.reify {
          s.append(Some(81).reflect)
        }
      val b = n1.map(_.mkString) == Some("81")

      val n2 = optionMonad.reify {
        if (true)
          s.append(82)
        else
          s.append(83)
        s.append(Some(84).reflect)
        s.append(85)
        s.append(86)
      }
      b && n2.map(_.mkString) == Some("8182848586")
    }
  }

  test("methodize") {
    assert {
      val s = StringBuilder.newBuilder

      def or[A](r: Option[A]) = optionMonad.reflect(r)

      def f9 = {
        optionMonad.reify {
          s.append(optionMonad.reflect(Some(91)))
          s.append(optionMonad.reflect(None)) // skip
          s.append(92) // skip
        }

        optionMonad.impure {
          s.append(or(Some(93)))
          s.append(or(None)) // skip
          s.append(or(Some(94))) // skip
        }
      }

      f9
      // f9 is external reference
      s.mkString == "9193" && optionMonad.reify {
        f9.toString
      } == None
    }
  }
}

object OptionMonadTest {

  implicit class ReflectiveOption[A](val o: Option[A]) extends AnyVal {
    def rget(): Reflect[A, Option[A]] = optionMonad.reflect(o)
  }

}
