package jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.cps_new.sample

/*
Copyright (c) 2015, Hiroshi Yamaguchi (Core Software Group)
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

import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.debug._
import org.scalatest.FunSuite

import scala.language.implicitConversions

class SimpleForkJoinTransformerTest extends FunSuite {
  def hoge(a: Int): Unit = ???

  def piyo(a: Int)(b: Int): Unit = ???

  def fuga(a: Int)(b: Int, c: Int = 30): Unit = ???

  def funi(a: => Int): Unit = ???

  def in: Int = ???

  def bo: Boolean = ???

  def &&(a: Int): Boolean = ???

  class Join[T] {
    def jget(): T = ???
  }

  import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.cps_new.SimpleForkJoinTransformer._

  def parInt(): Int@forkJoin[Join[Int]] = ???

  def parBoolean(): Boolean@forkJoin[Join[Boolean]] = ???

  implicit def toBoolean(i: Int): Boolean = if (i == 0) false else true

  test("simple cases") {
    println(show(SimpleForkJoinTransform.forkJoin {
      println(hoge(10))
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      println("hoge" + 1)
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      println(Math.sqrt(10))
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      Array(10)(0)
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      piyo(10)(20)
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      println(Math.sqrt(10))
      println("hoge" + 1)
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      (bo, in) match {
        case (a, b) => false;
        case _ => true
      }
    }))
  }

  test("@ simple cases @") {
    println(show(SimpleForkJoinTransform.forkJoin {
      println(parInt())
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      println(parInt() + 1)
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      println(Math.sqrt(parInt()))
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      Array(parInt())(parInt())
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      piyo(parInt())(parInt())
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      println(Math.sqrt(parInt()))
      println(parInt() + 1)
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      (parBoolean(), parInt()) match {
        case (a, b) => false;
        case _ => true
      }
    }))
  }

  test("@ annotations 1 @") {
    class A extends scala.annotation.TypeConstraint

    class Join[T] {
      def jget(): T@A = ???
    }

    def parInt(): Int@forkJoin[Join[Int]] = ???

    println(show(SimpleForkJoinTransform.forkJoin {
      println(parInt())
    }))
  }

  test("@ annotations 2 @") {
    class A extends scala.annotation.TypeConstraint

    class Join[T] {
      def jget(): T = ???
    }

    def parInt(): Int@forkJoin[Join[Int]@A] = ???

    println(show(SimpleForkJoinTransform.forkJoin {
      println(parInt())
    }))
  }

  test("@ isolate par @") {
    println(show(SimpleForkJoinTransform.forkJoin {
      parInt()
      parBoolean()
      println()
    }))
  }

  // no expasion
  test("by-names") {
    println(show(SimpleForkJoinTransform.forkJoin {
      funi(1 + in)
    }))

    // short circuit operator
    println(show(SimpleForkJoinTransform.forkJoin {
      bo && (bo && bo)
    }))
  }

  // expand
  test("not by-name") {
    // not short circuit operator
    println(show(SimpleForkJoinTransform.forkJoin {
      &&(in + 1)
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      if (&&(in + 1)) 10 else 20
    }))

    // prefix is not short circuit
    println(show(SimpleForkJoinTransform.forkJoin {
      bo && bo && bo
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      if (bo && bo && bo) 10 else 20
    }))
  }

  test("complex pattern") {
    val a = new SimpleForkJoinTransformerTest
    println(show(SimpleForkJoinTransform.forkJoin {
      a.hoge(10)
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      fuga(10)(b = 20)
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      fuga(10)(20, c = 30)
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      val a: String = "piyo"
      println(a)
    }))
  }

  test("type apply") {

    println(show(SimpleForkJoinTransform.forkJoin {
      None.asInstanceOf[Option[Int]]
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      None.isInstanceOf[Option[Int]]
    }))
  }

  test("if / match / try") {
    println(show(SimpleForkJoinTransform.forkJoin {
      if (bo) 1 else 2
    }))

    // empty guards
    println(show(SimpleForkJoinTransform.forkJoin {
      bo match {
        case _ => 1
      }
    }))

    // empty catch
    println(show(SimpleForkJoinTransform.forkJoin {
      try {
        bo
      }
    }))
  }

  test("@ if / match / try @") {
    println(show(SimpleForkJoinTransform.forkJoin {
      if (parBoolean()) 1 else 2
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      println(if (bo) parInt() else 2)
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      parBoolean() match {
        case _ => 1
      }
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      println(bo match {
        case _ => parInt()
      })
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      println(try {
        parBoolean()
      })
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      println(try {
        bo
      } catch {
        case _ => parBoolean()
      })
    }))
  }

  test("complex cases 2") {
    println(show(SimpleForkJoinTransform.forkJoin {
      val a = {
        val a = 10
        a + 20
      } + 30
      println(a)
      a
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      val a = 5
      val b = {
        val a = 10
        a + 20
      } + 30
      println(a)
      a
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      (a: Int) => {
        a
      }
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      if (bo)
        1
      else
        true
    }))
  }

  test("select expansion") {
    import scala.concurrent.Promise
    println(show(SimpleForkJoinTransform.forkJoin {
      Promise.successful(if (bo) 10 else 20).trySuccess(10)
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      Promise.successful(if (bo) 10 else 20)
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      println(if (bo) 10 else 20)
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      Promise.successful(if (bo) 10 else 20).future
    }))

    println(show(SimpleForkJoinTransform.forkJoin {
      val a = scala.Predef
      Promise.successful(if (bo) 10 else 20).future.failed
    }))
  }

  test("def") {
    println(show {
      def f(a: Int): Unit = {
        SimpleForkJoinTransform.forkJoin {
          val b = a
          println(b * a)
        }
      }
    })
  }
}
