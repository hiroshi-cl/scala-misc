package jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.cps_new

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

import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.cps_new.Ticket._
import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.debug._
import org.scalatest.FunSuite

import scala.language.implicitConversions

class TicketProxyTest extends FunSuite {
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

  def parInt(): Int@forkJoin[Join[Int]] = ???

  def parBoolean(): Boolean@forkJoin[Join[Boolean]] = ???

  class M[T] {
    def fmap[U](f: T => U): M[U] = ???

    def bind[U](f: T => U@cps[M[U]]): M[U] = ???
  }

  def reflectInt(): Int@cps[M[Int]] = ???

  def reflectBoolean(): Boolean@cps[M[Boolean]] = ???

  implicit def toBoolean(i: Int): Boolean = if (i == 0) false else true

  def transform[A, B](run: => A)(implicit transformed: TicketProxy[B]): B = transformed.a()

  test("simple cases") {
    println(show(transform {
      println(hoge(10))
    }))

    println(show(transform {
      println("hoge" + 1)
    }))

    println(show(transform {
      println(Math.sqrt(10))
    }))

    println(show(transform {
      Array(10)(0)
    }))

    println(show(transform {
      piyo(10)(20)
    }))

    println(show(transform {
      println(Math.sqrt(10))
      println("hoge" + 1)
    }))

    println(show(transform {
      (bo, in) match {
        case (a, b) => false;
        case _ => true
      }
    }))
  }


  test("@ simple cases (fork/join) @") {
    println(show(transform {
      println(parInt())
    }))

    println(show(transform {
      println(parInt() + 1)
    }))

    println(show(transform {
      println(Math.sqrt(parInt()))
    }))

    println(show(transform {
      Array(parInt())(parInt())
    }))

    println(show(transform {
      piyo(parInt())(parInt())
    }))

    println(show(transform {
      println(Math.sqrt(parInt()))
      println(parInt() + 1)
    }))

    println(show(transform {
      (parBoolean(), parInt()) match {
        case (a, b) => false;
        case _ => true
      }
    }))
  }


  test("@ simple cases (non-blocking) @") {
    class F[T] {
      def jget(): T@cps[F[T]] = ???

      def fmap[U](f: T => U): F[U] = ???

      def bind[U](f: T => U@cps[F[U]]): F[U] = ???
    }

    def parInt(): Int@forkJoin[F[Int]] = ???

    def parBoolean(): Boolean@forkJoin[F[Boolean]] = ???

    println(show(transform {
      println(parInt())
    }))

    println(show(transform {
      println(parInt() + 1)
    }))

    println(show(transform {
      println(Math.sqrt(parInt()))
    }))

    println(show(transform {
      Array(parInt())(parInt())
    }))

    println(show(transform {
      piyo(parInt())(parInt())
    }))

    println(show(transform {
      println(Math.sqrt(parInt()))
      println(parInt() + 1)
    }))

    println(show(transform {
      (parBoolean(), parInt()) match {
        case (a, b) => false;
        case _ => true
      }
    }))
  }


  test("@ another composition @") {
    class F[T] {
      def jget(): T@cps[F[T]] = ???

      def fmap[U](f: T => U): F[U] = ???

      def bind[U](f: T => U@cps[F[U]]): F[U] = ???
    }

    def parCpsInt(): Int@forkJoin[F[Int]@cps[F[F[Int]]]] = ???

    println(show(transform {
      hoge(parCpsInt())
    }))
  }

  // no expasion
  test("by-names") {
    println(show(transform {
      funi(1 + in)
    }))

    // short circuit operator
    println(show(transform {
      bo && (bo && bo)
    }))
  }

  // expand
  test("not by-name") {
    // not short circuit operator
    println(show(transform {
      &&(in + 1)
    }))

    println(show(transform {
      if (&&(in + 1)) 10 else 20
    }))

    // prefix is not short circuit
    println(show(transform {
      bo && bo && bo
    }))

    println(show(transform {
      if (bo && bo && bo) 10 else 20
    }))
  }


  test("@ simple cases (cps) @") {
    println(show(transform {
      println(reflectInt())
    }))

    println(show(transform {
      println(reflectInt() + 1)
    }))

    println(show(transform {
      println(Math.sqrt(reflectInt()))
    }))

    println(show(transform {
      Array(reflectInt())(reflectInt())
    }))

    println(show(transform {
      piyo(reflectInt())(reflectInt())
    }))

    println(show(transform {
      println(Math.sqrt(reflectInt()))
      println(reflectInt() + 1)
    }))

    println(show(transform {
      (reflectBoolean(), reflectInt()) match {
        case (a, b) => false;
        case _ => true
      }
    }))
  }



  test("@ simple cases (cps type) @") {
    println(show {
      val t = transform {
        println(reflectInt())
      }
      t
    })
  }

  test("complex pattern") {
    val a = new TicketProxyTest
    println(show(transform {
      a.hoge(10)
    }))

    println(show(transform {
      fuga(10)(b = 20)
    }))

    println(show(transform {
      fuga(10)(20, c = 30)
    }))

    println(show(transform {
      val a: String = "piyo"
      println(a)
    }))
  }

  test("type apply") {

    println(show(transform {
      None.asInstanceOf[Option[Int]]
    }))

    println(show(transform {
      None.isInstanceOf[Option[Int]]
    }))
  }

  test("if / match / try") {
    println(show(transform {
      if (bo) 1 else 2
    }))

    // empty guards
    println(show(transform {
      bo match {
        case _ => 1
      }
    }))

    // empty catch
    println(show(transform {
      try {
        bo
      }
    }))
  }

  test("complex cases 2") {
    println(show(transform {
      val a = {
        val a = 10
        a + 20
      } + 30
      println(a)
      a
    }))

    println(show(transform {
      val a = 5
      val b = {
        val a = 10
        a + 20
      } + 30
      println(a)
      a
    }))

    println(show(transform {
      (a: Int) => {
        a
      }
    }))

    println(show(transform {
      if (bo)
        1
      else
        true
    }))
  }

  test("select expansion") {
    import scala.concurrent.Promise
    println(show(transform {
      Promise.successful(if (bo) 10 else 20).trySuccess(10)
    }))

    println(show(transform {
      Promise.successful(if (bo) 10 else 20)
    }))

    println(show(transform {
      println(if (bo) 10 else 20)
    }))

    println(show(transform {
      Promise.successful(if (bo) 10 else 20).future
    }))

    println(show(transform {
      val a = scala.Predef
      Promise.successful(if (bo) 10 else 20).future.failed
    }))
  }

  test("def") {
    println(show {
      def f(a: Int): Unit = {
        transform {
          val b = a
          println(b * a)
        }
      }
    })
  }

  // multiple try-catch bug
  test("@ nested if @") {
    class F[T] {
      def jget(): T@cps[F[T]] = ???

      def fmap[U](f: T => U): F[U] = ???

      def bind[U](f: T => U@cps[F[U]]): F[U] = ???
    }

    def parInt(): Int@forkJoin[F[Int]] = ???

    def parBoolean(): Boolean@forkJoin[F[Boolean]] = ???

    println(show(transform {
      if (bo)
        in
      else {
        parInt() + in
      }
    }))

    println(show(transform {
      if (!bo)
        in
      else if (bo)
        in
      else {
        parInt() + in
      }
    }))

    println(show(transform {
      if (bo)
        in
      else if (!bo)
        in
      else if (bo)
        in
      else {
        parInt() + in
      }
    }))
  }

  // multiple try-catch bug
  test("@ path-dependent if @") {
    class F[T] {
      def jget(): T@cps[F[T]] = ???

      def fmap[U](f: T => U): F[U] = ???

      def bind[U](f: T => U@cps[F[U]]): F[U] = ???
    }

    def parInt(): Int@forkJoin[F[Int]] = ???

    def parBoolean(): Boolean@forkJoin[F[Boolean]] = ???


    println(show(transform {
      println(if (bo) parInt() else in) // not bug
    }))

    println(show(transform {
      val r = if (bo) parInt() else in // bug
      println(r)
    }))
  }
}
