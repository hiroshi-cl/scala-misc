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
import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.byname._
import monad.Monads._
import monad.RelaxedMonad._
import cps.Reflect
import Reflect._
import org.scalatest.FunSuite

import org.scalatest.FunSuite
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.SpanSugar._

class AsyncTest extends FunSuite with TimeLimitedTests {

  val timeLimit = 50000 millis

  import scala.concurrent._
  import scala.concurrent.duration._
  import ExecutionContext.Implicits.global

  {
    def fib(n: Int): Future[Int] =
      asyncMonad.reify {
        if (n < 2)
          n
        else {
          val xf = fib(n - 1)
          val yf = fib(n - 2)
          xf.reflect() + yf.reflect()
        }
      }

    test("warm up run (1)") {
      Await.result(fib(20), Duration.Inf)
      Await.result(fib(25), Duration.Inf)
    }

    test("fib 20 (1)") {
      assert(Await.result(fib(20), Duration.Inf) == 6765)
    }
    test("fib 25 (1)") {
      assert(Await.result(fib(25), Duration.Inf) == 75025)
    }
    test("fib 30 (1)") {
      assert(Await.result(fib(30), Duration.Inf) == 832040)
    }
  }

  // library level scala-async
  {
    @inline
    def async[T](body: => T)(implicit f: ByNameProxy[RelaxedMonad[Future[_]], Future[T]]): Future[T] = f.a

    @inline
    def await[T](future: Future[T]): Reflect[T, Future[T]] =
      new Reflect[T, Future[T]](future)

    def fib(n: Int): Future[Int] =
      async {
        if (n < 2)
          n
        else {
          val xf = fib(n - 1)
          val yf = fib(n - 2)
          await(xf) + await(yf)
        }
      }

    test("warm up run (2)") {
      Await.result(fib(20), Duration.Inf)
      Await.result(fib(25), Duration.Inf)
    }

    test("fib 20 (2)") {
      assert(Await.result(fib(20), Duration.Inf) == 6765)
    }
    test("fib 25 (2)") {
      assert(Await.result(fib(25), Duration.Inf) == 75025)
    }
    test("fib 30 (2)") {
      assert(Await.result(fib(30), Duration.Inf) == 832040)
    }
  }

  test("concurrency") {
    val v = new concurrent.SyncVar[Int]()
    v.put(0)

    def waiting(i: Int) {
      while (v.get < i)
        Thread.sleep(1)
      v.put(v.take + 1)
    }

    def check() = asyncMonad.reify {
      waiting(0)
      val hoge = asyncMonad.reify {
        waiting(2)
        waiting(4)
        v.get
      }
      waiting(1)
      waiting(3)
      println(hoge.reflect)
      waiting(5)
      v.get
    }

    Await.result(check(), Duration.Inf)
  }
}
