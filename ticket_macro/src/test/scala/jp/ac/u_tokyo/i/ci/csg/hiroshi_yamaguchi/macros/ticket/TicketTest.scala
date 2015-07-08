package jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.ticket

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

import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.byname.ByNameProxy
import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.ticket.TicketBuilder._
import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.debug._
import org.scalatest.FunSuite
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.SpanSugar._

class TicketTest extends FunSuite with TimeLimitedTests {

  val timeLimit = 50000 millis

  test("none") {
    assert(reset {
      10
    } == 10)
  }

  test("simple") {
    assert(reset {
      tearOff((k: Int => Int) => k(10), (x: Int) => (k: Int => Int) => k(x)) * 10
    } == 100)
  }

  test("laziness") {
    val sb = StringBuilder.newBuilder
    assert {
      reset {
        val y = tearOff((k: Int => Int) => k(10), (x: Int) => (k: Int => Int) => {
          sb.append("piyo: " + x).append('\n')
          k(x)
        })
        sb.append("hoge: " + 20).append('\n')
        y * 10
      } == 100 && sb.mkString.trim ==
        """hoge: 20
          |piyo: 10
          | """.stripMargin.trim
    }
  }

  test("Argument Type Modification") {
    val sb = StringBuilder.newBuilder
    assert {
      reset {
        val y = tearOff((k: String => Int) => k("10"), (x: String) => (k: Int => Int) => {
          sb.append("piyo: " + x).append('\n')
          k(x.toInt)
        })
        sb.append("hoge: " + 20).append('\n')
        y * 10
      } == 100 && sb.mkString.trim ==
        """hoge: 20
          |piyo: 10
          | """.stripMargin.trim
    }
  }

  test("double") {
    val sb = StringBuilder.newBuilder
    assert {
      reset {
        val y1 = tearOff((k: String => Int) => k("10"), (x: String) => (k: Int => Int) => {
          sb.append("hoge: " + x).append('\n')
          k(x.toInt)
        })
        val y2 = tearOff((k: String => Int) => k("20"), (x: String) => (k: Int => Int) => {
          sb.append("piyo: " + x).append('\n')
          k(x.toInt)
        })
        y1 * y2
      } == 200 && sb.mkString.trim ==
        """hoge: 10
          |piyo: 20
          | """.stripMargin.trim
    }
  }
  test("double (reverse order)") {
    val sb = StringBuilder.newBuilder
    assert {
      reset {
        val y1 = tearOff((k: String => Int) => k("10"), (x: String) => (k: Int => Int) => {
          sb.append("hoge: " + x).append('\n')
          k(x.toInt)
        })
        val y2 = tearOff((k: String => Int) => k("20"), (x: String) => (k: Int => Int) => {
          sb.append("piyo: " + x).append('\n')
          k(x.toInt)
        })
        y2 * y1
      } == 200 && sb.mkString.trim ==
        """piyo: 20
          |hoge: 10
          | """.stripMargin.trim
    }
  }
  test("inline") {
    assert {
      reset {
        tearOff((k: String => Int) => k("10"), (x: String) => (k: Int => Int) => k(x.toInt)) *
          tearOff((k: String => Int) => k("20"), (x: String) => (k: Int => Int) => k(x.toInt))
      } == 200
    }
  }

  test("methodize") {
    def passAsStr(i: Int) = tearOff((k: String => Int) => k(i.toString), (x: String) => (k: Int => Int) => k(x.toInt))

    assert(reset {
      passAsStr(10) * passAsStr(20)
    } == 200)
  }

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent._
  import scala.concurrent.duration._

  def await[T](v: Future[T]) = tearOff(
    (k: Future[T] => Future[T]) => k(v),
    (x: Future[T]) => (k: T => Future[T]) => x.flatMap(k)
  )

  def async[T](thunk: => T)(implicit f: ByNameProxy[TicketBuilder[Future[T]], Unit => Ticket[T, Future[T]]]):
  Future[T] = Promise.successful(()).future.flatMap((_: Unit) => reset(Promise.successful[T](f.a()).future))

  //  Future[T] = Promise.successful(()).future.flatMap((_: Unit) => reset{ Promise.successful[T](f.a).future}) // TODO

  test("async") {
    assert(Await.result(async {
      await(Future(10)) * await(Future(20))
    }, Duration.Inf) == 200)
  }

  def fib(n: Int): Future[Int] =
    async[Int] {
      if (n < 2)
        n
      else
        await(fib(n - 1)) + await(fib(n - 2))
    }

  test("warm up run") {
    Await.result(fib(20), Duration.Inf)
    Await.result(fib(25), Duration.Inf)
  }
  test("fib 20") {
    assert(Await.result(fib(20), Duration.Inf) == 6765)
  }
  test("fib 25") {
    assert(Await.result(fib(25), Duration.Inf) == 75025)
  }

  test("concurrency") {
    val v = new concurrent.SyncVar[Int]()
    v.put(0)

    def waiting(i: Int) {
      while (v.get < i)
        Thread.sleep(1)
      v.put(v.take + 1)
    }

    def check() = async {
      waiting(0)
      val hoge = await(Future {
        waiting(2)
        waiting(4)
        v.get
      })
      waiting(1)
      waiting(3)
      println(hoge: Int)
      waiting(5)
      v.get
    }

    Await.result(check(), Duration.Inf)
  }

    test("concurrency 2") { // corner case
      val v = new concurrent.SyncVar[Int]()
      v.put(0)

      def waiting(i: Int) {
        while (v.get < i)
          Thread.sleep(1)
        v.put(v.take + 1)
      }

      def piyo = async {
        waiting(2)
        waiting(4)
        v.get
      }

      def check() = async {
        waiting(0)
        val hoge = await(piyo)
        waiting(1)
        waiting(3)
        println(hoge: Int)
        waiting(5)
        v.get
      }

      Await.result(check(), Duration.Inf)
    }
}
