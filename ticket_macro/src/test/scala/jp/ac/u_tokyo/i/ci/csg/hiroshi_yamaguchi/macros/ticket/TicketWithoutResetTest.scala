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

import org.scalatest.FunSuite
import TicketBuilder._

class TicketWithoutResetTest extends FunSuite {

  test("none") {
    assert(ticketBuilder[Int] {
      10
    }.run == 10)
  }

  test("simple") {
    assert(ticketBuilder[Int] {
      tearOff((k: Int => Int) => k(10), (x: Int) => (k: Int => Int) => k(x)) * 10
    }.run == 100)
  }

  test("laziness") {
    val sb = StringBuilder.newBuilder
    assert {
      ticketBuilder[Int] {
        val y = tearOff((k: Int => Int) => k(10), (x: Int) => (k: Int => Int) => {
          sb.append("piyo: " + x).append('\n')
          k(x)
        })
        sb.append("hoge: " + 20).append('\n')
        y * 10
      }.run == 100 && sb.mkString.trim ==
        """hoge: 20
          |piyo: 10
          | """.stripMargin.trim
    }
  }

  test("Argument Type Modification") {
    val sb = StringBuilder.newBuilder
    assert {
      ticketBuilder[Int] {
        val y = tearOff((k: String => Int) => k("10"), (x: String) => (k: Int => Int) => {
          sb.append("piyo: " + x).append('\n')
          k(x.toInt)
        })
        sb.append("hoge: " + 20).append('\n')
        y * 10
      }.run == 100 && sb.mkString.trim ==
        """hoge: 20
          |piyo: 10
          | """.stripMargin.trim
    }
  }

  test("double") {
    val sb = StringBuilder.newBuilder
    assert {
      ticketBuilder[Int] {
        val y1 = tearOff((k: String => Int) => k("10"), (x: String) => (k: Int => Int) => {
          sb.append("hoge: " + x).append('\n')
          k(x.toInt)
        })
        val y2 = tearOff((k: String => Int) => k("20"), (x: String) => (k: Int => Int) => {
          sb.append("piyo: " + x).append('\n')
          k(x.toInt)
        })
        y1 * y2
      }.run == 200 && sb.mkString.trim ==
        """hoge: 10
          |piyo: 20
          | """.stripMargin.trim
    }
  }
  test("double (reverse order)") {
    val sb = StringBuilder.newBuilder
    assert {
      ticketBuilder[Int] {
        val y1 = tearOff((k: String => Int) => k("10"), (x: String) => (k: Int => Int) => {
          sb.append("hoge: " + x).append('\n')
          k(x.toInt)
        })
        val y2 = tearOff((k: String => Int) => k("20"), (x: String) => (k: Int => Int) => {
          sb.append("piyo: " + x).append('\n')
          k(x.toInt)
        })
        y2 * y1
      }.run == 200 && sb.mkString.trim ==
        """piyo: 20
          |hoge: 10
          | """.stripMargin.trim
    }
  }
  test("inline") {
    assert {
      ticketBuilder[Int] {
        tearOff((k: String => Int) => k("10"), (x: String) => (k: Int => Int) => k(x.toInt)) *
          tearOff((k: String => Int) => k("20"), (x: String) => (k: Int => Int) => k(x.toInt))
      }.run == 200
    }
  }

  test("methodize") {
    def passAsStr(i: Int) = tearOff((k: String => Int) => k(i.toString), (x: String) => (k: Int => Int) => k(x.toInt))

    assert(ticketBuilder[Int] {
      passAsStr(10) * passAsStr(20)
    }.run == 200)
  }

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent._
  import scala.concurrent.duration._

  def await[T](v: Future[T]) = tearOff(
    (k: Future[T] => Future[T]) => k(v),
    (x: Future[T]) => (k: T => Future[T]) => x.flatMap(k)
  )

  test("async") {

    assert(Await.result(ticketBuilder[Future[Int]] {
      val v = await(Future(10)) * await(Future(20))
      Promise.successful(v).future
    }.run, Duration.Inf) == 200)

    assert(Await.result(ticketBuilder[Future[Int]] {
      Promise.successful(await(Future(10)) * await(Future(20))).future
    }.run, Duration.Inf) == 200)
  }

  def fib(n: Int): Future[Int] =
    ticketBuilder[Future[Int]] {
      if (n < 2)
        n
      else
        await(fib(n - 1)) + await(fib(n - 2))
    }.map(_.map(Promise.successful(_).future).run).run

  test("fib 20") {
    assert(Await.result(fib(20), Duration.Inf) == 6765)
  }
  test("fib 25") {
    assert(Await.result(fib(25), Duration.Inf) == 75025)
  }
  test("fib 30") {
    assert(Await.result(fib(30), Duration.Inf) == 832040)
  }
}
