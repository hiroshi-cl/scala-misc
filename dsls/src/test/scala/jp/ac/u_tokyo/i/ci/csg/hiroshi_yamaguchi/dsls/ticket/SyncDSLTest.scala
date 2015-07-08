package jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.dsls.ticket

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

import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.ticket.TicketBuilder._
import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.ticket._
import org.scalatest.FunSuite
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.SpanSugar._

class SyncDSLTest extends FunSuite with TimeLimitedTests {

  val timeLimit = 50000 millis

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent._
  import scala.concurrent.duration._

  import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.dsls.ticket.SyncDSL._

  test("sync") {
    assert(dsl {
      sync(10) * sync(20)
    } == 200)
  }

  def fib(n: Int): Int =
    dsl {
      if (n < 2)
        n
      else
        sync(fib(n - 1)) + sync(fib(n - 2))
    }

  test("warm up run") {
    fib(10)
    fib(15)
  }
  test("fib 10") {
    assert(fib(10) == 55)
  }
  test("fib 15") {
    assert(fib(15) == 610)
  }

  test("concurrency") {
    val v = new concurrent.SyncVar[Int]()
    v.put(0)

    def waiting(i: Int) {
      while (v.get < i)
        Thread.sleep(1)
      v.put(v.take + 1)
    }

    def check() = dsl {
      waiting(0)
      val hoge = sync {
        waiting(2)
        waiting(4)
        v.get
      }
      waiting(1)
      waiting(3)
      println(hoge: Int)
      waiting(5)
      v.get
    }

    check()
  }

}
