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

import org.scalatest.FunSuite

import scala.language.implicitConversions

import scala.concurrent._
import duration._
import ExecutionContext.Implicits.global

class ForkJoinTest extends FunSuite {

  import ForkJoinTest._
  import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.cps_new.SimpleForkJoinTransformer._

  def par[T](run: => T): T@forkJoin[Future[T]] = throw new ForkJoinReflect(Future(run))
  def purePar[T](run: => T): T@forkJoin[Future[T]] = run

  test("simple cases") {
    SimpleForkJoinTransform.forkJoin {
      println(par(10))
    }

    SimpleForkJoinTransform.forkJoin {
      println(par("hoge" + 1))
    }

    SimpleForkJoinTransform.forkJoin {
      println(par(Math.sqrt(10)))
    }

    SimpleForkJoinTransform.forkJoin {
      println(par(Array(10)(0)))
    }

    SimpleForkJoinTransform.forkJoin {
      println(par(Math.sqrt(10)))
      println(par("hoge" + 1))
    }
  }


  test("par") {
    SimpleForkJoinTransform.forkJoin {
      val a = par {
        println("a")
        Thread.sleep(4)
        val v = Math.sqrt(10)
        println(v)
        v
      }
      val b = par {
        println("b")
        Thread.sleep(2)
        val v = "hoge" + 1
        println(v)
        v
      }
      Thread.sleep(1)
      println("last?")
      println(a + b)
    }
  }


  test("pure par") {
    SimpleForkJoinTransform.forkJoin {
      val a = purePar {
        println("a")
        Thread.sleep(4)
        val v = Math.sqrt(10)
        println(v)
        v
      }
      val b = purePar {
        println("b")
        Thread.sleep(2)
        val v = "hoge" + 1
        println(v)
        v
      }
      Thread.sleep(1)
      println("last?")
      println(a + b)
    }
  }
}

object ForkJoinTest {

  implicit class Join[T](val future: Future[T]) extends AnyVal {
    def jget(): T = Await.result(future, Duration.Inf)
  }
}

