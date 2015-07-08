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

import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.language.implicitConversions


import scala.language.implicitConversions

class FutureTest extends FunSuite {

  import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.cps_new.FutureTest._

  test("simple cases") {

    println(show(ticket {
      println(par(10))
    }))

    println(show(ticket {
      println(par("hoge") + 1)
    }))

    println(show(ticket {
      println(Math.sqrt(par(10)))
    }))

    println(show(ticket {
      Array(par(10))(par(0))
    }))

    println(show(ticket {
      println(Math.sqrt(par(10)))
      println(par(20) + 1)
    }))

    println(show(ticket {
      (par(true), par(10)) match {
        case (a, b) => false;
        case _ => true
      }
    }))
  }


  test("simple cases (run)") {

    syncGet(ticket {
      println("hoge")
      println(par(10))
    })

    println(syncGet(ticket {
      par("hoge") + 1
    }))

    println(syncGet(ticket {
      Math.sqrt(par(10))
    }))

    println(syncGet[Int](ticket {
      Array(par(10))(par(0))
    }))

    syncGet(ticket {
      println(Math.sqrt(par(10)))
      println(par(20) + 1)
    })

    println(syncGet(ticket {
      (par(true), par(10)) match {
        case (a, b) => false;
        case _ => true
      }
    }))
  }

  test("par") {
    syncGet(ticket {
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
    })
  }

  test("pure par") {
    syncGet(ticket {
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
    })
  }
}


object FutureTest {

  import ExecutionContext.Implicits.global

  implicit class FutureAux[T](val f: Future[T]) extends AnyVal {
    def jget(): T@cps[Future[T]] = throw new CPSReflect(f)

    def fmap[U](fun: T => U): Future[U] = f.map(fun)

    def bind[U](fun: T => U@cps[Future[U]]): Future[U] = {
      val p = Promise[U]
      f.onComplete { x =>
        try {
          p.success(fun(x.get))
        } catch {
          case e: CPSReflect[Future[U]] => e.value.onComplete { y =>
            p.success(y.get)
          }
        }
      }
      p.future
    }
  }

  def par[T](run: => T): T@forkJoin[Future[T]] = throw new ForkJoinReflect(Future.apply(run))

  def purePar[T](run: => T): T@forkJoin[Future[T]] = run

  def syncGet[T](run: => T) =
    try {
      run
    } catch {
      case e: CPSReflect[Future[T]] => Await.result(e.value, Duration.Inf)
    }
}
