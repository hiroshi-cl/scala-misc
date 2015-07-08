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

import scala.language.{higherKinds, implicitConversions}
import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.cps_new.CPSTransformer._

class CPSTest extends FunSuite {

  import CPSTest._

  def reflect[T](m: Option[T]): T@cps[Option[T]] = throw new CPSReflect[Option[T]](m)

  def pureReflect[T](v: T): T@cps[Option[T]] = v

  test("simple cases") {
    try {
      println("pure:\n\t" +
        CPSTransform.cpsTransform {
          println(reflect(Some(10)))
        })
    } catch {
      case e: CPSReflect[Option[_]] => println("impure:\n\t" + e.value)
    }

    try {
      println("pure:\n\t" +
        CPSTransform.cpsTransform {
          println(reflect(Some("hoge" + 1)))
        })
    } catch {
      case e: CPSReflect[Option[_]] => println("impure:\n\t" + e.value)
    }

    try {
      println("pure:\n\t" +
        CPSTransform.cpsTransform {
          println(reflect(Some(Math.sqrt(10))))
        })
    } catch {
      case e: CPSReflect[Option[_]] => println("impure:\n\t" + e.value)
    }

    try {
      println("pure:\n\t" +
        CPSTransform.cpsTransform {
          println(reflect(Some(Array(10)(0))))
        })
    } catch {
      case e: CPSReflect[Option[_]] => println("impure:\n\t" + e.value)
    }

    try {
      println("pure:\n\t" +
        CPSTransform.cpsTransform {
          println(reflect(Some(Math.sqrt(10))))
          println(reflect(Some("hoge" + 1)))
        })
    } catch {
      case e: CPSReflect[Option[_]] => println("impure:\n\t" + e.value)
    }

    try {
      println("pure:\n\t" +
        CPSTransform.cpsTransform {
          reflect(Some(Math.sqrt(10))) + reflect(Some("hoge" + 1))
        })
    } catch {
      case e: CPSReflect[Option[_]] => println("impure:\n\t" + e.value)
    }
  }


  test("reflect None") {
    try {
      println("pure:\n\t" +
        CPSTransform.cpsTransform {
          val a = reflect(Some(10))
          println(a)
          val b = reflect(None)
          println(b)
          val c = reflect(Some(20))
          println(c)
        })
    } catch {
      case e: CPSReflect[Option[_]] => println("impure:\n\t" + e.value)
    }
  }


  test("pure reflect") {
    try {
      println("pure:\n\t" +
        CPSTransform.cpsTransform {
          val a = pureReflect(10)
          println(a)
          val b = pureReflect(20)
          println(b)
          val c = pureReflect(30)
          println(c)
        })
    } catch {
      case e: CPSReflect[Option[_]] => println("impure:\n\t" + e.value)
    }
  }
}

object CPSTest {

  implicit class OptionAux[T](val option: Option[T]) extends AnyVal {
    def fmap[U](f: T => U) = option.map(f)

    def bind[U](f: T => U@cps[Option[U]]) = option.flatMap { (x: T) =>
      try {
        Some(f(x))
      } catch {
        case e: CPSReflect[Option[U]] => e.value
      }
    }
  }

}




