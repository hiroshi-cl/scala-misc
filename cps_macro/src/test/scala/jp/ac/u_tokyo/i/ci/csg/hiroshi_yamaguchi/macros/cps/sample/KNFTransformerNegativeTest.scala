package jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.cps.sample

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

import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.debug._
import org.scalatest.FunSuite

import scala.language.implicitConversions

class KNFTransformerNegativeTest extends FunSuite {

  def in: Int = ???

  def bo: Boolean = ???

  // TODO: unapply
  test("unapply 1") {
    assert(expectException(applied(Sample)("knfTransform") {
      {
        case a :+ b => a :+ b
      }: (List[Int] => List[Int])
    }))
  }
  test("unapply 4") {
    assert(expectException(applied(Sample)("knfTransform") {
      (x: List[Int]) => x match {
        case a :+ b => a :+ b;
        case _ => Nil
      }
    }))
  }

  // TODO: support lazy (can we convert using Tickets?)
  test("lazy 1") {
    assert(expectException(applied(Sample)("knfTransform") {
      (x: List[Int]) => x match {
        case a :+ b => a :+ b;
        case _ => Nil
      }
    }))
  }
  test("lazy 2") {
    assert(expectException(applied(Sample)("knfTransform") {
      lazy val a: String = "piyo"
      var b = a
      println(a)
    }))
  }
}
