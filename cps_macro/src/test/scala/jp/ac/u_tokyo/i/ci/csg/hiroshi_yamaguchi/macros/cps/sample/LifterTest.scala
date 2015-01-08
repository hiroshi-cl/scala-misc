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

class LifterTest extends FunSuite {
  def hoge(a: Int): Unit = ???

  def piyo(a: Int)(b: Int): Unit = ???

  def fuga(a: Int)(b: Int, c: Int = 30): Unit = ???

  def funi(a: => Int): Unit = ???

  def in: Int = ???

  def bo: Boolean = ???

  def &&(a: Int): Boolean = ???

  import Sample._

  test("simple move") {
    assert(show(insBV {
      val a = List(10)
      val b = dummy(a)
      b
    }) == """({
            |  val a: Int = dummy[Int](immutable.this.List.apply[Int](10));
            |  val b: Int = a;
            |  b
            |}: Int)""".stripMargin)
  }


  test("redundant sub (transitive)") {
    assert(show(insBV {
      val a = List(10)
      val c = a
      val b = dummy(c)
      b
    }) == """({
            |  val a: Int = dummy[Int](immutable.this.List.apply[Int](10));
            |  val b: Int = a;
            |  b
            |}: Int)""".stripMargin)
  }

  test("redundant sub　(fork)") {
    assert(show(insBV {
      val a = List(10)
      val c = a
      val d = c
      val b = dummy(d)
      dummy(c)
    }) == """({
            |  val a: Int = dummy[Int](immutable.this.List.apply[Int](10));
            |  val b: Int = a;
            |  a
            |}: Int)""".stripMargin)
  }

  test("if") {
    assert(show(insBV {
      val a = List(in)
      val b = if (bo) dummy(a) else 1
      b
    }) == """({
            |  val a: Int = dummy[Int](immutable.this.List.apply[Int](LifterTest.this.in));
            |  val b: Int = if (LifterTest.this.bo)
            |    a
            |  else
            |    1;
            |  b
            |}: Int)""".stripMargin)
  }

  def compare(code1: String)(code2: String) {
    val c1 = replaceFreshVariables(code1)
    val c2 = replaceFreshVariables(code2)
    assert(c1 == c2)
  }

  test("lift simple") {
    compare(show(lifter {
      val a = dummy(List(in))
      println(a)
    }))( """({
           |  <synthetic> <artifact> val fresh$macro$1: List[Int] = immutable.this.List.apply[Int](LifterTest.this.in);
           |  <synthetic> <artifact> val fresh$macro$2: Int => Unit = ((a: Int) => scala.this.Predef.println(a));
           |  fresh$macro$1.map[Unit, List[Unit]](fresh$macro$2)(immutable.this.List.canBuildFrom[Unit])
           |}: List[Unit])""".stripMargin)
  }

  test("lift double") {
    compare(show(lifter {
      val a = dummy(List(in))
      val b = dummy(List(in))
      a + b
    }))( """({
           |  <synthetic> <artifact> val fresh$macro$3: List[Int] = immutable.this.List.apply[Int](LifterTest.this.in);
           |  <synthetic> <artifact> val fresh$macro$6: Int => List[Int] = ((a: Int) => {
           |    <synthetic> <artifact> val fresh$macro$4: List[Int] = immutable.this.List.apply[Int](LifterTest.this.in);
           |    <synthetic> <artifact> val fresh$macro$5: Int => Int = ((b: Int) => a.+(b));
           |    fresh$macro$4.map[Int, List[Int]](fresh$macro$5)(immutable.this.List.canBuildFrom[Int])
           |  });
           |  fresh$macro$3.flatMap[Int, List[Int]](fresh$macro$6)(immutable.this.List.canBuildFrom[Int])
           |}: List[Int])""".stripMargin)
  }

  test("lift if") {
    compare(show(lifter {
      val a = List(in)
      val b = if (bo) dummy(a) else 1
      b
    }))( """({
           |  val a: List[Int] = immutable.this.List.apply[Int](LifterTest.this.in);
           |  <synthetic> <artifact> val $$: List[Int] = if (LifterTest.this.bo)
           |    a
           |  else
           |    {
           |      <synthetic> <artifact> val $$: Int = 1;
           |      immutable.this.List.apply[Int]($$)
           |    };
           |  <synthetic> <artifact> val $$: Int => Int = ((b: Int) => b);
           |  $$.map[Int, List[Int]]($$)(immutable.this.List.canBuildFrom[Int])
           |}: List[Int])""".stripMargin)
  }

}
