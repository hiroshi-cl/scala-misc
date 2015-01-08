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

class KNFTransformerTest extends FunSuite {
  def hoge(a: Int): Unit = ???

  def piyo(a: Int)(b: Int): Unit = ???

  def fuga(a: Int)(b: Int, c: Int = 30): Unit = ???

  def funi(a: => Int): Unit = ???

  def in: Int = ???

  def bo: Boolean = ???

  def &&(a: Int): Boolean = ???

  implicit def toBoolean(i: Int): Boolean = if (i == 0) false else true

  def compare(code1: String)(code2: String) {
    val c1 = replaceFreshVariables(code1)
    val c2 = replaceFreshVariables(code2)
    assert(c1 == c2)
  }

  test("simple cases") {
    compare(show(Sample.knfTransform {
      println(hoge(10))
    }))( """({
           |  <synthetic> <artifact> val fresh$macro$1: Unit = KNFTransformerTest.this.hoge(10);
           |  scala.this.Predef.println(fresh$macro$1)
           |}: Unit)""".stripMargin)

    compare(show(Sample.knfTransform {
      println("hoge" + 1)
    }))( """({
           |  <synthetic> <artifact> val fresh$macro$2: String = "hoge".+(1);
           |  scala.this.Predef.println(fresh$macro$2)
           |}: Unit)""".stripMargin)

    compare(show(Sample.knfTransform {
      println(Math.sqrt(10))
    }))( """({
           |  <synthetic> <artifact> val fresh$macro$3: Double = java.this.lang.Math.sqrt(10.0);
           |  scala.this.Predef.println(fresh$macro$3)
           |}: Unit)""".stripMargin)

    compare(show(Sample.knfTransform {
      Array(10)(0)
    }))( """({
           |  <synthetic> <artifact> val fresh$macro$5: Array[Int] = scala.Array.apply(10, <empty>);
           |  fresh$macro$5.apply(0)
           |}: Int)""".stripMargin)

    compare(show(Sample.knfTransform {
      piyo(10)(20)
    }))( """(KNFTransformerTest.this.piyo(10)(20): Unit)""".stripMargin)

    compare(show(Sample.knfTransform {
      println(Math.sqrt(10))
      println("hoge" + 1)
    }))( """({
           |  <synthetic> <artifact> val $$: Double = java.this.lang.Math.sqrt(10.0);
           |  <synthetic> <artifact> val $$: Unit = scala.this.Predef.println($$);
           |  <synthetic> <artifact> val $$: String = "hoge".+(1);
           |  scala.this.Predef.println($$)
           |}: Unit)""".stripMargin)

    compare(show(Sample.knfTransform {
      (bo, in) match {
        case (a, b) => false;
        case _ => true
      }
    }))( """({
           |  <synthetic> <artifact> val fresh$macro$1: (Boolean, Int) = scala.Tuple2.apply[Boolean, Int](KNFTransformerTest.this.bo, KNFTransformerTest.this.in);
           |  fresh$macro$1 match {
           |    case (_1: Boolean, _2: Int)(Boolean, Int)((a @ _), (b @ _)) => false
           |    case _ => true
           |  }
           |}: Any)""".stripMargin)

  }

  // no expasion
  test("by-names") {
    compare(show(Sample.knfTransform {
      funi(1 + in)
    }))("(KNFTransformerTest.this.funi(1.+(KNFTransformerTest.this.in)): Unit)")

    // short circuit operator
    compare(show(Sample.knfTransform {
      bo && (bo && bo)
    }))("(KNFTransformerTest.this.bo.&&(KNFTransformerTest.this.bo.&&(KNFTransformerTest.this.bo)): Boolean)")
  }

  // expand
  test("not by-name") {
    // not short circuit operator
    compare(show(Sample.knfTransform {
      &&(in + 1)
    }))( """({
           |  <synthetic> <artifact> val $$: Int = KNFTransformerTest.this.in.+(1);
           |  KNFTransformerTest.this.&&($$)
           |}: Boolean)""".stripMargin)

    compare(show(Sample.knfTransform {
      if (&&(in + 1)) 10 else 20
    }))( """({
           |  <synthetic> <artifact> val $$: Int = KNFTransformerTest.this.in.+(1);
           |  <synthetic> <artifact> val $$: Boolean = KNFTransformerTest.this.&&($$);
           |  if ($$)
           |    10
           |  else
           |    20
           |}: Any)""".stripMargin)

    // prefix is not short circuit
    compare(show(Sample.knfTransform {
      bo && bo && bo
    }))( """({
           |  <synthetic> <artifact> val $$: Boolean = KNFTransformerTest.this.bo.&&(KNFTransformerTest.this.bo);
           |  $$.&&(KNFTransformerTest.this.bo)
           |}: Boolean)""".stripMargin)

    compare(show(Sample.knfTransform {
      if (bo && bo && bo) 10 else 20
    }))( """({
           |  <synthetic> <artifact> val $$: Boolean = KNFTransformerTest.this.bo.&&(KNFTransformerTest.this.bo);
           |  <synthetic> <artifact> val $$: Boolean = $$.&&(KNFTransformerTest.this.bo);
           |  if ($$)
           |    10
           |  else
           |    20
           |}: Any)""".stripMargin)
  }

  test("complex pattern") {
    val a = new KNFTransformerTest
    compare(show(Sample.knfTransform {
      a.hoge(10)
    }))("(a.hoge(10): Unit)")

    compare(show(Sample.knfTransform {
      fuga(10)(b = 20)
    }))( """({
           |  <artifact> val x$1: Int = 10;
           |  <artifact> val x$2: Int = 20;
           |  <artifact> val x$3: Int = KNFTransformerTest.this.fuga$default$3(x$1);
           |  KNFTransformerTest.this.fuga(x$1)(x$2, x$3)
           |}: Unit)""".stripMargin)

    compare(show(Sample.knfTransform {
      fuga(10)(20, c = 30)
    }))("(KNFTransformerTest.this.fuga(10)(20, 30): Unit)")

    compare(show(Sample.knfTransform {
      val a: String = "piyo"
      println(a)
    }))( """({
           |  val a: String = "piyo";
           |  scala.this.Predef.println(a)
           |}: Unit)""".stripMargin)
  }

  test("type apply") {

    compare(show(Sample.knfTransform {
      None.asInstanceOf[Option[Int]]
    }))("(scala.None.asInstanceOf[Option[Int]]: Option[Int])")

    compare(show(Sample.knfTransform {
      None.isInstanceOf[Option[Int]]
    }))("(scala.None.isInstanceOf[Option[Int]]: Boolean)")
  }

  test("if / match / try") {
    compare(show(Sample.knfTransform {
      if (bo) 1 else 2
    }))( """(if (KNFTransformerTest.this.bo)
           |  1
           |else
           |  2: Any)""".stripMargin)

    // empty guards
    compare(show(Sample.knfTransform {
      bo match {
        case _ => 1
      }
    }))( """(KNFTransformerTest.this.bo match {
           |  case _ => 1
           |}: Any)""".stripMargin)

    // empty catch
    compare(show(Sample.knfTransform {
      try {
        bo
      }
    }))( """(try {
           |  KNFTransformerTest.this.bo
           |}: Any)""".stripMargin)
  }

  test("complex cases 2") {
    compare(show(Sample.knfTransform {
      val a = {
        val a = 10
        a + 20
      } + 30
      println(a)
      a
    }))( """({
           |  <synthetic> <artifact> val $$: Int = {
           |    val a: Int = 10;
           |    a.+(20)
           |  };
           |  val a: Int = $$.+(30);
           |  <synthetic> <artifact> val $$: Unit = scala.this.Predef.println(a);
           |  a
           |}: Int)""".stripMargin)

    compare(show(Sample.knfTransform {
      val a = 5
      val b = {
        val a = 10
        a + 20
      } + 30
      println(a)
      a
    }))( """({
           |  val a: Int = 5;
           |  <synthetic> <artifact> val $$: Int = {
           |    val a: Int = 10;
           |    a.+(20)
           |  };
           |  val b: Int = $$.+(30);
           |  <synthetic> <artifact> val $$: Unit = scala.this.Predef.println(a);
           |  a
           |}: Int)""".stripMargin)

    compare(show(Sample.knfTransform {
      (a: Int) => {
        a
      }
    }))( """(((a: Int) => a): Int => Int)""".stripMargin)

    compare(show(Sample.knfTransform {
      if (bo)
        1
      else
        true
    }))( """(if (KNFTransformerTest.this.bo)
           |  1
           |else
           |  true: Any)""".stripMargin)
  }

  test("select expansion") {
    import scala.concurrent.Promise
    compare(show(Sample.knfTransform {
      Promise.successful(if (bo) 10 else 20).trySuccess(10)
    }))( """({
           |  <synthetic> <artifact> val $$: Int = if (KNFTransformerTest.this.bo)
           |    10
           |  else
           |    20;
           |  <synthetic> <artifact> val $$: scala.concurrent.Promise[Int] = scala.concurrent.Promise.successful[Int]($$);
           |  $$.trySuccess(10)
           |}: Boolean)""".stripMargin)

    compare(show(Sample.knfTransform {
      Promise.successful(if (bo) 10 else 20)
    }))( """({
           |  <synthetic> <artifact> val $$: Int = if (KNFTransformerTest.this.bo)
           |    10
           |  else
           |    20;
           |  scala.concurrent.Promise.successful[Int]($$)
           |}: scala.concurrent.Promise[Int])""".stripMargin)

    compare(show(Sample.knfTransform {
      println(if (bo) 10 else 20)
    }))( """({
           |  <synthetic> <artifact> val $$: Int = if (KNFTransformerTest.this.bo)
           |    10
           |  else
           |    20;
           |  scala.this.Predef.println($$)
           |}: Unit)""".stripMargin)

    compare(show(Sample.knfTransform {
      Promise.successful(if (bo) 10 else 20).future
    }))( """({
           |  <synthetic> <artifact> val $$: Int = if (KNFTransformerTest.this.bo)
           |    10
           |  else
           |    20;
           |  <synthetic> <artifact> val $$: scala.concurrent.Promise[Int] = scala.concurrent.Promise.successful[Int]($$);
           |  $$.future
           |}: scala.concurrent.Future[Int])""".stripMargin)

    compare(show(Sample.knfTransform {
      val a = scala.Predef
      Promise.successful(if (bo) 10 else 20).future.failed
    }))( """({
           |  val a: scala.Predef.type = scala.Predef;
           |  <synthetic> <artifact> val $$: Int = if (KNFTransformerTest.this.bo)
           |    10
           |  else
           |    20;
           |  <synthetic> <artifact> val $$: scala.concurrent.Promise[Int] = scala.concurrent.Promise.successful[Int]($$);
           |  $$.future.failed
           |}: scala.concurrent.Future[Throwable])""".stripMargin)
  }
}
