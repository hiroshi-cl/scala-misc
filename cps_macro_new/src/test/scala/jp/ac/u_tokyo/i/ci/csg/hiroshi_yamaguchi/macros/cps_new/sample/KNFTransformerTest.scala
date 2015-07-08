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

  class TestAnnotation extends scala.annotation.TypeConstraint

  // annotations to return type require TypeConstraint trait

  def hogeAt(a: Int): (Unit@TestAnnotation) = ???

  def piyoAt(a: Int)(b: Int): (Unit@TestAnnotation) = ???

  def fugaAt(a: Int)(b: Int, c: Int = 30): (Unit@TestAnnotation) = ???

  def funiAt(a: => Int): (Unit@TestAnnotation) = ???

  def inAt: (Int@TestAnnotation) = ???

  def boAt: (Boolean@TestAnnotation) = ???

  def funiArgAt(a: => Int@TestAnnotation): Unit = ???


  implicit def toBoolean(i: Int): Boolean = if (i == 0) false else true

  def compare(code1: String)(code2: String) {
    val c1 = replaceFreshVariables(code1)
    val c2 = replaceFreshVariables(code2)
    assert(c1 == c2)
  }

  test("simple cases") {
    compare(show(KNFTransform.knfTransform {
      println(hoge(10))
    }))( """(scala.this.Predef.println(KNFTransformerTest.this.hoge(10)): Unit)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      println("hoge" + 1)
    }))( """(scala.this.Predef.println("hoge".+(1)): Unit)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      println(Math.sqrt(10))
    }))( """(scala.this.Predef.println(java.this.lang.Math.sqrt(10.0)): Unit)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      Array(10)(0)
    }))( """(scala.Array.apply(10, <empty>).apply(0): Int)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      piyo(10)(20)
    }))( """(KNFTransformerTest.this.piyo(10)(20): Unit)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      println(Math.sqrt(10))
      println("hoge" + 1)
    }))( """({
           |  scala.this.Predef.println(java.this.lang.Math.sqrt(10.0));
           |  scala.this.Predef.println("hoge".+(1))
           |}: Unit)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      (bo, in) match {
        case (a, b) => false;
        case _ => true
      }
    }))( """(scala.Tuple2.apply[Boolean, Int](KNFTransformerTest.this.bo, KNFTransformerTest.this.in) match {
           |  case (_1: Boolean, _2: Int)(Boolean, Int)((a @ _), (b @ _)) => false
           |  case _ => true
           |}: Any)""".stripMargin)
  }


  test("@ annotation @") {
    compare(show(KNFTransform.knfTransform {
      println(hogeAt(10))
    }))( """({
           |  <synthetic> <artifact> val $$: Unit @KNFTransformerTest.this.TestAnnotation = KNFTransformerTest.this.hogeAt(10);
           |  scala.this.Predef.println($$)
           |}: Unit)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      println(hogeAt(10): @TestAnnotation)
    }))( """({
           |  <synthetic> <artifact> val $$: Unit @KNFTransformerTest.this.TestAnnotation = KNFTransformerTest.this.hogeAt(10);
           |  <synthetic> <artifact> val $$: Unit @KNFTransformerTest.this.TestAnnotation @KNFTransformerTest.this.TestAnnotation = ($$: Unit @KNFTransformerTest.this.TestAnnotation @KNFTransformerTest.this.TestAnnotation);
           |  scala.this.Predef.println($$)
           |}: Unit)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      println(("hoge": String@TestAnnotation) + 1)
    }))( """({
           |  <synthetic> <artifact> val $$: String @KNFTransformerTest.this.TestAnnotation = ("hoge": String @KNFTransformerTest.this.TestAnnotation);
           |  <synthetic> <artifact> val $$: String = $$.+(1);
           |  scala.this.Predef.println($$)
           |}: Unit)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      println(Math.sqrt(10): @TestAnnotation)
    }))( """({
           |  <synthetic> <artifact> val $$: Double @KNFTransformerTest.this.TestAnnotation = (java.this.lang.Math.sqrt(10.0): Double @KNFTransformerTest.this.TestAnnotation);
           |  scala.this.Predef.println($$)
           |}: Unit)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      val p = piyoAt(10)(20)
    }))( """({
           |  val p: Unit @KNFTransformerTest.this.TestAnnotation = KNFTransformerTest.this.piyoAt(10)(20);
           |  ()
           |}: Unit)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      {
        piyoAt(10)(20)
      }
      val b = {
        piyoAt(10)(20)
        piyo(10)(20)
        piyoAt(10)(20)
        piyo(10)(20)
        piyoAt(10)(20)
      }
      piyoAt(10)(20)
    }))( """({
           |  KNFTransformerTest.this.piyoAt(10)(20);
           |  val b: Unit @KNFTransformerTest.this.TestAnnotation = {
           |    KNFTransformerTest.this.piyoAt(10)(20);
           |    KNFTransformerTest.this.piyo(10)(20);
           |    KNFTransformerTest.this.piyoAt(10)(20);
           |    KNFTransformerTest.this.piyo(10)(20);
           |    KNFTransformerTest.this.piyoAt(10)(20)
           |  };
           |  KNFTransformerTest.this.piyoAt(10)(20)
           |}: Unit)""".stripMargin)


    compare(show(KNFTransform.knfTransform {
      (boAt, inAt) match {
        case (a, b) => false;
        case _ => true
      }
    }))( """(scala.Tuple2.apply[Boolean, Int](KNFTransformerTest.this.boAt, KNFTransformerTest.this.inAt) match {
           |  case (_1: Boolean, _2: Int)(Boolean, Int)((a @ _), (b @ _)) => false
           |  case _ => true
           |}: Any)""".stripMargin)
  }

  // no expasion
  test("by-names") {
    compare(show(KNFTransform.knfTransform {
      funi(1 + in)
    }))("(KNFTransformerTest.this.funi(1.+(KNFTransformerTest.this.in)): Unit)")

    // short circuit operator
    compare(show(KNFTransform.knfTransform {
      bo && (bo && bo)
    }))("(KNFTransformerTest.this.bo.&&(KNFTransformerTest.this.bo.&&(KNFTransformerTest.this.bo)): Boolean)")
  }

  // no expasion
  test("@ by-names @") {
    compare(show(KNFTransform.knfTransform {
      funiAt(1 + in)
    }))("(KNFTransformerTest.this.funiAt(1.+(KNFTransformerTest.this.in)): Unit)")

    // short circuit operator
    compare(show(KNFTransform.knfTransform {
      boAt && (boAt && boAt)
    }))( """({
           |  <synthetic> <artifact> val $$: Boolean @KNFTransformerTest.this.TestAnnotation = KNFTransformerTest.this.boAt;
           |  $$.&&(KNFTransformerTest.this.boAt.&&(KNFTransformerTest.this.boAt))
           |}: Boolean)""".stripMargin)
  }

  // expand
  test("not by-name") {
    // not short circuit operator
    compare(show(KNFTransform.knfTransform {
      &&(in + 1)
    }))( """(KNFTransformerTest.this.&&(KNFTransformerTest.this.in.+(1)): Boolean)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      if (&&(in + 1)) 10 else 20
    }))( """(if (KNFTransformerTest.this.&&(KNFTransformerTest.this.in.+(1)))
           |  10
           |else
           |  20: Any)""".stripMargin)

    // prefix is not short circuit
    compare(show(KNFTransform.knfTransform {
      bo && bo && bo
    }))( """(KNFTransformerTest.this.bo.&&(KNFTransformerTest.this.bo).&&(KNFTransformerTest.this.bo): Boolean)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      if (bo && bo && bo) 10 else 20
    }))( """(if (KNFTransformerTest.this.bo.&&(KNFTransformerTest.this.bo).&&(KNFTransformerTest.this.bo))
           |  10
           |else
           |  20: Any)""".stripMargin)
  }


  // expand
  test("@ not by-name @") {
    // not short circuit operator
    compare(show(KNFTransform.knfTransform {
      &&(inAt + 1)
    }))( """({
           |  <synthetic> <artifact> val $$: Int @KNFTransformerTest.this.TestAnnotation = KNFTransformerTest.this.inAt;
           |  <synthetic> <artifact> val $$: Int = $$.+(1);
           |  KNFTransformerTest.this.&&($$)
           |}: Boolean)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      if (&&(inAt + 1)) 10 else 20
    }))( """({
           |  <synthetic> <artifact> val $$: Int @KNFTransformerTest.this.TestAnnotation = KNFTransformerTest.this.inAt;
           |  <synthetic> <artifact> val $$: Int = $$.+(1);
           |  <synthetic> <artifact> val $$: Boolean = KNFTransformerTest.this.&&($$);
           |  if ($$)
           |    10
           |  else
           |    20
           |}: Any)""".stripMargin)

    // prefix is not short circuit
    compare(show(KNFTransform.knfTransform {
      boAt && boAt && boAt
    }))( """({
           |  <synthetic> <artifact> val $$: Boolean @KNFTransformerTest.this.TestAnnotation = KNFTransformerTest.this.boAt;
           |  <synthetic> <artifact> val $$: Boolean = $$.&&(KNFTransformerTest.this.boAt);
           |  $$.&&(KNFTransformerTest.this.boAt)
           |}: Boolean)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      if (boAt && boAt && boAt) 10 else 20
    }))( """({
           |  <synthetic> <artifact> val $$: Boolean @KNFTransformerTest.this.TestAnnotation = KNFTransformerTest.this.boAt;
           |  <synthetic> <artifact> val $$: Boolean = $$.&&(KNFTransformerTest.this.boAt);
           |  <synthetic> <artifact> val $$: Boolean = $$.&&(KNFTransformerTest.this.boAt);
           |  if ($$)
           |    10
           |  else
           |    20
           |}: Any)""".stripMargin)
  }

  test("complex pattern") {
    val a = new KNFTransformerTest
    compare(show(KNFTransform.knfTransform {
      a.hoge(10)
    }))("(a.hoge(10): Unit)")

    compare(show(KNFTransform.knfTransform {
      fuga(10)(b = 20)
    }))( """({
           |  <artifact> val x$1: Int = 10;
           |  <artifact> val x$2: Int = 20;
           |  <artifact> val x$3: Int = KNFTransformerTest.this.fuga$default$3(x$1);
           |  KNFTransformerTest.this.fuga(x$1)(x$2, x$3)
           |}: Unit)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      fuga(10)(20, c = 30)
    }))("(KNFTransformerTest.this.fuga(10)(20, 30): Unit)")

    compare(show(KNFTransform.knfTransform {
      val a: String = "piyo"
      println(a)
    }))( """({
           |  val a: String = "piyo";
           |  scala.this.Predef.println(a)
           |}: Unit)""".stripMargin)
  }

  test("@ complex pattern @") {
    val a = new KNFTransformerTest
    compare(show(KNFTransform.knfTransform {
      a.hogeAt(10)
    }))("(a.hogeAt(10): Unit)")

    compare(show(KNFTransform.knfTransform {
      fugaAt(10)(b = 20)
    }))( """({
           |  <artifact> val x$4: Int = 10;
           |  <artifact> val x$5: Int = 20;
           |  <artifact> val x$6: Int = KNFTransformerTest.this.fugaAt$default$3(x$4);
           |  KNFTransformerTest.this.fugaAt(x$4)(x$5, x$6)
           |}: Unit)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      fugaAt(10)(20, c = 30)
    }))("(KNFTransformerTest.this.fugaAt(10)(20, 30): Unit)")

    compare(show(KNFTransform.knfTransform {
      val a: String@TestAnnotation = "piyo"
      println(a)
    }))( """({
           |  val a: String @KNFTransformerTest.this.TestAnnotation = "piyo";
           |  scala.this.Predef.println(a)
           |}: Unit)""".stripMargin)
  }

  test("type apply") {

    compare(show(KNFTransform.knfTransform {
      None.asInstanceOf[Option[Int]]
    }))("(scala.None.asInstanceOf[Option[Int]]: Option[Int])")

    compare(show(KNFTransform.knfTransform {
      None.isInstanceOf[Option[Int]]
    }))("(scala.None.isInstanceOf[Option[Int]]: Boolean)")
  }

  test("@ type apply @") {

    compare(show(KNFTransform.knfTransform {
      None.asInstanceOf[Option[Int]@TestAnnotation]
    }))("(scala.None.asInstanceOf[Option[Int] @KNFTransformerTest.this.TestAnnotation]: Option[Int])")

    compare(show(KNFTransform.knfTransform {
      None.isInstanceOf[Option[Int]@TestAnnotation]
    }))("(scala.None.isInstanceOf[Option[Int] @KNFTransformerTest.this.TestAnnotation]: Boolean)")

    compare(show(KNFTransform.knfTransform {
      println(None.asInstanceOf[Option[Int]@TestAnnotation])
    }))( """({
           |  <synthetic> <artifact> val $$: Option[Int] @KNFTransformerTest.this.TestAnnotation = scala.None.asInstanceOf[Option[Int] @KNFTransformerTest.this.TestAnnotation];
           |  scala.this.Predef.println($$)
           |}: Unit)""".stripMargin)
  }

  test("if / match / try") {
    compare(show(KNFTransform.knfTransform {
      if (bo) 1 else 2
    }))( """(if (KNFTransformerTest.this.bo)
           |  1
           |else
           |  2: Any)""".stripMargin)

    // empty guards
    compare(show(KNFTransform.knfTransform {
      bo match {
        case _ => 1
      }
    }))( """(KNFTransformerTest.this.bo match {
           |  case _ => 1
           |}: Any)""".stripMargin)

    // empty catch
    compare(show(KNFTransform.knfTransform {
      try {
        bo
      }
    }))( """(try {
           |  KNFTransformerTest.this.bo
           |}: Any)""".stripMargin)
  }

  test("@ if / match / try @") {
    compare(show(KNFTransform.knfTransform {
      if (boAt) 1 else 2
    }))( """({
           |  <synthetic> <artifact> val $$: Boolean @KNFTransformerTest.this.TestAnnotation = KNFTransformerTest.this.boAt;
           |  if ($$)
           |    1
           |  else
           |    2
           |}: Any)""".stripMargin)

    // empty guards
    compare(show(KNFTransform.knfTransform {
      boAt match {
        case _ => 1
      }
    }))( """({
           |  <synthetic> <artifact> val $$: Boolean @KNFTransformerTest.this.TestAnnotation = KNFTransformerTest.this.boAt;
           |  $$ match {
           |    case _ => 1
           |  }
           |}: Any)""".stripMargin)

    // empty catch
    compare(show(KNFTransform.knfTransform {
      try {
        boAt
      }
    }))( """((try {
           |  KNFTransformerTest.this.boAt
           |}: Any @KNFTransformerTest.this.TestAnnotation): Any)""".stripMargin)
  }

  test("complex cases 2") {
    compare(show(KNFTransform.knfTransform {
      val a = {
        val a = 10
        a + 20
      } + 30
      println(a)
      a
    }))( """({
           |  val a: Int = {
           |  val a: Int = 10;
           |  a.+(20)
           |}.+(30);
           |  scala.this.Predef.println(a);
           |  a
           |}: Int)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      val a = 5
      val b = {
        val a = 10
        a + 20
      } + 30
      println(a)
      a
    }))( """({
           |  val a: Int = 5;
           |  val b: Int = {
           |  val a: Int = 10;
           |  a.+(20)
           |}.+(30);
           |  scala.this.Predef.println(a);
           |  a
           |}: Int)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      (a: Int) => {
        a
      }
    }))( """(((a: Int) => a): Int => Int)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
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
    compare(show(KNFTransform.knfTransform {
      Promise.successful(if (bo) 10 else 20).trySuccess(10)
    }))( """(scala.concurrent.Promise.successful[Int](if (KNFTransformerTest.this.bo)
           |  10
           |else
           |  20).trySuccess(10): Boolean)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      Promise.successful(if (bo) 10 else 20)
    }))( """(scala.concurrent.Promise.successful[Int](if (KNFTransformerTest.this.bo)
           |  10
           |else
           |  20): scala.concurrent.Promise[Int])""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      println(if (bo) 10 else 20)
    }))( """(scala.this.Predef.println(if (KNFTransformerTest.this.bo)
           |  10
           |else
           |  20): Unit)""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      Promise.successful(if (bo) 10 else 20).future
    }))( """(scala.concurrent.Promise.successful[Int](if (KNFTransformerTest.this.bo)
           |  10
           |else
           |  20).future: scala.concurrent.Future[Int])""".stripMargin)

    compare(show(KNFTransform.knfTransform {
      val a = scala.Predef
      Promise.successful(if (bo) 10 else 20).future.failed
    }))( """({
           |  val a: scala.Predef.type = scala.Predef;
           |  scala.concurrent.Promise.successful[Int](if (KNFTransformerTest.this.bo)
           |  10
           |else
           |  20).future.failed
           |}: scala.concurrent.Future[Throwable])""".stripMargin)
  }

  test("@ by name @") {
    println(show(KNFTransform.knfTransform (hoge(1:Int@TestAnnotation))))
    println(show(KNFTransform.knfTransform (funi(1:Int@TestAnnotation))))
    println(show(KNFTransform.knfTransform (funiAt(1:Int@TestAnnotation))))
    println(show(KNFTransform.knfTransform (funiArgAt(1))))
  }
}
