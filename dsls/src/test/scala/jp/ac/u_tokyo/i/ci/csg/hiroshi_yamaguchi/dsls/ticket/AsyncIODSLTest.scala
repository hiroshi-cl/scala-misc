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
import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.debug._
import org.scalatest.FunSuite
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.SpanSugar._

class AsyncIODSLTest extends FunSuite with TimeLimitedTests {

  val timeLimit = 50000 millis

  import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.dsls.ticket.AsyncIODSL._

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent._
  import scala.concurrent.duration._

  test("dsl") {
    assert(Await.result(dsl {
      10
    }, Duration.Inf) == 10)
  }
  test("using 1") {
    assert(Await.result(dsl {
      val s = using(scala.io.Source.fromFile("LICENSE.md"))
      s.getLines.next
    }, Duration.Inf).startsWith("Copyright"))
  }
  test("using 2") {
    assert(Await.result(dsl {
      val s = using(scala.io.Source.fromFile("LICENSE.md"))
      println("@@@@")
      s.getLines.next
    }, Duration.Inf).startsWith("Copyright"))
  }
  test("code") {
    def compare(code1: String)(code2: String) {
      val c1 = replaceFreshVariables(code1)
      val c2 = replaceFreshVariables(code2)
      assert(c1 == c2)
    }
  }

  def compare(code1: String)(code2: String) {
    val c1 = replaceFreshVariables(code1)
    val c2 = replaceFreshVariables(code2)
    assert(c1 == c2)
  }

  test("simple cases") {
    compare(show(dsl {
      val s = using(scala.io.Source.fromFile(""))
      println("@@@@")
      s.getLines.next
    }))( """jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.dsls.ticket.AsyncIODSL.dsl[String]({
           |  val s: jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.ticket.Ticket[scala.io.BufferedSource,scala.concurrent.Future[Any]] = jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.dsls.ticket.AsyncIODSL.using[scala.io.BufferedSource](scala.io.Source.fromFile("")(io.this.Codec.fallbackSystemCodec));
           |  scala.this.Predef.println("@@@@");
           |  jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.ticket.TicketBuilder.byNameMarker[scala.io.BufferedSource](s).getLines().next()
           |})(ByNameProxy.apply[jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.ticket.TicketBuilder[scala.concurrent.Future[Any]], Unit => jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.ticket.Ticket[String,scala.concurrent.Future[Any]]](((fresh$macro$95: Unit) => ({
           |  <synthetic> <artifact> val fresh$macro$89: jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.ticket.TicketBuilder[scala.concurrent.Future[Any]] = jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.ticket.TicketBuilder.ticketBuilder[scala.concurrent.Future[Any]];
           |  <synthetic> <artifact> val fresh$macro$94: jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.ticket.Ticket[String,scala.concurrent.Future[Any]] = {
           |    <synthetic> <artifact> val fresh$macro$90: jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.ticket.Ticket[scala.io.BufferedSource,scala.concurrent.Future[Any]] = jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.dsls.ticket.AsyncIODSL.using[scala.io.BufferedSource](scala.io.Source.fromFile("")(io.this.Codec.fallbackSystemCodec));
           |    <synthetic> <artifact> val fresh$macro$93: jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.ticket.Stub[scala.io.BufferedSource,scala.concurrent.Future[Any]] => jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.ticket.Ticket[String,scala.concurrent.Future[Any]] = ((s: jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.ticket.Stub[scala.io.BufferedSource,scala.concurrent.Future[Any]]) => {
           |      <synthetic> <artifact> val fresh$macro$86: Unit = scala.this.Predef.println("@@@@");
           |      <synthetic> <artifact> val fresh$macro$91: jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.ticket.Stub[scala.io.BufferedSource,scala.concurrent.Future[Any]] = s;
           |      <synthetic> <artifact> val fresh$macro$92: scala.io.BufferedSource => String = ((fresh$macro$87: scala.io.BufferedSource) => {
           |        <synthetic> <artifact> val fresh$macro$88: Iterator[String] = fresh$macro$87.getLines();
           |        fresh$macro$88.next()
           |      });
           |      fresh$macro$91.map[String](fresh$macro$92)
           |    });
           |    fresh$macro$90.flatMap[String](fresh$macro$93)
           |  };
           |  fresh$macro$94
           |}: jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.ticket.Ticket[String,scala.concurrent.Future[Any]]))))""".stripMargin)
  }

}
