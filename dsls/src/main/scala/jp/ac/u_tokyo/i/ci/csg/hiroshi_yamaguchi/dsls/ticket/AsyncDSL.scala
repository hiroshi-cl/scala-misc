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

import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.byname.ByNameProxy
import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.ticket._
import Ticket._
import TicketBuilder._

object AsyncDSL {

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent._
  import scala.concurrent.duration._

  @inline
  def async[T](thunk: => T) = tearOff(
    (k: Future[T] => Future[T]) => k(Future(thunk)),
    (x: Future[T]) => (k: T => Future[T]) => x.flatMap(k)
  )

  @inline
  def await[T](f: Future[T]) = tearOff(
    (k: Future[T] => Future[T]) => k(f),
    (x: Future[T]) => (k: T => Future[T]) => x.flatMap(k)
  )

  val unit = Promise.successful(()).future

  @inline
  def dsl[T](thunk: => T)(implicit
                          f: ByNameProxy[TicketBuilder[Future[T]], Unit => Ticket[T, Future[T]]]): Future[T] =
    unit.flatMap((_: Unit) => reset(Promise.successful[T](f.a()).future))

}
