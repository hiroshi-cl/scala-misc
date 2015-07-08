package jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.ticket

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

import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}
import scala.reflect.macros.blackbox.Context

sealed trait GTicket[A, R] extends Any {

  // TODO: other methods for Any/AnyRef
  // required: because Object + String precedes implicit conversion
  def +(a: A): A = macro GTicket.Bundle.dummy_+[A, R]

  def ==(a: A): Boolean = macro GTicket.Bundle.dummy_==[A, R]

  override def toString: String = macro GTicket.Bundle.dummy_toString[A, R]

}

object GTicket {

  // for coercion
  private class Bundle(val c: Context) {

    import c.universe._

    def coerce_prefix[A: c.WeakTypeTag, R: c.WeakTypeTag] = {
      val typeCA = weakTypeOf[R]
      val typeA = weakTypeOf[A]
      val typeR = c.typecheck(tq"Ticket[$typeA, $typeCA]", mode = c.TYPEmode).tpe
      c.typecheck(q"${c.inferImplicitView(c.prefix.tree, typeR, typeA, silent = false)}(${c.prefix.tree})")
    }

    def dummy_+[A: c.WeakTypeTag, R: c.WeakTypeTag](a: Tree) = c.typecheck(q"${coerce_prefix[A, R]} + $a")

    def dummy_==[A: c.WeakTypeTag, R: c.WeakTypeTag](a: Tree) = c.typecheck(q"${coerce_prefix[A, R]} == $a")

    def dummy_toString[A: c.WeakTypeTag, R: c.WeakTypeTag] = c.typecheck(q"${coerce_prefix[A, R]}.toString")
  }
}

case class Stub[A, R](val ctl: (A => R) => R) extends AnyVal with GTicket[A, R] {
  @inline
  def map[B](f: A => B) = Ticket[B, R](
    k => ctl(x => k(Stub(k1 => k1(f(x))))),
    x => k => x.ctl(k)
  )

  @inline
  def flatMap[B, CB](f: A => GTicket[B, R]) = Ticket[B, R](
    k => ctl(x => f(x) match {
      case t: Stub[B, R] =>
        k(t)
      case t: Ticket[B, R] =>
        t.eager_ctl(y => k(Stub(t.lazy_ctl(y))))
    }),
    x => k => x.ctl(k)
  )

  @inline
  def run(implicit dep: A =:= R): R = ctl(dep(_))
}

case class Ticket[A, R](val eager_ctl: (Stub[A, R] => R) => R, val lazy_ctl: Stub[A, R] => (A => R) => R)
  extends GTicket[A, R] {

  @inline
  def map[B](f: Stub[A, R] => B) = Ticket[B, R](
    k => eager_ctl(x => k(Stub(k1 => k1(f(Stub(lazy_ctl(x))))))),
    x => k => x.ctl(k)
  )

  @inline
  def flatMap[B](f: Stub[A, R] => GTicket[B, R]) = Ticket[B, R](
    k => eager_ctl(x => f(Stub(lazy_ctl(x))) match {
      case t: Stub[B, R] =>
        k(t)
      case t: Ticket[B, R] =>
        t.eager_ctl(y => k(Stub(t.lazy_ctl(y))))
    }),
    x => k => x.ctl(k)
  )

  @inline
  def run(implicit dep: A =:= R): R = eager_ctl(x => Stub(lazy_ctl(x)).run)
}
