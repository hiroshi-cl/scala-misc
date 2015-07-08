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

import language.implicitConversions
import language.experimental.macros
import scala.annotation.compileTimeOnly

trait TicketBuilder[R] extends Any {
  @inline
  def unit[A](a: A) = Ticket[A, R](
    k => k(Stub(k1 => k1(a))),
    x => k => x.ctl(k)
  )

  @inline
  def apply[A](a: A): Ticket[A, R] = macro TicketBuilder.Bundle.applyImpl
}

object TicketBuilder {

  import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.byname.{ByNameProxy, ByNameBundle}
  import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.base.EnhancedMacroBundleBase
  import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.util.infixify._

  import scala.reflect.macros.blackbox.Context

  class Bundle(override val c: Context) extends EnhancedMacroBundleBase(c)
  with KNFTransformer with MarkerInserter with Lifter {

    import c.universe._

    val byName = typeOf[TicketBuilder.type].member(TermName("byNameMarker")).asMethod
    val byValue = typeOf[TicketBuilder.type].member(TermName("byValueMarker")).asMethod
    val byNeed = typeOf[TicketBuilder.type].member(TermName("byNeedMarker")).asMethod

    def applyImpl(a: Tree) = {
      if (a.tpe.typeConstructor weak_<:< typeOf[GTicket[_, _]].typeConstructor)
        error(a)("it is now allowed that the return type of the thunk is GTicket (return types must be direct types)")
      if (a.tpe.takesTypeArgs)
        for (tpe <- a.tpe.typeArgs)
          if (tpe.typeConstructor weak_<:< typeOf[GTicket[_, _]].typeConstructor)
            error(a)("it is now allowed that the return type of the thunk is GTicket (return types must be direct types)")

      val knf = knfTransform(a)
      val move = moveMarkers(knf)(byName, byValue, byNeed)

      val r = defineVal(c.prefix.tree)
      val unit = (value: Tree) => q"${makeRef(r)}.unit($value)" *^ typecheck
      val bind = (ref: Tree, f: Tree) => q"$ref.flatMap($f)" *^ typecheck
      val fmap = (ref: Tree, f: Tree) => q"$ref.map($f)" *^ typecheck
      lifter(q"$r; $move" *^ typecheck)(byValue, byNeed, unit, bind, fmap) *^ refresh
    }


    def resetImpl[ReturnType: c.WeakTypeTag](a: Tree) = {
      if (a.tpe.typeConstructor weak_<:< typeOf[GTicket[_, _]].typeConstructor)
        error(a)("it is now allowed that the return type of the thunk is GTicket (return types must be direct types)")
      if (a.tpe.takesTypeArgs)
        for (tpe <- a.tpe.typeArgs)
          if (tpe.typeConstructor weak_<:< typeOf[GTicket[_, _]].typeConstructor)
            error(a)("it is now allowed that the return type of the thunk is GTicket (return types must be direct types)")

      val knf = knfTransform(a)
      val move = moveMarkers(knf)(byName, byValue, byNeed)

      val tpe = c.typecheck(tq"TicketBuilder[${weakTypeOf[ReturnType]}]", mode = c.TYPEmode).tpe
      val r = defineVal(c.inferImplicitValue(tpe))
      val unit = (value: Tree) => q"${makeRef(r)}.unit($value)" *^ typecheck
      val bind = (ref: Tree, f: Tree) => q"$ref.flatMap($f)" *^ typecheck
      val fmap = (ref: Tree, f: Tree) => q"$ref.map($f)" *^ typecheck
      val hoge = lifter(q"$r; $move" *^ typecheck)(byValue, byNeed, unit, bind, fmap) *^ refresh
      val targs = hoge.tpe.typeArgs
      q"$hoge.run(${c.inferImplicitValue(c.typecheck(tq"${targs(0)} =:= ${targs(1)}", mode = c.TYPEmode).tpe)})" *^ typecheck
    }
  }


  @inline
  def tearOff[A, CA, R](eager_ctl: (CA => R) => R, lazy_ctl: CA => (A => R) => R): Ticket[A, R] =
    Ticket[A, R](
      k => eager_ctl(x => k(Stub(lazy_ctl(x)))),
      x => k => x.ctl(k)
    )

  //  @inline
  def reset[R](a: R): R = macro Bundle.resetImpl[R]

  implicit def byNameMarker[A](a: Ticket[A, _]): A = ???

  @compileTimeOnly("Macro System Bug: exists only markerInserter -> lifter")
  def byValueMarker[A, R](a: Ticket[A, R]): Stub[A, R] = ???

  @compileTimeOnly("Macro System Bug: exists only markerInserter -> lifter")
  def byNeedMarker[A, _](a: Stub[A, _]): A = ???

  @inline
  implicit def ticketBuilder[R] = new TicketBuilder[R] {}

  @inline
  implicit def ticketProxy[A, R]: ByNameProxy[TicketBuilder[R], Unit => Ticket[A, R]] = macro ByNameBundle.byNameProxy

}
