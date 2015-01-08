package jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.cps

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


class Reflect[+A, +CA](val container: CA) {
  type C[+X]

  // TODO: other methods for Any/AnyRef
  // required: because Object + String precedes implicit conversion
  def +(a: A): A = macro Reflect.Bundle.dummy_+[A, CA]

  def ==(a: A): Boolean = macro Reflect.Bundle.dummy_==[A, CA]

  override def toString: String = macro Reflect.Bundle.dummy_toString[A, CA]

}

object Reflect {

  // for coercion
  private class Bundle(val c: Context) {

    import c.universe._

    def coerce_prefix[A: c.WeakTypeTag, CA: c.WeakTypeTag] = {
      val typeCA = weakTypeOf[CA]
      val typeA = weakTypeOf[A]
      val typeR = c.typecheck(tq"Reflect[$typeA, $typeCA]", mode = c.TYPEmode).tpe
      c.typecheck(q"${c.inferImplicitView(c.prefix.tree, typeR, typeA, silent = false)}(${c.prefix.tree})")
    }

    def dummy_+[A: c.WeakTypeTag, CA: c.WeakTypeTag](a: Tree) = c.typecheck(q"${coerce_prefix[A, CA]} + $a")

    def dummy_==[A: c.WeakTypeTag, CA: c.WeakTypeTag](a: Tree) = c.typecheck(q"${coerce_prefix[A, CA]} == $a")

    def dummy_toString[A: c.WeakTypeTag, CA: c.WeakTypeTag] = c.typecheck(q"${coerce_prefix[A, CA]}.toString")
  }

  implicit def reflect2direct[A](r: Reflect[A, _]): A = ???
}