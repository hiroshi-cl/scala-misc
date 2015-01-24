package jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.util.java8adapter

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

object Java8Adapter {
  import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.base.EnhancedMacroBundleBase

import scala.language.experimental.macros
  import scala.reflect.macros.blackbox.Context

  class Bundle(override val c: Context) extends EnhancedMacroBundleBase(c) {

    import c.universe._

    def impl[T: WeakTypeTag](fun: Tree) = {
      val tag = implicitly[WeakTypeTag[T]]
      val methods = tag.tpe.members.filter(s => s.isAbstract && s.isMethod).map(_.asMethod)
      methods match {
        case List(unique) =>
          fun match {
            case q"(..$params) => $body" =>
              debug(refresh(q"new $tag { override def ${unique.name} (..$params) = $body }"))("take params")
            case q"$body" =>
              debug(refresh(q"new $tag { override def ${unique.name} () = $body }"))("take no param")
          }
        case List() => error(tq"$tag")("no method found")
        case _ => error(tq"$tag")("multiple methods detected:\n" + methods)
      }
    }

  }

  def j8fi[T](fun: Any): T = macro Bundle.impl[T]
}
