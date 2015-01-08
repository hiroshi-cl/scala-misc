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

import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi._
import macros.base.EnhancedMacroBundleBase
import util.infixify._

import scala.reflect.macros.blackbox.Context

class CPSBundle(override val c: Context) extends EnhancedMacroBundleBase(c)
with KNFTransformer with MarkerInserter with Lifter {

  import c.universe._

  val reflect = typeOf[Reflect.type].member(TermName("reflect2direct")).asMethod

  def applyImpl[ReturnType: c.WeakTypeTag](a: Tree) = {
    val r = defineVal(c.prefix.tree)
    smartReflect(r, q"${transform(r, a)}" *^ typecheck)
  }

  def applyRelax[ReturnType: c.WeakTypeTag](a: Tree) = {
    val r = defineVal(c.prefix.tree)
    smartReflect(r, q"${makeRef(r)}.bind(${makeRef(r)}.unit(), (_: Unit) => ${transform(r, a)})" *^ typecheck)
  }

  def transform(r: ValDef, a: Tree) = {
    if (a.tpe.typeConstructor =:= typeOf[Reflect[_, _]].typeConstructor)
      error(a)("it is now allowed that the return type of the thunk is Reflect (return types must be direct types)")

    val knf = knfTransform(a)
    val move = insertByValue(knf)(reflect)

    val extractor = (ref: Tree) => q"$ref.container" *^ typecheck
    val unit = (value: Tree) => q"${makeRef(r)}.unit($value)" *^ typecheck
    val bind = (ref: Tree, f: Tree) => q"${makeRef(r)}.bind($ref, $f)" *^ typecheck
    val fmap = (ref: Tree, f: Tree) => q"${makeRef(r)}.fmap($ref, $f)" *^ typecheck
    lifter(move)(reflect, extractor, unit, bind, fmap) *^ typecheck
  }

  def smartReflect[ReturnType: c.WeakTypeTag](r: ValDef, ret: Tree) =
    if (weakTypeOf[ReturnType].typeConstructor == typeOf[Reflect[_, _]].typeConstructor)
      q"$r; ${makeRef(r)}.reflect($ret)" *^ typecheck *^ refresh
    else
      q"$r; $ret" *^ typecheck *^ refresh
}