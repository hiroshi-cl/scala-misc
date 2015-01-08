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

import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.base.EnhancedMacroBundleBase
import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.cps._
import language.experimental.macros
import language.higherKinds

object Sample {

  import scala.reflect.macros.blackbox.Context

  private[this] class Bundle(override val c: Context) extends EnhancedMacroBundleBase(c)
  with KNFTransformer with MarkerInserter with Lifter {

    import c.universe._

    val r = typeOf[Sample.type].member(TermName("dummy")).asMethod

    def knfTransformSmpl(t: Tree) = refresh(knfTransform(t))

    def insBVSmpl(t: Tree) = refresh(insertByValue(t)(r))

    def lifterSmpl(t: Tree) = refresh(lifter(t)(r, identity,
      t => q"List($t)", (s, t) => q"$s.flatMap($t)", (s, t) => q"$s.map($t)"))

  }

  def knfTransform[R](t: R): R = macro Bundle.knfTransformSmpl

  def insBV[R](t: R): R = macro Bundle.insBVSmpl

  def lifter[R](t: R): List[R] = macro Bundle.lifterSmpl

  def dummy[A](hoge: List[A]): A = ???
}
