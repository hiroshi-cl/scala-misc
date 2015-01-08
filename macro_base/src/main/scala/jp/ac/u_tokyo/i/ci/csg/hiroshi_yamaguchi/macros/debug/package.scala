package jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros

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

import language.experimental.macros
import base.EnhancedMacroBundleBase
import scala.reflect.macros.blackbox.Context

package object debug {

  private[this] class Bundle(override val c: Context) extends EnhancedMacroBundleBase(c) {

    import c.universe._

    def appliedImpl(prefix: Tree)(name: Tree)(a: Tree*) =
      catchError(macroApply(prefix)(name)(a: _*))

    def showImpl(t: Tree) = q"${c.universe.show(t)}"

    def parseImpl(code: Tree) = {
      val Literal(Constant(s: String)) = code
      catchError(c.parse(s))
    }

    def abortImpl(t: Tree) = c.abort(c.enclosingPosition, "Abort")
  }

  def applied(prefix: Any)(name: String)(a: Any*): Any = macro Bundle.appliedImpl

  def show(t: Any): String = macro Bundle.showImpl

  def parse(code: String): Any = macro Bundle.parseImpl

  def abort(t: Any): Nothing = macro Bundle.abortImpl

  private[this] val pat = java.util.regex.Pattern.compile(java.util.regex.Pattern.quote("fresh$macro$") + "\\d+")

  def replaceFreshVariables(code: String): String = pat.matcher(code).replaceAll("\\$\\$")


  def expectException(a: => Any): Boolean = try {
    a
    false
  } catch {
    case e: Exception =>
      true
  }

  object Abort {
    def abort(t: Any): Nothing = macro Bundle.abortImpl
  }
}
