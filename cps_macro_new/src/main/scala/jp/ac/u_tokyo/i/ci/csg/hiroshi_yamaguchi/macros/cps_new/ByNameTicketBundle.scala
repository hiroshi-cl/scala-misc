package jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.cps_new

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

import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.base._
import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.util.infixify._

import scala.reflect.macros.whitebox

class ByNameTicketBundle(override val c: whitebox.Context) extends EnhancedMacroBundleBase(c) {

  import c.universe._
  import ByNameTicketBundle._

  def byName = convertError {
    val list = c.enclosingImplicits
    if (!list.isEmpty)
      getByNames(list.last.tree) match {
        case List(byNameArg) =>
          val ticket = typeOf[Ticket.type].termSymbol
          val proxy = typeOf[TicketProxy.type].termSymbol
          q"$proxy(() => $ticket.ticket{$byNameArg})" *^ refresh
        case argList => multipleByNameParameterError(list.last.tree, argList)
      }
    else outOfImplicitsError
  }

  private[this] def convertError(run: => Tree): Tree =
    try {
      run
    } catch {
      case e =>
        e.printStackTrace()
        q"{@scala.annotation.compileTimeOnly(${e.getMessage}) def error: Nothing = ???; error}"
    }


  private[this] def getByNames(tree: Tree): List[Tree] = {
    tree match {
      case FineGrainedApply(_, _, _, list) =>
        for ((arg, isByName) <- list.flatten if isByName) yield {
          for (i@Ident(name) <- arg if i.symbol.isTerm && i.symbol.asTerm.isByNameParam)
            error(tree)(
              "bad news: ordinal macros cannot collaborate with the by-name variable "
                + name + " directly"
            )

          arg
        }

      case _ =>
        error(tree)("given tree must be instance of Apply")
    }
  }

  private[this] def outOfImplicitsError = error(EmptyTree)("this macro cannot use out of implicit parameters")

  private[this] def multipleByNameParameterError(tree: Tree, list: List[Tree]) =
    error(q"{$tree; {..$list}}")("by name parameter must be just one")
}

object ByNameTicketBundle {

  case class TicketProxy[Transformed](val a: () => Transformed) extends AnyVal

}
