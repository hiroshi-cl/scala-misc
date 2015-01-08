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

import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.base.EnhancedMacroBundleBase
import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.util.infixify._

trait MarkerInserter {
  self: EnhancedMacroBundleBase =>
  import c.universe._
  import c.internal._
  import decorators._

  def insertByValue(tree: Tree)(reflect: MethodSymbol): Tree =
    insertBVImpl(List(tree) *^ toBlock *^ removeRedundantValDefs)(reflect)

  private[this] def insertBVImpl(tree: Tree)(reflect: MethodSymbol): Tree = {
    val fromType = reflect.paramLists.head.head.info.erasure
    var set = Set[Symbol]()
    transform(tree) {
      (t, api) =>
        t match {
          case Apply(tpt, args) if (t.symbol == reflect && set.contains(args.head.symbol)) =>
            args.head

          case ValDef(mods, name, tpt, rhs) =>
            if (tpt.tpe.erasure == fromType) {
              val newType = tpt.tpe.typeArgs.head
              t.symbol.setInfo(newType)
              set += t.symbol
              treeCopy.ValDef(t, mods, name, TypeTree(newType), q"$reflect($rhs)")
            } else if (rhs.symbol == reflect && set.contains(rhs.asInstanceOf[Apply].args.head.symbol))
              treeCopy.ValDef(t, mods, name, tpt, rhs.asInstanceOf[Apply].args.head)
            else {
              val newRhs = api.recur(rhs) *^ typecheck
              treeCopy.ValDef(t, mods, name, TypeTree(newRhs.tpe), newRhs)
            }

          case Block(stats, expr) =>
            treeCopy.Block(t, stats.map(api.recur(_)), api.recur(expr))

          case If(cond, thenp, elsep) =>
            treeCopy.If(t, cond, api.recur(thenp), api.recur(elsep))

          case Match(selector, cases) =>
            treeCopy.Match(t, selector, cases.map {
              c => treeCopy.CaseDef(c, c.pat, api.recur(c.guard), api.recur(c.body))
            })

          case Try(block, catches, finalizer) =>
            treeCopy.Try(t, api.recur(block), catches.map {
              c => treeCopy.CaseDef(c, c.pat, api.recur(c.guard), api.recur(c.body))
            }, api.recur(finalizer))

          case LabelDef(name, params, rhs) =>
            treeCopy.LabelDef(t, name, params, api.recur(rhs))

          // :TODO by-name parameters & anonymous functions

            // following does not work since using blockbox macro
//          case _ if (t.tpe.erasure == fromType) =>
//            q"$reflect($t)"

          case _ =>
            t
        }
    }
  }

}
