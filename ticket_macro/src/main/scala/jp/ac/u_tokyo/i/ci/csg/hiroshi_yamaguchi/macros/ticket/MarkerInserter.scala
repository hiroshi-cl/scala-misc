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

import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.base.EnhancedMacroBundleBase
import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.util.infixify._
import scala.collection.mutable.HashMap

trait MarkerInserter {
  self: EnhancedMacroBundleBase =>

  import c.universe._
  import c.internal._
  import decorators._

  def moveMarkers(tree: Tree)
                 (byName: MethodSymbol, byValue: MethodSymbol, byNeed: MethodSymbol): Tree = {
    reorder(moveMarkersImpl(List(tree) *^ toBlock *^ (removeRedundantValDefs))(byName, byValue, byNeed))(byNeed)
  }

  /*
  1. find by-need position and replace markers
  2. insert by-value markers & remove by-name markers
   */

  // :TODO verify nested cases (if, match, ...)
  // :TODO <- supported by lifter
  // :TODO <- no! precisely, it requires recursive transformation

  private[this] def moveMarkersImpl(tree: Tree)
                                   (byName: MethodSymbol,
                                    byValue: MethodSymbol,
                                    byNeed: MethodSymbol): Tree = {
    val fromType = byName.paramLists.head.head.info.typeConstructor
    val map = HashMap.empty[Symbol, Tree]
    var set = Set[Symbol]()
    transform(tree) {
      (t, api) =>
        t match {
          case i@Ident(_) if (map.contains(i.symbol)) =>
            q"$byNeed(${map.remove(i.symbol).getOrElse(null)})" *^ typecheck
          // byNeed marker at the last position regarded as a stub (not invoke)

          case Apply(tpt, List(arg)) if (t.symbol == byName) =>
            val symbol = arg.symbol
            // by-need position
            if (map.contains(symbol))
              q"$byNeed(${map.remove(symbol).getOrElse(null)})" *^ typecheck
            // by-name position
            else if (set.contains(symbol))
              arg // remove by-name marker
            else
              q"$byNeed($byValue($arg))" *^ typecheck


          case ValDef(mods, name, tpt, rhs) =>
            if (tpt.tpe.typeConstructor == fromType) {
              val valDef = q"$mods val $name = $byValue($rhs)".asInstanceOf[ValDef] *^ typecheck
              map(t.symbol) = makeRef(valDef)
              set += t.symbol
              valDef
            } else if (rhs.symbol == byName) {
              val Apply(_, List(arg)) = rhs
              val symbol = arg.symbol
              // by-need position
              if (map.contains(symbol))
                treeCopy.ValDef(t, mods, name, tpt, q"$byNeed(${map.remove(symbol).getOrElse(null)})" *^ typecheck)
              // by-name position
              else if (set.contains(symbol))
                treeCopy.ValDef(t, mods, name, tpt, arg) // remove by-name marker
              else
                treeCopy.ValDef(t, mods, name, tpt, q"$byNeed($byValue($arg))" *^ typecheck)
            } else {
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

          case _ =>
            t
        }
    }
  }

  private[this] def reorder(tree: Tree)(byNeed: MethodSymbol): Tree = {
    transform(tree) {
      (t, api) =>
        t match {

          case ValDef(mods, name, tpt, rhs) =>
            treeCopy.ValDef(t, mods, name, tpt, api.recur(rhs))

          case Block(stats, expr) =>
            var list = List.empty[Tree]
            val map = new HashMap[Symbol, Tree]
            for (tt <- stats :+ expr)
              tt match {
                case ValDef(mods, name, tpt, rhs) if (rhs.symbol == byNeed) =>
                  map(tt.symbol) = tt

                case _ =>
                  tt.foreach {
                    s =>
                      if (map.contains(s.symbol))
                        list :+= map.remove(s.symbol).getOrElse(null)
                  }
                  list :+= api.recur(tt)
              }
            if (!map.isEmpty)
              error(t)("something wrong (map is not empty):\n\t" + map)
            list *^ toBlock *^ typecheck

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

          case _ =>
            t
        }
    }
  }
}
