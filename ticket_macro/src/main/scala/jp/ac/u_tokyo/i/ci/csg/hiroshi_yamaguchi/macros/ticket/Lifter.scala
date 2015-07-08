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

import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi._
import util.infixify._
import macros.base.EnhancedMacroBundleBase

trait Lifter {
  self: EnhancedMacroBundleBase =>

  import c.universe._
  import c.internal._

  def lifter(tree: Tree)(byValue: MethodSymbol, byNeed: MethodSymbol,
                         unit: Tree => Tree,
                         bind: (Tree, Tree) => Tree,
                         fmap: (Tree, Tree) => Tree): Tree = {

    def isPure(t: Tree): Boolean = t match {
      // no byValue, no byNeed
      case Apply(fun, _) =>
        fun.symbol != byValue && fun.symbol != byNeed

      case ValDef(mods, name, tpt, rhs) =>
        isPure(rhs)

      case Block(stats, expr) =>
        stats.forall(isPure(_)) && isPure(expr)

      case If(cond, thenp, elsep) =>
        isPure(thenp) && isPure(elsep)

      // :TODO support impure guards
      case Match(selector, cases) => {
        if (cases.exists(c => !isPure(c.guard)))
          error(t)("impure guards are not supported")
        cases.forall(c => isPure(c.body))
      }

      case Try(block, catches, finalizer) => {
        if (catches.exists(c => !isPure(c.guard)))
          error(t)("impure guards are not supported")
        isPure(block) && catches.forall(c => isPure(c.body)) && isPure(finalizer)
      }

      // :TODO support impure while loops
      case LabelDef(name, params, rhs) => {
        val res = isPure(rhs)
        if (!res)
          error(t)("impure while loops are not supported (LabelDef is very difficult!)")
        res
      }

      case _ =>
        true
    }

    transform(tree) {
      (t, api) =>
        if (isPure(t))
          t *^ toList *^*(extendLast, unit(_)) *^ toBlock
        else t match {
          case Apply(fun, List(arg)) =>
              arg

          case Block(_, _) =>
            def loop(cc: List[Tree]): List[Tree] = cc match {
              case Nil => Nil
              case (head@ValDef(mods, name, tpt, rhs)) +: tail =>
                if (isPure(rhs))
                  head +: loop(tail)
                else {
                  val wrhs = api.recur(rhs) *^ defineVal
                  if (tail.forall(isPure(_))) {
                    val fun0 = cc *^ defineFunction *^ defineVal
                    List[Tree](wrhs, fun0, fmap(makeRef(wrhs), makeRef(fun0)))
                  } else {
                    val fun0 = {
                      val Function(vparams, body) = cc *^ defineFunction
                      q"(..$vparams) => ${api.recur(body)}" *^ typecheck
                    } *^ defineVal
                    List[Tree](wrhs, fun0, bind(makeRef(wrhs), makeRef(fun0)))
                  }
                }
              case List(expr) =>
                val wrhs = api.recur(expr) *^ defineVal
                List(wrhs, makeRef(wrhs))
              case _ =>
                error(cc *^ toBlock)("something is wrong")
            }

            loop(t *^ toList) *^ toBlock

          case If(cond, thenp, elsep) =>
            q"if ($cond) ${api.recur(thenp)} else ${api.recur(elsep)}" *^ typecheck

          case Match(selector, cases) =>
            q"$selector match { case ..${
              cases.map {
                c =>
                  cq"${c.pat} if ${c.guard} => ${api.recur(c.body)}"
              }
            } }" *^ typecheck

          case Try(block, catches, finalizer) =>
            q"try ${
              api.recur(block)
            } catch { case ..${
              catches.map {
                c =>
                  cq"${c.pat} if ${c.guard} => ${api.recur(c.body)}"
              }
            } } finally ${
              api.recur(finalizer)
            } " *^ typecheck

          case _ =>
            t
        }
    }
  }

}
