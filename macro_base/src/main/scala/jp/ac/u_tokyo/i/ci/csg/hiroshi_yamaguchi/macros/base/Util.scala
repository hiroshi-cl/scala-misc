package jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.base

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

import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.util.infixify._

trait Util {
  self: EnhancedMacroBundleBase =>

  import c.universe._
  import c.internal._
  import decorators._

  def toList[T <: Tree](tree: T): List[Tree] = tree match {
    case Block(stats, expr) => stats :+ expr
    case EmptyTree => Nil
    case t => List(t)
  }

  def defineVal[T <: Tree](tree: T): ValDef = tree match {
    case t@ValDef(_, _, _, _) =>
      t

    case _ =>
      val name = TermName(c.freshName())

      import Flag._
      (q"${ARTIFACT | SYNTHETIC} val $name = $tree".asInstanceOf[ValDef] *^ typecheck)
  }

  def typecheck[T <: Tree](tree: T): T = c.typecheck(tree).asInstanceOf[T]

  def untypecheck[T <: Tree](tree: T): T =
    if (tree.exists(
      _ match {
        case UnApply(_, _) =>
          true
        case _ =>
          false
      }
    ))
      error(tree)("UnApply is not supported because (c.typecheck . c.untypecheck) is not identity! (I'm angry!)")
    else
      c.untypecheck(tree).asInstanceOf[T]

  def refresh[T <: Tree](tree: T): T = tree *^ untypecheck *^ typecheck

  def removeRedundantValDefs[T <: Tree](tree: T): Tree = {
    var from: List[Symbol] = Nil
    var to: List[Symbol] = Nil
    var newTree = transform(tree) {
      (t, api) =>
        t match {
          case ValDef(mods, name, tpt, i@Ident(_)) =>
            from +:= t.symbol
            to +:= i.symbol
            EmptyTree

          case Block(stats, expr) =>
            treeCopy.Block(t, stats.map(api.recur), api.recur(expr))

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
    for ((f, t) <- (from, to).zipped)
      newTree = newTree.substituteSymbols(List(f), List(t))
    newTree *^ typecheck
  }

  def makeRef(valDef: ValDef): Tree =
    gen.mkAttributedStableRef(valDef.symbol) *^ typecheck

  def toBlock(list: List[Tree]): Tree = q"{..$list}" *^ typecheck

  def defineFunction(list: List[Tree]): Function = {
    import Flag._
    list match {
      case (x@ValDef(_, _, _, _)) +: xs =>
        val param = valDef(x.symbol.setFlag(PARAM | ARTIFACT | SYNTHETIC))
        q"($param) => {..$xs}".asInstanceOf[Function] *^ typecheck
      case x +: Nil =>
        val param = valDef(x.*^(defineVal).symbol.setFlag(PARAM | ARTIFACT | SYNTHETIC))
        q"($param) => {${param *^ makeRef}}".asInstanceOf[Function] *^ typecheck

      case _ =>
        unreachableError(list *^ toBlock)
    }
  }

  def applyLast(list: List[Tree], f: Tree => Tree): List[Tree] =
    list.init :+ f(list.last)

  def extendLast(list: List[Tree], f: Tree => Tree): List[Tree] = {
    val x = list.last *^ defineVal
    list.init :+ x :+ f(x *^ makeRef)
  }

  // :TODO return & throw
  def smartExtendLast(list: List[Tree], f: Tree => Tree): List[Tree] =
    if (isSingleton(list.last))
      list *^*(applyLast, f)
    else {
      val x = list.last *^ defineVal
      list.init :+ x :+ f(x *^ makeRef)
    }

}
