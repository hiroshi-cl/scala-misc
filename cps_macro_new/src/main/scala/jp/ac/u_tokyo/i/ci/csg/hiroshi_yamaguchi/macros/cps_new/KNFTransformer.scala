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

import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.base.EnhancedMacroBundleBase
import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.util.infixify._

trait KNFTransformer extends RecursiveTransformer with Util {
  self: EnhancedMacroBundleBase =>

  import c.universe._
  import c.internal._
  import decorators._

  // selective knf transform
  def knfTransform(trees: List[Tree]): List[Tree] = trees.flatMap { tree =>
    (typecheck(tree)) match {
      case EmptyTree =>
        List[Tree]()

      case _ if isSingleton(tree) =>
        List(tree)

      case Select(qual, name) =>
        recursiveTransform(List(qual)) *^*(selectiveExtendLast, treeCopy.Select(tree, _: Tree, name))

      // type annotation may not be reachable here
      case Annotated(annot, arg) =>
        recursiveTransform(List(arg)) *^*(selectiveExtendLast, treeCopy.Annotated(tree, annot, _: Tree))

      case Typed(expr, tpt) =>
        recursiveTransform(List(expr)) *^*(selectiveExtendLast, treeCopy.Typed(tree, _: Tree, tpt))

      case FineGrainedApply(receiver, method, targs, argss) => {
        val rl = {
          val tmp = recursiveTransform(List(receiver))
          if (tmp.isEmpty)
            List(EmptyTree)
          else
            tmp
        }
        val arss =
          for (ps <- argss) yield
            for (p <- ps) yield
              if (p._2)
                (List(p._1), p._2)
              else
                (recursiveTransform(List(p._1)), p._2)

        if (arss.exists(_.exists(x => expansionRequired(x._1)))) {
          val rs :+ r = selectiveExtendLast(rl, identity)
          val arss2 =
            for (ars <- arss) yield
              for ((ar, byName) <- ars) yield
                if (ar.isEmpty)
                  List()
                else if (byName)
                  ar
                else
                  smartExtendLast(ar, identity)
          var list = rs
          for (ars <- arss2; ar <- ars)
            ar match {
              case as :+ a => list ++= as
              case _ =>
            }
          val namess =
            for (ars <- arss2) yield
              for (ar <- ars) yield
                ar match {
                  case as :+ a => a
                  case _ => EmptyTree
                }
          list :+ FineGrainedApply.copy(tree, r, method, targs, namess)
        }
        else rl *^*(selectiveExtendLast,
          FineGrainedApply.copy(tree, _: Tree, method, targs, arss.map(_.map(_._1.head))))
      }

      case FineGrainedTypeApply(receiver, method, targs) =>
        recursiveTransform(List(receiver)) *^*(selectiveExtendLast, FineGrainedTypeApply.copy(tree, _: Tree, method, targs))

      case ValDef(mods, name, tpt, rhs) => {
        //      if (mods.hasFlag(Flag.LAZY))
        //        debug(tree)("applying to lazy val may cause unexpected behavior")

        //      if (mods.hasFlag(Flag.MUTABLE))
        //        debug(tree)("applying to mutable variable (var) may cause unexpected behavior")

        // avoid redundant valDefs
        recursiveTransform(List(rhs)) *^*(applyLast, {
          (t: Tree) =>
            val tt = t *^ typecheck
            treeCopy.ValDef(tree, mods, name, TypeTree(tt.tpe), tt)
        })
      }

      case Block(stats, expr) =>
        val statsl :+ exprl = recursiveTransform(toList(tree))
        List(treeCopy.Block(tree, statsl, exprl))

      case If(cond, thenp, elsep) =>
        val newCond = recursiveTransform(List(cond))
        val newThenp = recursiveTransform(toList(thenp)) *^ toBlock
        val newElsep = recursiveTransform(toList(elsep)) *^ toBlock
        newCond *^*(selectiveExtendLast, treeCopy.If(tree, _: Tree, newThenp, newElsep) *^ typeRestore)

      case Match(selector, cases) =>
        val newSelector = recursiveTransform(List(selector))
        val newCases = cases.map(transformCaseDef(_))
        newSelector *^*(selectiveExtendLast, treeCopy.Match(tree, _: Tree, newCases) *^ typeRestore)

      case Return(expr) =>
        recursiveTransform(List(expr)) *^*(selectiveExtendLast, treeCopy.Return(tree, _: Tree))

      case Throw(expr) =>
        recursiveTransform(List(expr)) *^*(selectiveExtendLast, treeCopy.Throw(tree, _: Tree))

      case Try(block, catches, finalizer) =>
        val newBlock = recursiveTransform(toList(block)) *^ toBlock
        val newCatches = catches.map(transformCaseDef(_))
        val newFinalizer = if (finalizer.isEmpty) EmptyTree else recursiveTransform(toList(finalizer)) *^ toBlock
        // avoid @Object
        List(treeCopy.Try(tree, newBlock, newCatches, newFinalizer) *^ typeRestore)

      case t@CaseDef(_, _, _) =>
        List(transformCaseDef(t))

      case LabelDef(name, params, rhs) =>
        List(treeCopy.LabelDef(tree, name, params, recursiveTransform(List(rhs)) *^ toBlock))


      case Assign(lhs, rhs) => {
        //      debug(tree)("applying to assignment may cause unexpected behavior")
        recursiveTransform(List(rhs)) *^*(selectiveExtendLast, treeCopy.Assign(tree, lhs, _: Tree))
      }

      case Alternative(trees) => {
        //      debug(tree)("applying to pattern match expressions may cause unexpected behavior")
        List(treeCopy.Alternative(tree, trees))
      }

      case Bind(sym, body) => {
        //      debug(tree)("applying to pattern match expressions may cause unexpected behavior")
        List(treeCopy.Bind(tree, sym, body))
      }

      case Star(elem) => {
        //      debug(tree)("applying to pattern match expressions may cause unexpected behavior")
        List(treeCopy.Star(tree, elem))
      }

      case UnApply(fun, args) => {
        error(tree)("Unapply is not supported because (_: Tree).untypecheck().typecheck() is not a total function!")
        //      debug(tree)("applying to pattern match expressions may cause unexpected behavior")
        //      List(treeCopy.UnApply(tree, fun, args))
      }

      // * maybe not reachable
      case AssignOrNamedArg(_, _) | ReferenceToBoxed(_) | _: TypeTree | RefTree(_, _)
           | ImportSelector(_, _, _, _) | Template(_, _, _) =>
        unreachableError(tree)

      case _ =>
        List(tree)
    }
  }
}
