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

trait Extractors {
  self: EnhancedMacroBundleBase =>

  import c.universe._

  // copy and modify from TreeInfo in the scala compiler
  class FineGrainedApply(val tree: Apply) {

    val callee: Tree = {
      def loop(tree: Tree): Tree = tree match {
        case Apply(fn, _) => loop(fn)
        case tree => tree
      }
      loop(tree)
    }

    val tapply: FineGrainedTypeApply = new FineGrainedTypeApply(callee)

    val argss: List[List[Tree]] = {
      def loop(tree: Tree): List[List[Tree]] = tree match {
        case Apply(fn, args) => loop(fn) :+ args
        case _ => Nil
      }
      loop(tree)
    }

    // by-name (!attention! "&&" and "||" do not contains by-name flag!)
    val isByName: List[List[Boolean]] = {
      if (isBooleanSC(callee.symbol))
        tapply.core.tpe.paramLists.map(_.map(_ => true))
      else
        tapply.core.tpe.paramLists.map(_.map(_.asTerm.isByNameParam))
    }

  }

  object FineGrainedApply {
    def unapply(tree: Tree): Option[(Tree, TermName, List[Tree], List[List[(Tree, Boolean)]])] =
      tree match {
        case t@Apply(_, _) =>
          val applied = new FineGrainedApply(t)
          val tapply = applied.tapply
          Some((tapply.receiver, tapply.method, tapply.targs,
            applied.argss.zip(applied.isByName).map(p => p._1.zipAll(p._2, EmptyTree, false))))
        case _ =>
          None
      }

    def copy(tree: Tree, receiver: Tree, method: TermName,
             targs: List[Tree], argss: List[List[Tree]]): Tree = {

      def applyChain(t: Tree, a: List[List[Tree]]): Tree = t match {
        case Apply(fn, _) =>
          treeCopy.Apply(t, applyChain(fn, a.init), a.last)
        case _ =>
          FineGrainedTypeApply.copy(t, receiver, method, targs)
      }

      applyChain(tree, argss).asInstanceOf[Apply]
    }
  }

  class FineGrainedTypeApply(val tree: Tree) {
    val core: Tree = tree match {
      case TypeApply(fn, _) => fn
      case AppliedTypeTree(fn, _) => fn
      case tree => tree
    }

    val targs: List[Tree] = tree match {
      case TypeApply(_, args) => args
      case AppliedTypeTree(_, args) => args
      case _ => Nil
    }

    val receiver: Tree = core match {
      case Select(r, _) => r
      case Ident(_) => EmptyTree
      case _ =>
        unreachableError(tree)
    }

    val method: TermName = core match {
      case Select(_, m) => m.toTermName
      case Ident(m) => m.toTermName
      case _ =>
        unreachableError(tree)
    }
  }


  object FineGrainedTypeApply {
    def unapply(tree: Tree): Option[(Tree, TermName, List[Tree])] =
      tree match {
        case TypeApply(_, _) | AppliedTypeTree(_, _) =>
          val tapply = new FineGrainedTypeApply(tree)
          Some((tapply.receiver, tapply.method, tapply.targs))
        case _ =>
          None
      }

    def copy(tree: Tree, receiver: Tree, method: TermName, targs: List[Tree]): Tree = {

      def tapplyChain(t: Tree): Tree = t match {
        case TypeApply(fn, _) =>
          treeCopy.TypeApply(t, core(fn), targs)
        case AppliedTypeTree(fn, _) =>
          treeCopy.AppliedTypeTree(t, core(fn), targs)
        case fn => core(fn)
      }

      def core(t: Tree): Tree = t match {
        case Select(_, _) => treeCopy.Select(t, receiver, method)
        case Ident(_) => treeCopy.Ident(tree, method)
        case _ =>
          unreachableError(t)
      }

      tapplyChain(tree)
    }
  }

}
