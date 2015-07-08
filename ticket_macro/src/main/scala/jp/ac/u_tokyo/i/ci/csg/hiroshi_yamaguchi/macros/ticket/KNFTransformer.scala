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

trait KNFTransformer {
   self: EnhancedMacroBundleBase =>

   import c.universe._

   def knfTransform(tree: Tree): Tree =
     if (tree == EmptyTree)
       EmptyTree
     else
       transformImpl(List(tree) *^ toBlock) *^ toBlock

   private[this] def transformImpl(tree: Tree): List[Tree] = tree match {

     case _ if isIgnored(tree) =>
       List(tree)

     case Select(qual, name) =>
       transformImpl(qual) *^*(smartExtendLast, treeCopy.Select(tree, _: Tree, name))

     // type annotation may not be reachable here
     case Annotated(annot, arg) =>
       transformImpl(arg) *^*(smartExtendLast, treeCopy.Annotated(tree, annot, _: Tree))

     case Typed(expr, tpt) =>
       transformImpl(expr) *^*(smartExtendLast, treeCopy.Typed(tree, _: Tree, tpt))

     case FineGrainedApply(receiver, method, targs, argss) =>
       var list = List[Tree]()

       def knf(a: Tree, isByName: Boolean) =
         if (isByName || isIgnored(a) || a == EmptyTree)
           a
         else {
           val l = transformImpl(a) *^*(extendLast, identity(_: Tree))
           list ++= l.init
           l.last
         }

       val r = knf(receiver, false)
       val namess = argss.map(_.map(p => knf(p._1, p._2)))
       list :+ FineGrainedApply.copy(tree, r, method, targs, namess)

     case FineGrainedTypeApply(receiver, method, targs) =>
       transformImpl(receiver) *^*(smartExtendLast, FineGrainedTypeApply.copy(tree, _: Tree, method, targs))

     case ValDef(mods, name, tpt, rhs) =>
       // :TODO Lazy
       if (mods.hasFlag(Flag.LAZY))
         error(tree)("lazy is not supported by KNF Transformer")

       // :TODO Mutable variable
       if (mods.hasFlag(Flag.MUTABLE))
         error(tree)("mutable variable (var)  is not supported by KNF Transformer")

       // avoiding redundant valDefs
       transformImpl(rhs) *^*(applyLast, treeCopy.ValDef(tree, mods, name, tpt, _: Tree))

     // * stage 2

     case Block(stats, expr) =>
       val statsl :+ exprl = stats.map(transformImpl(_) *^*(applyLast, defineVal(_: Tree))).flatten ++ transformImpl(expr)
       List(treeCopy.Block(tree, statsl, exprl))

     case If(cond, thenp, elsep) =>
       transformImpl(cond) *^*(smartExtendLast, treeCopy.If(tree, _: Tree, knfTransform(thenp), knfTransform(elsep)))

     case Match(selector, cases) =>
       transformImpl(selector) *^*(smartExtendLast, treeCopy.Match(tree, _: Tree, cases.map(transformCaseDef(_))))

     case Return(expr) =>
       transformImpl(expr) *^*(smartExtendLast, treeCopy.Return(tree, _: Tree))

     case Throw(expr) =>
       transformImpl(expr) *^*(smartExtendLast, treeCopy.Throw(tree, _: Tree))

     case Try(block, catches, finalizer) =>
       List(treeCopy.Try(tree, knfTransform(block), catches.map(transformCaseDef(_)), knfTransform(finalizer)))

     case t@CaseDef(_, _, _) =>
       List(transformCaseDef(t))

     case LabelDef(name, params, rhs) =>
       List(treeCopy.LabelDef(tree, name, params, knfTransform(rhs)))

     // ignore
     case PackageDef(_, _) | TypeDef(_, _, _, _)
          | ClassDef(_, _, _, _) | ModuleDef(_, _, _) | DefDef(_, _, _, _, _, _)
          | Import(_, _) =>
       List(tree)

     // :TODO assignment
     case Assign(_, _) =>
       error(tree)("assignment is not supported in this version")

     // * maybe not reachable
     case AssignOrNamedArg(_, _) | ReferenceToBoxed(_) | _: TypeTree | RefTree(_, _)
          | ImportSelector(_, _, _, _) | Template(_, _, _) =>
       unreachableError(tree)

     // * pattern match expressions are not supported
     case Alternative(_) | Bind(_, _) | Star(_) | UnApply(_, _) =>
       unreachableError(tree)

     case _ =>
       unreachableError(tree)
   }

   private[this] def transformCaseDef(caseDef: CaseDef): CaseDef = {
     // :TODO knf of pat
     treeCopy.CaseDef(caseDef, caseDef.pat, caseDef.guard, knfTransform(caseDef.body))
   }

   private[this] def isIgnored(tree: Tree): Boolean = tree match {
     case PackageDef(_, _) | TypeDef(_, _, _, _)
          | ClassDef(_, _, _, _) | ModuleDef(_, _, _) | DefDef(_, _, _, _, _, _)
          | Import(_, _)  =>
       true
     case _ if isSingleton(tree) =>
       true
     case _ =>
       false
   }
 }
