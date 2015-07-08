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

trait SimpleForkJoinTransformer extends Util {
  self: EnhancedMacroBundleBase =>

  import c.internal._
  import c.universe._
  import decorators._

  def getForkJoinAnnotationType: Type = typeOf[SimpleForkJoinTransformer.forkJoin[_]].typeConstructor

  def replaceIdentifier(tree: Tree): List[Tree] =
    transform(tree *^ typecheck) {
      (t, api) => t match {
        case EmptyTree =>
          List(t)

        case i@Ident(_) if i.symbol.attachments.contains[Tree] =>
          val x = i.symbol.attachments.get[Tree].get *^ defineVal
          List(x, makeRef(x))

        case Select(qual, name) =>
          api.recurList(qual) *^*(smartExtendLast, treeCopy.Select(t, _: Tree, name))

        case _ if isSingleton(t) =>
          List(t)


        // type annotation may not be reachable here
        case Annotated(annot, arg) =>
          api.recurList(arg) *^*(applyLast, treeCopy.Annotated(t, annot, _: Tree))

        case Typed(expr, tpt) =>
          api.recurList(expr) *^*(applyLast, treeCopy.Typed(t, _: Tree, tpt))

        case FineGrainedApply(receiver, method, targs, argss) =>
          val rl :+ r = api.recurList(receiver)
          val ll = argss.map(_.map(p => api.recurList(p._1)))
          val argl = ll.flatMap(_.flatMap(_.init))
          val arss = ll.map(_.map(_.last))
          rl ++ argl :+ FineGrainedApply.copy(t, r, method, targs, arss)

        case FineGrainedTypeApply(receiver, method, targs) =>
          api.recurList(receiver) *^*(smartExtendLast, FineGrainedTypeApply.copy(t, _: Tree, method, targs))

        case ValDef(mods, name, tpt, rhs) =>
          val list :+ newRhs = api.recurList(rhs)
          val tp = TypeTree(typecheck(newRhs).tpe)
          list :+ treeCopy.ValDef(t, mods, name, tp, newRhs)

        case Block(stats, expr) =>
          val newStats :+ newExpr = (stats :+ expr).flatMap(api.recurList(_))
          List(treeCopy.Block(t, newStats, newExpr))

        case If(cond, thenp, elsep) =>
          val newCond = api.recurList(cond)
          val newThenp = api.recurBlock(thenp)
          val newElsep = api.recurBlock(elsep)
          newCond *^*(selectiveExtendLast, treeCopy.If(t, _: Tree, newThenp, newElsep) *^ typeRestore)

        case Match(selector, cases) =>
          val newSelector = api.recurList(selector)
          val newCases = cases.map(transformCaseDef(_, api))
          newSelector *^*(selectiveExtendLast, treeCopy.Match(t, _: Tree, newCases) *^ typeRestore)

        case Return(expr) =>
          api.recurList(expr) *^*(smartExtendLast, treeCopy.Return(t, _: Tree))

        case Throw(expr) =>
          api.recurList(expr) *^*(smartExtendLast, treeCopy.Throw(t, _: Tree))

        case Try(block, catches, finalizer) =>
          val newBlock = api.recurBlock(block)
          val newCatches = catches.map(transformCaseDef(_, api))
          val newFinalizer = if (finalizer.isEmpty) EmptyTree else api.recurBlock(finalizer)
          List(treeCopy.Try(t, newBlock, newCatches, newFinalizer) *^ typeRestore)

        case t@CaseDef(_, _, _) =>
          List(transformCaseDef(t, api))

        case LabelDef(name, params, rhs) =>
          List(treeCopy.LabelDef(t, name, params, api.recurBlock(rhs)))


        case Assign(lhs, rhs) =>
          //          debug(tree)("applying to assignment may cause unexpected behavior")
          api.recurList(rhs) *^*(smartExtendLast, treeCopy.Assign(t, lhs, _: Tree))

        case Alternative(trees) =>
          //          debug(tree)("applying to pattern match expressions may cause unexpected behavior")
          List(treeCopy.Alternative(t, trees))

        case Bind(sym, body) =>
          //          debug(tree)("applying to pattern match expressions may cause unexpected behavior")
          List(treeCopy.Bind(t, sym, body))

        case Star(elem) =>
          //          debug(t)("applying to pattern match expressions may cause unexpected behavior")
          List(treeCopy.Star(t, elem))

        case UnApply(fun, args) =>
          error(t)("Unapply is not supported because (_: Tree).untypecheck().typecheck() is not a total function!")

        // * maybe not reachable
        case AssignOrNamedArg(_, _) | ReferenceToBoxed(_) | _: TypeTree | RefTree(_, _)
             | ImportSelector(_, _, _, _) | Template(_, _, _) =>
          unreachableError(t)

        case _ =>
          List(t)
      }
    }

  def replaceIdentifierBlock(tree: Tree): Tree = replaceIdentifier(tree) *^ toBlock

  def insertForkJoin(trees: List[Tree]): List[Tree] = {
    val ts = trees.flatMap { tree =>
      val t = tree *^ typecheck
      t match {
        case EmptyTree =>
          List(t)

        case i@Ident(_) if i.symbol.attachments.contains[Tree] =>
          replaceIdentifier(t)

        case Select(qual, name) =>
          replaceIdentifier(qual) *^*(smartExtendLast, treeCopy.Select(t, _: Tree, name))

        case _ if isSingleton(t) =>
          List(t)


        // type annotation may not be reachable here
        case Annotated(annot, arg) =>
          replaceIdentifier(arg) *^*(applyLast, treeCopy.Annotated(t, annot, _: Tree))

        case Typed(expr, tpt) =>
          replaceIdentifier(expr) *^*(applyLast, treeCopy.Typed(t, _: Tree, tpt))

        case FineGrainedApply(receiver, method, targs, argss) =>
          val rl :+ r = replaceIdentifier(receiver)
          val ll = argss.map(_.map(p => replaceIdentifier(p._1)))
          val argl = ll.flatMap(_.flatMap(_.init))
          val arss = ll.map(_.map(_.last))
          rl ++ argl :+ FineGrainedApply.copy(t, r, method, targs, arss)

        case FineGrainedTypeApply(receiver, method, targs) =>
          replaceIdentifier(receiver) *^*(smartExtendLast, FineGrainedTypeApply.copy(t, _: Tree, method, targs))

        case ValDef(mods, name, tpt, rhs) => tpt.tpe match {
          case AnnotatedType(annotations, underlying) =>
            annotations.find(_.tree.tpe.typeConstructor weak_<:< getForkJoinAnnotationType) match {
              case Some(a) =>
                import Flag._
                val flags = ARTIFACT | SYNTHETIC
                val T = a.tree.tpe.typeArgs.head
                val tp = tq"ForkJoinReflect[$T]"
                val cache =
                  q"$flags var ${TermName(name + "$tmpvar")} : $underlying = ${dval(underlying)}" *^
                    typecheck *^ (_.asInstanceOf[ValDef])
                val cacheRef = makeRef(cache)
                val cacheVal = defineVal(cacheRef)
                val cacheValRef = makeRef(cacheVal)
                val list :+ recur = replaceIdentifier(rhs)
                val ex = TermName(name + "$e")
                val newRhs =
                  q"try{$cacheRef = $recur;null}catch{case $ex: $tp => $ex.value}" *^ typeRestore
                val newValDef =
                  q"$mods val $name: $T = $newRhs" *^ typecheck *^ (_.asInstanceOf[ValDef])
                val ref = makeRef(newValDef)
                t.symbol.updateAttachment[Tree](
                  q"if($ref == null)$cacheValRef : $underlying else $ref.jget()" *^ typeRestore)
                list ++ List(cache, newValDef, cacheVal)
              case None =>
                val list :+ newRhs = replaceIdentifier(rhs)
                val tp = TypeTree(typecheck(newRhs).tpe)
                list :+ treeCopy.ValDef(t, mods, name, tp, newRhs)
            }
          case _ =>
            val list :+ newRhs = replaceIdentifier(rhs)
            val tp = TypeTree(typecheck(newRhs).tpe)
            list :+ treeCopy.ValDef(t, mods, name, tp, newRhs)
        }

        case Block(stats, expr) =>
          val newStats :+ newExpr = (stats :+ expr).flatMap(replaceIdentifier(_))
          List(treeCopy.Block(t, newStats, newExpr))

        case If(cond, thenp, elsep) =>
          val newCond = replaceIdentifier(cond)
          val newThenp = replaceIdentifierBlock(thenp)
          val newElsep = replaceIdentifierBlock(elsep)
          newCond *^*(selectiveExtendLast, treeCopy.If(t, _: Tree, newThenp, newElsep) *^ typeRestore)

        case Match(selector, cases) =>
          val newSelector = replaceIdentifier(selector)
          val newCases = cases.map(transformCaseDef(_))
          newSelector *^*(selectiveExtendLast, treeCopy.Match(t, _: Tree, newCases) *^ typeRestore)

        case Return(expr) =>
          replaceIdentifier(expr) *^*(smartExtendLast, treeCopy.Return(t, _: Tree))

        case Throw(expr) =>
          replaceIdentifier(expr) *^*(smartExtendLast, treeCopy.Throw(t, _: Tree))

        case Try(block, catches, finalizer) =>
          val newBlock = replaceIdentifierBlock(block)
          val newCatches = catches.map(transformCaseDef(_))
          val newFinalizer = if (finalizer.isEmpty) EmptyTree else replaceIdentifierBlock(finalizer)
          List(treeCopy.Try(t, newBlock, newCatches, newFinalizer) *^ typeRestore)

        case t@CaseDef(_, _, _) =>
          List(transformCaseDef(t))

        case LabelDef(name, params, rhs) =>
          List(treeCopy.LabelDef(t, name, params, replaceIdentifierBlock(rhs)))


        case Assign(lhs, rhs) =>
          //          debug(tree)("applying to assignment may cause unexpected behavior")
          replaceIdentifier(rhs) *^*(smartExtendLast, treeCopy.Assign(t, lhs, _: Tree))

        case Alternative(trees) =>
          //          debug(tree)("applying to pattern match expressions may cause unexpected behavior")
          List(treeCopy.Alternative(t, trees))

        case Bind(sym, body) =>
          //          debug(tree)("applying to pattern match expressions may cause unexpected behavior")
          List(treeCopy.Bind(t, sym, body))

        case Star(elem) =>
          //          debug(t)("applying to pattern match expressions may cause unexpected behavior")
          List(treeCopy.Star(t, elem))

        case UnApply(fun, args) =>
          error(t)("Unapply is not supported because (_: Tree).untypecheck().typecheck() is not a total function!")

        // * maybe not reachable
        case AssignOrNamedArg(_, _) | ReferenceToBoxed(_) | _: TypeTree | RefTree(_, _)
             | ImportSelector(_, _, _, _) | Template(_, _, _) =>
          unreachableError(t)

        case _ =>
          List(t)
      }
    }

    ts match {
      case list :+ last =>
        list.map(tree => tree match {
          case ValDef(_, _, _, _) => tree
          case _ => tree.tpe match {
            case AnnotatedType(annotations, underlying) =>
              annotations.find(_.tree.tpe.typeConstructor weak_<:< getForkJoinAnnotationType) match {
                case Some(a) =>
                  q"try{$tree}catch{case _: ForkJoinReflect[_] =>}" *^ typecheck
                case None =>
                  tree
              }
            case _ =>
              tree
          }
        }) :+ last
      case _ => List()
    }
  }

  private[this] def transformCaseDef(caseDef: CaseDef): CaseDef = {
    treeCopy.CaseDef(caseDef, caseDef.pat, caseDef.guard, replaceIdentifierBlock(caseDef.body))
  }

  private[this] def transformCaseDef(caseDef: CaseDef, api: TransformApi): CaseDef = {
    treeCopy.CaseDef(caseDef, caseDef.pat, caseDef.guard, api.recurBlock(caseDef.body))
  }
}

object SimpleForkJoinTransformer {

  final class forkJoin[T] extends scala.annotation.StaticAnnotation with scala.annotation.TypeConstraint

  final class ForkJoinReflect[T](val value: T) extends scala.util.control.ControlThrowable

}