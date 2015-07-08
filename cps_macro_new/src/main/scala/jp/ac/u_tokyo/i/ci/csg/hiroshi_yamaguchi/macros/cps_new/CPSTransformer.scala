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

trait CPSTransformer {
  self: EnhancedMacroBundleBase =>

  import c.internal._
  import c.universe._
  import decorators._

  def getCPSAnnotationType: Type = typeOf[CPSTransformer.cps[_]].typeConstructor

  def cpsTransform(trees: List[Tree]): List[Tree] =
    if (trees.isEmpty)
      List[Tree]()
    else {
      val init :+ last = trees
      init.foldRight(List(last)) {
        case (tree, cont) => (typecheck(tree)) match {

          case ValDef(mods, name, tpt, rhs) => getCPSflagOption(tpt.tpe) match {
            case Some((a, underlying)) =>
              val T = a.tree.tpe.typeArgs.head
              val tp = tq"CPSReflect[$T]"
              val funcName = TermName(name + "$func")
              val paramName = TermName(name + "$x")
              val bn = TermName(name + "$x")
              val ex1 = TermName(name + "$e1")
              val ex2 = TermName(name + "$e2")

              val newValDef = treeCopy.ValDef(tree, mods, name, TypeTree(underlying), q"$paramName")
              val ctnBlock = q"{$newValDef; ..$cont}"
              val ctnTree = q"@inline def $funcName($paramName : $underlying) = $ctnBlock" *^ typecheck
              val ctn = ctnTree.symbol

              val ctlBlock = getCPSflagOption(ctn.info.resultType) match {
                case Some((b, _)) =>
                  q"$ex1.value.bind(($bn: $underlying) => $ctn($bn))"
                case _ =>
                  q"$ex1.value.fmap(($bn: $underlying) => $ctn($bn))"
              }
              val MA = q"{val $ex1: $tp = ???; $ctlBlock}" *^ typecheck *^ (_.tpe)
              val ctp = tq"${removeCPSAnnotation(ctn.info.resultType)}@cps[$MA]"
              val ctl1Tree =
                q"try($rhs)catch{case $ex1:$tp => throw new CPSReflect($ctlBlock)}" *^ typecheck *^ defineVal
              val ref = makeRef(ctl1Tree)
              val ctl2Tree =
                q"$ctn($ref) : $ctp" *^ typecheck

              List(q"{$ctnTree; $ctl1Tree; $ctl2Tree}" *^ typecheck)
            case _ =>
              tree +: cont
          }

          case _ =>
            getCPSflagOption(tree.tpe) match {
              case Some((a, underlying)) =>
                val T = a.tree.tpe.typeArgs.head
                val tp = tq"CPSReflect[$T]"
                val name = c.freshName()
                val funcName = TermName(name + "$func")
                val bn = TermName(name + "$x")
                val ex1 = TermName(name + "$e1")
                val ex2 = TermName(name + "$e2")

                val ctnBlock = q"{..$cont}"
                val ctnTree = q"@inline def $funcName() = $ctnBlock" *^ typecheck
                val ctn = ctnTree.symbol

                val ctlBlock = getCPSflagOption(ctn.info.resultType) match {
                  case Some((b, _)) =>
                    q"$ex1.value.bind(($bn: $underlying) => $ctn())"
                  case _ =>
                    q"$ex1.value.fmap(($bn: $underlying) => $ctn())"
                }
                val MA = q"{val $ex1: $tp = ???; $ctlBlock}" *^ typecheck *^ (_.tpe)
                val ctp = tq"${removeCPSAnnotation(ctn.info.resultType)}@cps[$MA]"
                val ctl1Tree =
                  q"try($tree)catch{case $ex1:$tp => throw new CPSReflect($ctlBlock)}" *^ typecheck
                val ctl2Tree =
                  q"$ctn() : $ctp" *^ typecheck

                List(q"{$ctnTree; $ctl1Tree; $ctl2Tree}" *^ typecheck)
              case _ =>
                tree +: cont
            }
        }
      }
    }

  private[this] def hasCPSflag(tpe: Type): Boolean = !getCPSflagOption(tpe).isEmpty

  private[this] def getCPSflagOption(tpe: Type): Option[(Annotation, Type)] = tpe match {
    case AnnotatedType(annotations, underlying) =>
      annotations.find(_.tree.tpe.typeConstructor weak_<:< getCPSAnnotationType).map((_, underlying))
    case _ => None
  }

  private[this] def removeCPSAnnotation(tpe: Type): Type = tpe match {
    case AnnotatedType(annotations, underlying) =>
      val newAnnots =
        annotations.filterNot(_.tree.tpe.typeConstructor weak_<:< getCPSAnnotationType).map(_.tree.tpe)
      if (newAnnots.isEmpty)
        underlying
      else
        tq"$underlying@..$newAnnots".tpe
    //        AnnotatedType(newAnnots, underlying)
    case _ => tpe
  }
}

object CPSTransformer {

  final class cps[T] extends scala.annotation.StaticAnnotation with scala.annotation.TypeConstraint

  final class CPSReflect[T](val value: T) extends scala.util.control.ControlThrowable

}