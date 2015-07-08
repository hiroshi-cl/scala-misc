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

trait Util extends RecursiveTransformer {
  self: EnhancedMacroBundleBase =>

  import c.universe._
  import c.internal._

  def transformCaseDef(caseDef: CaseDef): CaseDef = {
    // :TODO knf of pat
    treeCopy.CaseDef(caseDef, caseDef.pat, caseDef.guard, recursiveTransform(toList(caseDef.body)) *^ toBlock)
  }

  def selectiveExtendLast(ts: List[Tree], f: Tree => Tree): List[Tree] = ts match {
    case List(t) =>
      t.tpe match {
        case AnnotatedType(_, _) =>
          val x = defineVal(t)
          List(x, f(makeRef(x)))
        case _ =>
          List(f(t))
      }
    case ts =>
      smartExtendLast(ts, f(_))
  }

  def expansionRequired(ts: List[Tree]): Boolean = ts match {
    case List(t) =>
      t.tpe match {
        case AnnotatedType(_, _) =>
          true
        case _ =>
          false
      }
    case _ =>
      true
  }

  def isIgnored(tree: Tree): Boolean = tree match {
    case PackageDef(_, _) | TypeDef(_, _, _, _)
         | ClassDef(_, _, _, _) | ModuleDef(_, _, _) | DefDef(_, _, _, _, _, _)
         | Import(_, _) =>
      true
    case _ if isSingleton(tree) =>
      true
    case _ =>
      false
  }

  def getAnnotations(tree: Tree): Set[Annotation] = tree.tpe match {
    case AnnotatedType(annotations, _) => Set(annotations.seq: _*)
    case _ => Set()
  }

  class TransformApi(val transformer: (Tree, TransformApi) => List[Tree]) {
    def recurList(tree: Tree) = transformer(tree, this)

    def recurBlock(tree: Tree) = transformer(tree, this) *^ toBlock

    def recur(tree: Tree) = {
      transformer(tree, this) match {
        case List(t) => t
        case ts => error(tree)("invalid transformation\n" + ts)
      }
    }
  }

  def transform(tree: Tree)(transformer: (Tree, TransformApi) => List[Tree]) =
    new TransformApi(transformer).recurList(tree)

  def dval(tpe: Type): Tree = (tpe match {
    case _ if tpe weak_<:< weakTypeOf[Unit] => q"() : $tpe"
    case _ if tpe weak_<:< weakTypeOf[Boolean] => q"false : $tpe"
    case _ if tpe weak_<:< weakTypeOf[Char] => q"'\0' : $tpe"
    case _ if tpe weak_<:< weakTypeOf[Byte] => q"${0.toByte} : $tpe"
    case _ if tpe weak_<:< weakTypeOf[Short] => q"${0.toByte} : $tpe"
    case _ if tpe weak_<:< weakTypeOf[Int] => q"0 : $tpe"
    case _ if tpe weak_<:< weakTypeOf[Long] => q"0l : $tpe"
    case _ if tpe weak_<:< weakTypeOf[Float] => q"0.0f : $tpe"
    case _ if tpe weak_<:< weakTypeOf[Double] => q"0.0d : $tpe"
    case _ => q"null : $tpe"
  }) *^ typecheck

  def typeRestore(t: Tree): Tree = {
    val tree = typecheck(t)
    val annots = tree match {
      case If(cond, thenp, elsep) =>
        (getAnnotations(thenp) | getAnnotations(elsep)).map(_.tree.tpe).toList
      case Match(selector, cases) =>
        cases.map(getAnnotations(_)).fold(Set())(_ | _).map(_.tree.tpe).toList
      case Try(block, catches, finalizer) =>
        (getAnnotations(block) | catches.map(getAnnotations(_)).fold(Set())(_ | _) | getAnnotations(finalizer)).
          map(_.tree.tpe).toList
      case _ =>
        debug(tree)("no type restoration except for If, Match and Try")
        List()
    }
    // avoid @Object
    if (annots.isEmpty)
      tree
    else
      q"$tree : ${tq"${tree.tpe}@..$annots"}" *^ typecheck
  }
}
