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

trait Debug {
  self: EnhancedMacroBundleBase =>

  import c.universe._

  def error(tree: Tree)(message: String = "ERROR") = {
    c.abort(tree.pos,
      List(message
        , "** symbol **", Option(tree.symbol).getOrElse(NoSymbol).toString
        , "** code **", show(tree)
        , "** tree **", showRaw(tree)
        , "** stack trace **", Thread.getAllStackTraces.get(Thread.currentThread()).mkString("\n\t")
      ).mkString("\n\t"))
  }

  def debug[T <: Tree](tree: T)(message: String = "DEBUG") = {
    c.warning(tree.pos,
      List(message
        , "** symbol **", Option(tree.symbol).getOrElse(NoSymbol).toString
        , "** code **", show(tree)
        , "** tree **", showRaw(tree)
      ).mkString("\n\t"))
    tree
  }

  def unreachableError(tree: Tree) = error(tree)("unreachable is expected")

  def notImplementedError(tree: Tree) = error(tree)("not implemented yet")


  def macroApply(prefix: Tree)(name: Tree)(a: Tree*) = {
    val Literal(Constant(s: String)) = name
    q"$prefix.${TermName(s).encodedName.toTermName}(..$a)" *^ (c.typecheck(_))
  }

  def catchError(tree: => Tree) = {
    try {
      tree *^ typecheck
    } catch {
      case e: Throwable =>
        q"throw new Exception(${e.toString})" *^ typecheck
    }
  }

  def showType(tpe: Type): String = tpe match {
    case TypeRef(pre, sym, args) =>
      "TypeRef (%s, %s, %s)".format(showType(pre), sym, args.map(showType))
    case AnnotatedType(annotations, underlying) =>
      "AnnotatedType (%s, %s)".format(annotations, showType(underlying))
    case BoundedWildcardType(bounds) =>
      "BoundedWildcardType (%s)".format(showType(bounds))
    case ClassInfoType(parents, decls, clazz) =>
      "ClasInfo (%s, %s, %s)".format(parents.map(showType), decls, clazz)
    case RefinedType(parents, decls) =>
      "RefinedType (%s, %s, %s)".format(parents.map(showType), decls)
    case ExistentialType(quantified, underlying) =>
      "ExistentialType (%s, %s)".format(quantified, showType(underlying))
    case MethodType(params, restpe) =>
      "MethodType (%s, %s)".format(params, showType(restpe))
    case NullaryMethodType(resultType) =>
      "NullaryMethodType (%s)".format(showType(resultType))
    case PolyType(typeParams, resultType) =>
      "PolyType (%s, %s)".format(typeParams, showType(resultType))
    case ConstantType(constant) =>
      "ConstantType (%s)".format(constant)
    case SingleType(pre, sym) =>
      "SingleType (%s, %s)".format(showType(pre), sym)
    case SuperType(thistpe, supertpe) =>
      "SuperType (%s, %s)".format(showType(thistpe), showType(supertpe))
    case ThisType(sym) =>
      "ThisType (%s)".format(sym)
    case TypeBounds(lower, upper) =>
      "TypeBounds (%s, %s)".format(showType(lower), showType(upper))
    case NoPrefix =>
      "NoPrefix"
    case NoType =>
      "NoType"
    case _ =>
      tpe.toString
  }
}
