package jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.monad

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

import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros._
import cps._
import byname._

import language.higherKinds
import language.implicitConversions
import language.experimental.macros

trait Monad[CX] extends Any {
  type C[+X]

  @inline
  def unit[A](x: A): C[A]

  @inline
  def bind[A, B](container: C[A], f: A => C[B]): C[B]

  @inline
  def fmap[A, B](container: C[A], f: A => B): C[B]

  @inline
  def impure[A](a: A): Reflect[A, C[A]] = macro CPSBundle.applyImpl[Reflect[A, C[A]]]

  @inline
  def reify[A](a: A): C[A] = macro CPSBundle.applyImpl[A]

  @inline
  def apply[A](a: A): C[A] = macro CPSBundle.applyImpl[A]

  @inline
  def reflect[A](c: C[A]): Reflect[A, C[A]] = new Reflect[A, C[A]](c)
}

object Monad {
  @inline
  implicit def monadProxy[CX, C[+ _], A]: ByNameProxy[Monad[CX], C[A]] = macro ByNameBundle.byNameProxy

  @inline
  implicit def monad2Proxy[CX, C[+ _, _], A, R]: ByNameProxy[Monad[CX], C[A, R]] = macro ByNameBundle.byNameProxy
}

