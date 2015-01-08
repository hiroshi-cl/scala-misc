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

trait IndexedMonad[CX, Q, R] extends Any {
  type C[+X, +I, -J]

  @inline
  def unit[A](x: A): C[A, R, R]

  @inline
  def bind[A, B, S, T, U](container: C[A, S, T], f: A => C[B, T, U]): C[B, S, U]

  @inline
  def fmap[A, B, S, T](container: C[A, S, T], f: A => B): C[B, S, T]

  @inline
  def impure[A](a: A): Reflect[A, C[A, Q, R]] = macro CPSBundle.applyImpl[Reflect[A, C[A, Q, R]]]

  @inline
  def reify[A](a: A): C[A, Q, R] = macro CPSBundle.applyImpl[C[A, Q, R]]

  @inline
  def apply[A](a: A): C[A, Q, R] = macro CPSBundle.applyImpl[C[A, Q, R]]

  def reflect[A, S, T](c: C[A, S, T]): Reflect[A, C[A, S, T]] = new Reflect[A, C[A, S, T]](c)
}

object IndexedMonad {
  @inline
  implicit def monadProxy[CX, Q, R, C[+ _, - _, + _], A]:
  ByNameProxy[IndexedMonad[CX, Q, R], C[A, Q, R]] = macro ByNameBundle.byNameProxy
}