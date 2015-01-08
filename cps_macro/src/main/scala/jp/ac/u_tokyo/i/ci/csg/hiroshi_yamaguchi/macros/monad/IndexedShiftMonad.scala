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

object IndexedShiftMonad {

  class ShiftContainer[+A, +S, -T](val ctl: (A => T) => S) extends AnyVal

  implicit def shiftMonad[Q, R] = new IndexedMonad[ShiftContainer[_, _, _], Q, R] {
    type C[+X, +I, -J] = ShiftContainer[X, I, J]

    @inline
    override def unit[A](x: A): C[A, R, R] =
      new ShiftContainer[A, R, R]((k: A => R) => k(x))

    @inline
    override def bind[A, B, S, T, U](container: C[A, S, T], f: (A) => C[B, T, U]): C[B, S, U] =
      new ShiftContainer[B, S, U](
        (k: B => U) =>
          container.ctl {
            (s: A) =>
              f(s).ctl(k)
          }
      )

    @inline
    override def fmap[A, B, S, T](container: C[A, S, T], f: (A) => B): C[B, S, T] =
      new ShiftContainer[B, S, T]((k: B => T) =>
        container.ctl {
          (s: A) =>
            k(f(s))
        })
  }

  @inline
  def shift[A, S, T](ctl: (A => T) => S) =
    new Reflect[A, ShiftContainer[A, S, T]](new ShiftContainer[A, S, T](ctl))

  @inline
  def reset[Q, R](thunk: => R)(
    implicit f: ByNameProxy[IndexedMonad[ShiftContainer[_, _, _], Q, R], ShiftContainer[R, Q, R]]): Q =
    f.a.ctl(identity)
}