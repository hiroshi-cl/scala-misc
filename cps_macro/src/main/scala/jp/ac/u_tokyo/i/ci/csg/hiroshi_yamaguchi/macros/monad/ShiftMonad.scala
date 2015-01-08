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

object ShiftMonad {

  class ShiftContainer[+A, R](val ctl: (A => R) => R) extends AnyVal

  implicit def shiftMonad[R] = new Monad[ShiftContainer[_, R]] {
    type C[+X] = ShiftContainer[X, R]

    @inline
    override def unit[A](x: A): C[A] = new ShiftContainer[A, R]((k: A => R) => k(x))

    @inline
    override def bind[A, B](container: C[A], f: (A) => C[B]): C[B] =
      new ShiftContainer[B, R](
        (k: B => R) =>
          container.ctl {
            (s: A) =>
              f(s).ctl(k)
          }
      )

    @inline
    override def fmap[A, B](container: C[A], f: (A) => B): C[B] =
      new ShiftContainer[B, R]((k: B => R) =>
        container.ctl {
          (s: A) =>
            k(f(s))
        })
  }

  @inline
  def shift[A, R](ctl: (A => R) => R) =
    new Reflect[A, ShiftContainer[A, R]](new ShiftContainer[A, R](ctl))

  @inline
  def reset[R](thunk: => R)(implicit f: ByNameProxy[Monad[ShiftContainer[_, R]], ShiftContainer[R, R]]): R =
    f.a.ctl(identity)
}
