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

import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.cps._

import language.higherKinds
import language.implicitConversions

object Monads {
  implicit def optionMonad = new Monad[Option[_]] {
    type C[+X] = Option[X]

    @inline
    override def unit[A](x: A): C[A] = Some(x)

    @inline
    override def bind[A, B](container: C[A], f: (A) => C[B]): C[B] = container.flatMap(f)

    @inline
    override def fmap[A, B](container: C[A], f: (A) => B): C[B] = container.map(f)
  }

  implicit def listMonad = new Monad[List[_]] {
    type C[+X] = List[X]

    @inline
    override def unit[A](x: A): C[A] = List(x)

    @inline
    override def bind[A, B](container: C[A], f: (A) => C[B]): C[B] = container.flatMap(f)

    @inline
    override def fmap[A, B](container: C[A], f: (A) => B): C[B] = container.map(f)
  }

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent._

  implicit def asyncMonad = new RelaxedMonad[Future[_]] {
    // not a real monad
    type C[+X] = Future[X]

    @inline
    override def unit[A](x: A): C[A] = Promise.successful(x).future

    @inline
    override def bind[A, B](container: C[A], f: (A) => C[B]): C[B] = container.flatMap(f)

    @inline
    override def fmap[A, B](container: C[A], f: (A) => B): C[B] = container.map(f)
  }

  implicit class OptionMonad[A](val c: Option[A]) extends AnyVal {
    @inline
    def reflect(): Reflect[A, Option[A]] = optionMonad.reflect(c)
  }

  implicit class ListMonad[A](val c: List[A]) extends AnyVal {
    @inline
    def reflect(): Reflect[A, List[A]] = listMonad.reflect(c)
  }

  implicit class AsyncMonad[A](val c: Future[A]) extends AnyVal {
    @inline
    def reflect(): Reflect[A, Future[A]] = asyncMonad.reflect(c)
  }

}
