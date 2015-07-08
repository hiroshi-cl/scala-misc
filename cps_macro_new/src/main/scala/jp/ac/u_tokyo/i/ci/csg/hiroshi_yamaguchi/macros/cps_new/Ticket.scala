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

import scala.language.experimental.macros

object Ticket {

  import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.macros.base.EnhancedMacroBundleBase
  import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.util.infixify._

//  import scala.reflect.macros.blackbox.Context
  import scala.reflect.macros.whitebox.Context

  class Bundle(override val c: Context) extends EnhancedMacroBundleBase(c)
  with KNFTransformer with SimpleForkJoinTransformer with CPSTransformer {

    import c.universe._

    override def recursiveTransform(trees: List[Tree]): List[Tree] =
      trees *^ knfTransform *^ insertForkJoin *^ cpsTransform

    def ticketImpl(a: Tree) = a *^ toList *^ recursiveTransform *^ toBlock *^ refresh
  }

  type cps[T] = CPSTransformer.cps[T]
  type CPSReflect[T] = CPSTransformer.CPSReflect[T]
  type forkJoin[T] = SimpleForkJoinTransformer.forkJoin[T]
  type ForkJoinReflect[T] = SimpleForkJoinTransformer.ForkJoinReflect[T]
  type TicketProxy[T] = ByNameTicketBundle.TicketProxy[T]

  def ticket[R](a: R): R = macro Bundle.ticketImpl
  implicit def ticketProxy[T]: TicketProxy[T] = macro ByNameTicketBundle.byName
}