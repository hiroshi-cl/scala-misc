package jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.util.java8adapter

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

import java.util.function.{BinaryOperator, Consumer, IntBinaryOperator, Supplier}

object Test {
  import jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.util.java8adapter.Java8Adapter._

  def main(args: Array[String]): Unit = {
    j8fi[Supplier[Int]](10).get()
    j8fi[Supplier[Int]](() => 10).get()
    j8fi[Consumer[Int]](println(_: Int)).accept(10)
    j8fi[BinaryOperator[Int]]((a: Int, b: Int) => a + b).apply(10, 20)
    j8fi[IntBinaryOperator]((a: Int, b: Int) => a + b).applyAsInt(10, 20)
  }
}
