package jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi.util

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

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import infixify._

object InfixifyCheck extends Properties("Infixify") {
  property("infixify") = forAll { (a: Int, b: Int) =>
    a + b == a *^*(_ + (_ : Int), b)
  }
  property("postfixify") = forAll { (a: Int, b: Int) =>
    a + b == a *^ (_ + b)
  }
  property("associativity 1") = forAll { (a: Int, b: Int, c: Int) =>
    a - b - c == a *^*(_ - (_ : Int), b) *^*(_ - (_ : Int), c)
  }
  property("associativity 2") = forAll { (a: Int, b: Int, c: Int) =>
    a - b - c == a *^ (_ - b) *^ (_ - c)
  }
}

