# scala-misc

This project uses SBT.

Currently, this project consists of 4 sub-micro-projects:

1. infixify: infixify all two-parameter function and postfixify all one-parameter function.
  For example, `add(a, b)` => `a *^* (add, b)`; `not(b)` => `b *^ not`.
2. macro-base: macro bundles for Scala 2.11 macro.
3. byname macro: rewrites AST of by-name argument using implicit macro.
  For example, in `def byname(t: => A)(implicit rewritten: Hoge[A]) { ...; rewritten.hoge; ... }`,
  `rewritten` is completed by rewritten AST of `t`.
4. cps macro: provides Fillinski's representing monads (reify/reflect) directly.
  Shift/resets (both of answer type modification enabled version and disabled version) are also available.
  (Slightly more type annotations is required than CPS plugin.)
  Different from other macro libraries providing CPS transformation (e.g. Effectfully, Scala Async, Scala Workflow, etc.), library developers can hide macro keywords by using byname macro.


# 日本語

このプロジェクトは SBT を使っています.

現在、このプロジェクトは4つのマイクロプロジェクトからなっています:

1. infixify: すべての2引数関数を中置化し、すべての1引数関数を後置化します。
  例: `add(a, b)` => `a *^* (add, b)`, `not(b)` => `b *^ not`.
2. macro-base: Scala 2.11 マクロ向けの macro bundle ル群です。
3. byname macro: implicit macro を使って by-name 引数の AST を書き換えます。
  例えば、 `def byname(t: => A)(implicit rewritten: Hoge[A]) { ...; rewritten.hoge; ... }` においては、
  `rewritten` には `t` の AST を書き換えた結果が入ります.
4. cps macro: Fillinski の representing monad (reify/reflect) を直接提供します。
  Shift/reset (answer type modification 有効版、無効版のどちらも) 使えます。
  (ただし、CPS プラグインよりも型アノテーションが多少多く必要になります)
  他の CPS 変換を提供するマクロライブラリ (例えば、 Effectfully, ScalaAsync, Scala Workflow など) と違い、
  byname macro を使うことにより、ライブラリ開発者がマクロキーワードを隠すことができます。
