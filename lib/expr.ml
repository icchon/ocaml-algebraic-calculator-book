(* このファイルはエイリアスのみで構成する。本文・テストコードは今までどおり
   Expr.put・Expr.Integer・(module Expr.Rational) のように参照できるが、
   実体はexpr_core.ml・expr_integer.mlのように代数ごとに分かれた別々の
   コンパイル単位になっている。-no-alias-depsでコンパイルすることで、
   「Expr.Quadraticを使わないプログラムは、expr_quadratic.mlが必要とする
   実体のQ_rootpモジュールさえリンク時に要求されない」という、単位ごとの
   選択的リンクが効くようにしてある（詳しくはREADMEの「to_tex.cmxaの設計」を
   参照） *)
include Expr_core
module Integer = Expr_integer
module Rational = Expr_rational
module Complex = Expr_complex
module Quaternion = Expr_quaternion
module Octonion = Expr_octonion
module Hexadecanion = Expr_hexadecanion
module Q_poly = Expr_q_poly
module Root2_field = Expr_root2_field
module Q_rootp = Expr_q_rootp
module Quadratic = Expr_quadratic
module Quadratic_poly = Expr_quadratic_poly
module Omega_field = Expr_omega_field
module Cubic = Expr_cubic
module Elementary = Expr_elementary
