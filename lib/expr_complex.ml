(* Complex.t = Complexical.Make(Rational).t = Rational.t * Rational.t という
   形が分かっていればよいので、実体のComplexモジュールは参照しない。
   Make_paired_exprにExpr_rationalを渡すだけで、同じ形の変換が手に入る *)
include Expr_core.Make_paired_expr (Expr_rational) (struct let symbol = "i" end)
