(* Q_poly.t = Polynomial.Make(Rational).t = Rational.t list という形が
   分かっていればよいので、実体のQ_polyモジュールは参照しない *)
include Expr_core.Make_poly_expr (Expr_rational) (struct let name = "x" end)
