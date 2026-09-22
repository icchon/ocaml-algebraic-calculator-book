(* Root2_field.K・Root5_fieldなど、Field_extension.Makeの出力はどれも
   coeff listという同じ形（t = I.P.t = C.t list）なので、Make_poly_exprを
   基底元の名前だけ変えて使い回せる。Root2_field.K専用にこの名前で
   用意しておく *)
include Expr_core.Make_poly_expr (Expr_rational) (struct let name = "\\sqrt{2}" end)
