(* Cubic.Make(F).t は、Fが何であってもOmega_field.t listという同じ形に
   固定されているので、専用のコンバータは1つあれば足りる *)
module Make (Q: Expr_q_rootp.Q_ROOTP_LIKE) =
  Expr_core.Make_poly_expr (Expr_omega_field.Make (Q)) (struct let name = "v" end)
