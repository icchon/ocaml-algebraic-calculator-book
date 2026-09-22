module Make (Q: Expr_q_rootp.Q_ROOTP_LIKE) =
  Expr_core.Make_poly_expr (Expr_q_rootp.Make (Q)) (struct let name = "{\\omega}" end)
