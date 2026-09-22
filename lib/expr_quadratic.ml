(* Quadratic = 複素構造ファンクタ（Complexical.Make）をQ_rootpに適用した
   ものなので、Complexと同じMake_paired_exprがそのまま使い回せる。
   Expr_q_rootpがファンクタになったのにあわせて、こちらもファンクタにする。
   実体のQuadraticモジュールは参照しない *)
module Make (Q: Expr_q_rootp.Q_ROOTP_LIKE) =
  Expr_core.Make_paired_expr (Expr_q_rootp.Make (Q)) (struct let symbol = "i" end)
