let solve_and_verify (poly : Q_poly.t) =
  let res = Cardano_solver.solve poly in
  let module R = (val res : Cardano_solver.ROOT_SET) in
  let module P = Polynomial.Make(R.C) in
  let poly_c = List.map R.embed poly in
  let module Root_expr = struct
    type t = R.C.t
    let to_expr = R.to_expr
  end in
  let module Root_poly_expr =
    Expr.Make_poly_expr (Root_expr) (struct let name = "x" end)
  in
  let solve (_: Q_poly.t): R.C.t list = R.roots in
  Expr.put (Expr.solve_to_string
    (module Expr.Q_poly) (module Root_expr) solve poly);
  R.roots |> List.iter (fun root ->
    Expr.put (Expr.poly_eval_to_string
      (module Root_poly_expr) (module Root_expr) P.eval poly_c root))

let () =
  solve_and_verify [(-9, 1); (-6, 1); (0, 1); (1, 1)];
  solve_and_verify [(1, 1); (-3, 1); (0, 1); (1, 1)]
