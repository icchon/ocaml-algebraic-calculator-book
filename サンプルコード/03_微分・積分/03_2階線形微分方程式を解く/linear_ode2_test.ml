module Elementary_expr = Expr.Elementary.Make (Q_rootp) (Quadratic) (Quadratic_poly) (Elementary)

(* y''+by'+cy=0という式そのものをExpr.tで組み立てて表示する *)
let ode_str b c =
  let ode = Expr.sum [
    Expr.atom "y''";
    Expr.product [Expr.Rational.to_expr b; Expr.atom "y'"];
    Expr.product [Expr.Rational.to_expr c; Expr.atom "y"];
  ] in
  Printf.sprintf "%s = 0" (Expr.to_string ode)

(* y''+by'+cyにyを代入した式を、項ごとに計算してからExpr.tで組み立てて表示する *)
let check_str b c y =
  let y' = Elementary.differentiate y in
  let y'' = Elementary.differentiate y' in
  let by' = Elementary.scale (Quadratic.of_rational b) y' in
  let cy = Elementary.scale (Quadratic.of_rational c) y in
  let lhs = Expr.sum [
    Elementary_expr.to_expr y'';
    Elementary_expr.to_expr by';
    Elementary_expr.to_expr cy;
  ] in
  Printf.sprintf "%s = %s" (Expr.to_string lhs)
    (Expr.to_string_of (module Elementary_expr) (Linear_ode2.check b c y))

let show b c =
  let (y1, y2) = Linear_ode2.solve b c in
  Expr.put (ode_str b c);
  Expr.put (Expr.join [
    Printf.sprintf "y_1 = %s" (Expr.to_string_of (module Elementary_expr) y1);
    Printf.sprintf "y_2 = %s" (Expr.to_string_of (module Elementary_expr) y2);
  ]);
  Expr.put (Expr.join [check_str b c y1; check_str b c y2])

let () =
  (* 判別式が正: y'' - 3y' + 2y = 0, r = 1, 2 *)
  show (-3, 1) (2, 1);

  (* 判別式が負: y'' + y = 0, r = i, -i *)
  show (0, 1) (1, 1)
