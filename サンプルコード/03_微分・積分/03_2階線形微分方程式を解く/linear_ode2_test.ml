module Elementary_expr = Expr.Elementary.Make (Q_rootp) (Quadratic) (Quadratic_poly) (Elementary)

let show name b c =
  let (y1, y2) = Linear_ode2.solve b c in
  print_endline ("--- " ^ name ^ " ---");
  Expr.put (Printf.sprintf "y'' + (%s) y' + (%s) y = 0"
    (Expr.to_string_of (module Expr.Rational) b)
    (Expr.to_string_of (module Expr.Rational) c));
  Expr.put (Expr.join [
    Printf.sprintf "y_1 = %s" (Expr.to_string_of (module Elementary_expr) y1);
    Printf.sprintf "y_2 = %s" (Expr.to_string_of (module Elementary_expr) y2);
  ]);
  let check = Expr.call_unop "check" (Linear_ode2.check b c) in
  Expr.put (Expr.join [
    Expr.unop_to_string (module Elementary_expr) check y1;
    Expr.unop_to_string (module Elementary_expr) check y2;
  ])

let () =
  (* 判別式が正: y'' - 3y' + 2y = 0, r = 1, 2 *)
  show "distinct real roots" (-3, 1) (2, 1);

  (* 判別式が負: y'' + y = 0, r = i, -i *)
  show "complex roots" (0, 1) (1, 1)
