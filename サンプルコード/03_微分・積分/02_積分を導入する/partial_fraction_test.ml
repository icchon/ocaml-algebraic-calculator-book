module Elementary_expr = Expr.Elementary.Make (Q_rootp) (Quadratic) (Quadratic_poly) (Elementary)

let show p q =
  (try
     let r = Elementary.integrate_rational p q in
     let integrand = Expr.frac (Expr.Q_poly.to_expr p) (Expr.Q_poly.to_expr q) in
     Expr.put (Expr.integral_to_string "x" integrand
       (Expr.to_string_of (module Elementary_expr) r))
   with Failure msg -> print_endline ("failwith: " ^ msg))

let () =
  (* 実根のみ: 1/((x-1)(x-2)) = 1/(x-2) - 1/(x-1) *)
  show [Rational.of_integer 1] [(2,1); (-3,1); (1,1)];

  (* 既約2次（複素根）: 1/(x^2+1) *)
  show [Rational.of_integer 1] [(1,1); (0,1); (1,1)];

  (* 3次以上が残る: 1/(x^3+x+1) （有理根なし、既約3次） *)
  show [Rational.of_integer 1] [(1,1); (1,1); (0,1); (1,1)];

  (* 重根: 1/(x-1)^2 = 1/(x^2 - 2x + 1) *)
  show [Rational.of_integer 1] [(1,1); (-2,1); (1,1)]
