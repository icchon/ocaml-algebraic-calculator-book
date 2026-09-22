module Elementary_expr = Expr.Elementary.Make (Q_rootp) (Quadratic) (Quadratic_poly) (Elementary)

let check name p a =
  let r = Elementary.integrate_log p a in
  print_endline ("--- " ^ name ^ " ---");
  let integrand = Expr.product [
    Expr.Q_poly.to_expr p;
    Expr.fn "log" [Expr.sum [Expr.atom "x"; Expr.neg (Expr.Rational.to_expr a)]];
  ] in
  Expr.put (Expr.integral_to_string "x" integrand
    (Expr.to_string_of (module Elementary_expr) r));
  (* 検算: 微分すると元の p(x)*log(x-a) に戻るはず *)
  let deriv = Elementary.differentiate r in
  let target = Elementary.log_term (Elementary.of_poly (List.map Quadratic.of_rational p)) (Quadratic.of_rational a) in
  let diff = Elementary.add deriv (Elementary.neg target) in
  Expr.put (Expr.call "differentiate_check" [] (Expr.to_string_of (module Elementary_expr) diff))

let () =
  (* log x (係数1) *)
  check "log x" [Rational.one] (0, 1);

  (* 3 log(x-2) (定数係数) *)
  check "3 log(x-2)" [Rational.of_integer 3] (2, 1);

  (* x^2 log(x-1) (多項式係数) *)
  check "x^2 log(x-1)" [Rational.zero; Rational.zero; Rational.one] (1, 1)
