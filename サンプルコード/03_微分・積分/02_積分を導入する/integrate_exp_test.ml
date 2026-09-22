module Elementary_expr = Expr.Elementary.Make (Q_rootp) (Quadratic) (Quadratic_poly) (Elementary)

let check name a p =
  let r = Elementary.integrate_exp a p in
  print_endline ("--- " ^ name ^ " ---");
  let integrand = Expr.product [
    Expr.Q_poly.to_expr p;
    Expr.pow (Expr.atom "e") (Expr.product [Expr.Rational.to_expr a; Expr.atom "x"]);
  ] in
  Expr.put (Expr.integral_to_string "x" integrand
    (Expr.to_string_of (module Elementary_expr) r));
  (* 検算: 微分すると元の P(x)e^{ax} に戻るはず *)
  let deriv = Elementary.differentiate r in
  let target = Elementary.exp_term (Elementary.of_poly (List.map Quadratic.of_rational p)) (Quadratic.of_rational a) in
  let diff = Elementary.add deriv (Elementary.scale (Quadratic.neg Quadratic.one) target) in
  Expr.put (Expr.call "differentiate_check" [] (Expr.to_string_of (module Elementary_expr) diff))

let () =
  (* ∫ x e^x dx = (x-1) e^x *)
  check "x e^x" (1, 1) [Rational.of_integer 0; Rational.of_integer 1];

  (* ∫ e^{2x} dx = (1/2) e^{2x} *)
  check "e^{2x}" (2, 1) [Rational.of_integer 1];

  (* ∫ (x^2 - 1) e^{-x} dx *)
  check "(x^2-1) e^{-x}" (-1, 1)
    [Rational.of_integer (-1); Rational.of_integer 0; Rational.of_integer 1]
