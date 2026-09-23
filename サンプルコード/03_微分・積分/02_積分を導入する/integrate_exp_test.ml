module Elementary_expr = Expr.Elementary.Make (Q_rootp) (Quadratic) (Quadratic_poly) (Elementary)

let check a p =
  let r = Elementary.integrate_exp a p in
  let integrand = Expr.product [
    Expr.Q_poly.to_expr p;
    Expr.pow (Expr.atom "e") (Expr.product [Expr.Rational.to_expr a; Expr.atom "x"]);
  ] in
  Expr.put (Expr.integral_to_string "x" integrand
    (Expr.to_string_of (module Elementary_expr) r));
  (* 検算: 積分結果を微分すると、元の被積分関数に戻るはず *)
  let diff = Expr.postfix_unop "'" Elementary.differentiate in
  Expr.put (Expr.unop_to_string (module Elementary_expr) diff r)

let () =
  (* ∫ x e^x dx = (x-1) e^x *)
  check (1, 1) [Rational.of_integer 0; Rational.of_integer 1];

  (* ∫ e^{2x} dx = (1/2) e^{2x} *)
  check (2, 1) [Rational.of_integer 1];

  (* ∫ (x^2 - 1) e^{-x} dx *)
  check (-1, 1)
    [Rational.of_integer (-1); Rational.of_integer 0; Rational.of_integer 1]
