module Elementary_expr = Expr.Elementary.Make (Q_rootp) (Quadratic) (Quadratic_poly) (Elementary)

let check p a =
  let r = Elementary.integrate_log p a in
  let integrand = Expr.product [
    Expr.Q_poly.to_expr p;
    Expr.fn "log" [Expr.sum [Expr.atom "x"; Expr.neg (Expr.Rational.to_expr a)]];
  ] in
  Expr.put (Expr.integral_to_string "x" integrand
    (Expr.to_string_of (module Elementary_expr) r));
  (* 検算: 積分結果を微分すると、元の被積分関数に戻るはず *)
  let diff = Expr.postfix_unop "'" Elementary.differentiate in
  Expr.put (Expr.unop_to_string (module Elementary_expr) diff r)

let () =
  (* log x (係数1) *)
  check [Rational.one] (0, 1);

  (* 3 log(x-2) (定数係数) *)
  check [Rational.of_integer 3] (2, 1);

  (* x^2 log(x-1) (多項式係数) *)
  check [Rational.zero; Rational.zero; Rational.one] (1, 1);

  (* (x+2) log(x+1) (多項式係数、1次) *)
  check [Rational.of_integer 2; Rational.one] (-1, 1)
