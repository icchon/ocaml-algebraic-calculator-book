let () =
  let diff_int = Expr.postfix_unop "'" Integer.differentiate in
  Expr.put (Expr.unop_to_string (module Expr.Integer) diff_int 5);

  let diff_rat = Expr.postfix_unop "'" Rational.differentiate in
  let half : Rational.t = Rational.div (Rational.of_integer 1) (Rational.of_integer 2) in
  Expr.put (Expr.unop_to_string (module Expr.Rational) diff_rat half);

  let diff_complex = Expr.postfix_unop "'" Complex.differentiate in
  let c : Complex.t = (Rational.of_integer 3, Rational.of_integer 4) in
  Expr.put (Expr.unop_to_string (module Expr.Complex) diff_complex c);

  let module Poly = Polynomial.Make (Rational) in
  let diff_poly = Expr.postfix_unop "'" Poly.differentiate in
  let p : Poly.t =
    [Rational.of_integer 1; Rational.of_integer 2; Rational.of_integer 3]
  in
  Expr.put (Expr.unop_to_string (module Expr.Q_poly) diff_poly p);

  let diff_k = Expr.postfix_unop "'" Root2_field.K.differentiate in
  let k : Root2_field.K.t =
    [Rational.of_integer 2; Rational.of_integer 3]
  in
  Expr.put (Expr.unop_to_string (module Expr.Root2_field) diff_k k)
