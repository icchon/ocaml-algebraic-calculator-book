let () =
  print_endline "--- Integer: 5 ---";
  Expr.put (Expr.to_string_of (module Expr.Integer) (Integer.differentiate 5));

  print_endline "--- Rational: 1/2 ---";
  let half : Rational.t = Rational.div (Rational.of_integer 1) (Rational.of_integer 2) in
  Expr.put (Expr.to_string_of (module Expr.Rational) (Rational.differentiate half));

  print_endline "--- Complex: 3 + 4i ---";
  let c : Complex.t = (Rational.of_integer 3, Rational.of_integer 4) in
  Expr.put (Expr.to_string_of (module Expr.Complex) (Complex.differentiate c));

  print_endline "--- Polynomial.Make(Rational): 1 + 2x + 3x^2 ---";
  let module Poly = Polynomial.Make (Rational) in
  let p : Poly.t =
    [Rational.of_integer 1; Rational.of_integer 2; Rational.of_integer 3]
  in
  Expr.put (Expr.to_string_of (module Expr.Q_poly) (Poly.differentiate p));

  print_endline "--- K(root 2): 2 + 3 root2 ---";
  let k : Root2_field.K.t =
    [Rational.of_integer 2; Rational.of_integer 3]
  in
  Expr.put (Expr.to_string_of (module Expr.Root2_field) (Root2_field.K.differentiate k))
