let vec_to_string (v: Rational_vector.t): string =
  v |> List.map (Expr.to_string_of (module Expr.Rational)) |> Expr.tuple

let () =
  let a : Rational_vector.t = List.map Rational.of_integer [1; 2; 3] in
  let b : Rational_vector.t = List.map Rational.of_integer [4; 5; 6] in
  let s = Rational.of_integer 2 in

  Expr.put (Printf.sprintf "%s + %s = %s"
    (vec_to_string a) (vec_to_string b) (vec_to_string (Rational_vector.add a b)));
  Expr.put (Printf.sprintf "%s \\cdot %s = %s"
    (Expr.to_string_of (module Expr.Rational) s) (vec_to_string a)
    (vec_to_string (Rational_vector.mul_scalar s a)))
