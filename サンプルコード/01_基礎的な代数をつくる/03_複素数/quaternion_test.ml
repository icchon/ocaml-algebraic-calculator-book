let complex_i : Complex.t = (Rational.zero, Rational.one)
let i : Quaternion.t = (complex_i, Complex.zero)
let j : Quaternion.t = (Complex.zero, Complex.one)

let () =
  Expr.put (Expr.join [
    Printf.sprintf "i = %s" (Expr.to_string_of (module Expr.Quaternion) i);
    Printf.sprintf "j = %s" (Expr.to_string_of (module Expr.Quaternion) j);
  ]);
  let mul = Expr.cdot_biop Quaternion.mul in
  Expr.put (Expr.biop_to_string (module Expr.Quaternion) mul i j);
  Expr.put (Expr.biop_to_string (module Expr.Quaternion) mul j i)
