let complex_i : Complex.t = (Rational.zero, Rational.one)
let qi : Quaternion.t = (complex_i, Complex.zero)

let oe1 : Octonion.t = (qi, Quaternion.zero)
let oe4 : Octonion.t = (Quaternion.zero, Quaternion.one)

let e1 : Hexadecanion.t = (oe1, Octonion.zero)
let e4 : Hexadecanion.t = (oe4, Octonion.zero)

let a : Hexadecanion.t = Hexadecanion.add e1 e4
let b : Hexadecanion.t = Hexadecanion.sub e1 e4

let () =
  let a_str = Expr.to_string_of (module Expr.Hexadecanion) a in
  let b_str = Expr.to_string_of (module Expr.Hexadecanion) b in
  Expr.put (Printf.sprintf "a = e_1 + e_4 = %s" a_str);
  Expr.put (Printf.sprintf "b = e_1 - e_4 = %s" b_str);
  let mul = Expr.cdot_biop Hexadecanion.mul in
  Expr.put (Expr.biop_to_string (module Expr.Hexadecanion) mul a b)
