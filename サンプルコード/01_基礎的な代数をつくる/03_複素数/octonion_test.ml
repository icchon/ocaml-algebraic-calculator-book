let complex_i : Complex.t = (Rational.zero, Rational.one)
let qi : Quaternion.t = (complex_i, Complex.zero)
let qj : Quaternion.t = (Complex.zero, Complex.one)

let e1 : Octonion.t = (qi, Quaternion.zero)
let e2 : Octonion.t = (qj, Quaternion.zero)
let e4 : Octonion.t = (Quaternion.zero, Quaternion.one)

let () =
  Expr.put (Expr.join [
    Printf.sprintf "e_1 = %s" (Expr.to_string_of (module Expr.Octonion) e1);
    Printf.sprintf "e_2 = %s" (Expr.to_string_of (module Expr.Octonion) e2);
    Printf.sprintf "e_4 = %s" (Expr.to_string_of (module Expr.Octonion) e4);
  ]);
  let lhs1 =
    let inner = Expr.cdot_join ["e_1"; "e_2"] in
    Expr.cdot_join [Printf.sprintf "(%s)" inner; "e_4"]
  in
  Expr.put (Printf.sprintf "%s = %s" lhs1
    (Octonion.mul (Octonion.mul e1 e2) e4
     |> Expr.to_string_of (module Expr.Octonion)));
  let lhs2 =
    let inner = Expr.cdot_join ["e_2"; "e_4"] in
    Expr.cdot_join ["e_1"; Printf.sprintf "(%s)" inner]
  in
  Expr.put (Printf.sprintf "%s = %s" lhs2
    (Octonion.mul e1 (Octonion.mul e2 e4)
     |> Expr.to_string_of (module Expr.Octonion)))
