let () =
  let root2 : Root2_field.K.t = [Rational.zero; Rational.one] in
  Expr.put (Expr.to_string_of (module Expr.Root2_field) root2);
  let square = Expr.pow_unop 2 (fun x -> Root2_field.K.mul x x) in
  Expr.put (Expr.unop_to_string (module Expr.Root2_field) square root2);
  let mul = Expr.cdot_biop Root2_field.K.mul in
  let root2_inv = Root2_field.K.inv root2 in
  Expr.put (Expr.biop_to_string (module Expr.Root2_field) mul root2 root2_inv)
