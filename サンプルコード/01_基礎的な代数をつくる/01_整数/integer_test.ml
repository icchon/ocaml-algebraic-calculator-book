let () =
  let add = Expr.infix_biop "+" Integer.add in
  Expr.put (Expr.biop_to_string (module Expr.Integer) add 2 3);
  Expr.put (Expr.biop_to_string (module Expr.Integer) add 2 (-3));
  let mul = Expr.cdot_biop Integer.mul in
  Expr.put (Expr.biop_to_string (module Expr.Integer) mul 2 3);
  Expr.put (Expr.biop_to_string (module Expr.Integer) mul 2 (-3));
  let gcd = Expr.call_biop "gcd" Integer.gcd in
  Expr.put (Expr.biop_to_string (module Expr.Integer) gcd 6 2);
  Expr.put (Expr.biop_to_string (module Expr.Integer) gcd 11 4);
  let pow4 = Expr.pow_unop 4 (Integer.pow 4) in
  Expr.put (Expr.unop_to_string (module Expr.Integer) pow4 4);
  Expr.put (Expr.unop_to_string (module Expr.Integer) pow4 0);
