let () =
  let add = Expr.infix_biop "+" Rational.add in
  Expr.put (Expr.biop_to_string (module Expr.Rational) add (1, 2) (1, 3));
  Expr.put (Expr.biop_to_string (module Expr.Rational) add (1, 2) (-1, 3));
  let mul = Expr.cdot_biop Rational.mul in
  Expr.put (Expr.biop_to_string (module Expr.Rational) mul (1, 2) (1, 3));
  Expr.put (Expr.biop_to_string (module Expr.Rational) mul (1, 2) (-1, 3));
  let div = Expr.infix_biop "\\div" Rational.div in
  Expr.put (Expr.biop_to_string (module Expr.Rational) div (1, 2) (1, 3));
  let pow4 = Expr.pow_unop (-4) (Rational.pow (-4)) in
  Expr.put (Expr.unop_to_string (module Expr.Rational) pow4 (1, 2))
