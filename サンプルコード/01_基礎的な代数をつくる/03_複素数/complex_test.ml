let () =
  let x_2 = (2, 1) in
  let x_3 = (3, 1) in
  let x_neg2 = (-2, 1) in

  let add = Expr.infix_biop "+" Complex.add in
  Expr.put (Expr.biop_to_string (module Expr.Complex) add
    (x_2, x_3) (x_3, x_2));
  Expr.put (Expr.biop_to_string (module Expr.Complex) add
    (x_2, x_3) (x_neg2, x_3));
  let mul = Expr.cdot_biop Complex.mul in
  Expr.put (Expr.biop_to_string (module Expr.Complex) mul
    (x_2, x_3) Complex.zero);
  Expr.put (Expr.biop_to_string (module Expr.Complex) mul
    (x_2, x_3) (x_neg2, x_3));
  let div = Expr.infix_biop "\\div" Complex.div in
  Expr.put (Expr.biop_to_string (module Expr.Complex) div
    (x_2, x_3) (x_3, x_2));
  let pow4 = Expr.pow_unop (-4) (Complex.pow (-4)) in
  Expr.put (Expr.unop_to_string (module Expr.Complex) pow4
    (Rational.one, Rational.one))
