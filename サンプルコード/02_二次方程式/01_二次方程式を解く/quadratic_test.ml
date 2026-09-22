module P = Polynomial.Make(Quadratic)
module Quadratic_expr = Expr.Quadratic.Make (Q_rootp)
module Quadratic_poly_expr = Expr.Quadratic_poly.Make (Q_rootp)

let run_example (a, b, c) =
  let poly : Rational.t list = [c; b; a] in
  let solve (_ : Rational.t list) =
    let (x1, x2) = Quadratic.solve a b c in
    [x1; x2]
  in
  Expr.put (Expr.solve_to_string
    (module Expr.Q_poly) (module Quadratic_expr) solve poly);
  let (x1, x2) = Quadratic.solve a b c in
  let check_poly : Quadratic_poly.t =
    [Quadratic.of_rational c;
     Quadratic.of_rational b;
     Quadratic.of_rational a]
  in
  Expr.put (Expr.poly_eval_to_string
    (module Quadratic_poly_expr) (module Quadratic_expr)
    P.eval check_poly x1);
  Expr.put (Expr.poly_eval_to_string
    (module Quadratic_poly_expr) (module Quadratic_expr)
    P.eval check_poly x2)

let () =
  run_example ((1, 1), (-5, 1), (6, 1));
  run_example ((1, 1), (-2, 1), (-1, 1));
  run_example ((1, 1), (1, 1), (1, 1))
