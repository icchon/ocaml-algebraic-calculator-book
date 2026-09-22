module P = Polynomial.Make(Rational)

let () =
  let p1 : P.t =
    [Rational.of_integer 1; Rational.of_integer 2; Rational.of_integer 3]
  in
  let p2 : P.t = [Rational.of_integer 1; Rational.of_integer 1] in
  let p1_str = Expr.to_string_of (module Expr.Q_poly) p1 in
  let p2_str = Expr.to_string_of (module Expr.Q_poly) p2 in

  Expr.put (Expr.join [
    Printf.sprintf "p_1 = %s" p1_str;
    Printf.sprintf "p_2 = %s" p2_str;
  ]);
  Expr.put (Expr.biop_to_string (module Expr.Q_poly)
    (Expr.infix_biop "+" P.add) p1 p2);
  Expr.put (Expr.biop_to_string (module Expr.Q_poly)
    (Expr.cdot_biop P.mul) p1 p2);

  let (q, r) = P.div_rem p1 p2 in
  let q_str = Expr.to_string_of (module Expr.Q_poly) q in
  let r_str = Expr.to_string_of (module Expr.Q_poly) r in
  Expr.put (Expr.call "div_rem" [p1_str; p2_str] (Expr.tuple [q_str; r_str]));

  Expr.put (Expr.poly_eval_to_string (module Expr.Q_poly) (module Expr.Rational)
    P.eval p1 (Rational.of_integer 2))
