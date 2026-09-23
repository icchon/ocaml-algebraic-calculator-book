(* 求解 *)
let solve (b: Rational.t) (c: Rational.t): Elementary.t * Elementary.t =
  let (r1, r2) = Quadratic.solve Rational.one b c in
  (Elementary.exp_term (Elementary.of_scalar Quadratic.one) r1,
   Elementary.exp_term (Elementary.of_scalar Quadratic.one) r2)

(* 検算 *)
let check
    (b: Rational.t) (c: Rational.t) (y: Elementary.t): Elementary.t =
  let y' = Elementary.differentiate y in
  let y'' = Elementary.differentiate y' in
  Elementary.add y''
    (Elementary.add
       (Elementary.scale (Quadratic.of_rational b) y')
       (Elementary.scale (Quadratic.of_rational c) y))
