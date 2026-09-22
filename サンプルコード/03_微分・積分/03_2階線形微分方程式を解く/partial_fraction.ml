(* 係数の埋め込み *)
let embed (p: Q_poly.t): Quadratic_poly.t = List.map Quadratic.of_rational p

(* 多項式部分の積分 *)
let integrate_poly (s: Q_poly.t): Q_poly.t =
  Rational.zero ::
    List.mapi (fun i c -> Rational.div c (Rational.of_integer (i + 1))) s

(* 1次式 x - a *)
let linear (a: Quadratic.t): Quadratic_poly.t =
  [Quadratic.neg a; Quadratic.one]

(* 単根aの寄与: r(a)/q'(a) を係数とするlog項 *)
let simple_root_term
    (r: Quadratic_poly.t) (q': Quadratic_poly.t) (a: Quadratic.t)
  : Elementary.t =
  let c =
    Quadratic.div (Quadratic_poly.eval r a) (Quadratic_poly.eval q' a)
  in
  Elementary.log_term (Elementary.of_scalar c) a

(* 二重根aの寄与: A*log(x-a) - B/(x-a) *)
let double_root_term
    (r: Quadratic_poly.t) (q: Quadratic_poly.t) (a: Quadratic.t)
  : Elementary.t =
  let lin = linear a in
  let h =
    fst (Quadratic_poly.div_rem (fst (Quadratic_poly.div_rem q lin)) lin)
  in
  let h' = Quadratic_poly.differentiate h in
  let r' = Quadratic_poly.differentiate r in
  let r_a = Quadratic_poly.eval r a in
  let h_a = Quadratic_poly.eval h a in
  let r'_a = Quadratic_poly.eval r' a in
  let h'_a = Quadratic_poly.eval h' a in
  let b = Quadratic.div r_a h_a in
  let a_coeff =
    Quadratic.div
      (Quadratic.sub (Quadratic.mul r'_a h_a) (Quadratic.mul r_a h'_a))
      (Quadratic.mul h_a h_a)
  in
  let log_part = Elementary.log_term (Elementary.of_scalar a_coeff) a in
  let pole =
    Elementary.QF.div
      (Elementary.of_scalar (Quadratic.neg b)) (Elementary.of_poly lin)
  in
  Elementary.add log_part (Elementary.of_rational pole)

(* 分母qの次数から、直接すべての根を求める *)
let roots_of (q: Q_poly.t): (Quadratic.t * int) list =
  match Q_poly.degree q with
  | d when d <= 0 -> []
  | 1 ->
    let c0 = List.nth q 0 in
    let c1 = List.nth q 1 in
    [ (Quadratic.of_rational (Rational.div (Rational.neg c0) c1), 1) ]
  | 2 ->
    let c0 = List.nth q 0 in
    let c1 = List.nth q 1 in
    let c2 = List.nth q 2 in
    let (x1, x2) = Quadratic.solve c2 c1 c0 in
    if Quadratic.equal x1 x2 then [ (x1, 2) ] else [ (x1, 1); (x2, 1) ]
  | _ ->
    failwith "partial_fraction.integrate: denominator has degree >= 3"

(* 積分 *)
let integrate (p: Q_poly.t) (q: Q_poly.t): Elementary.t =
  let (s, r_rem) = Q_poly.div_rem p q in
  let all_roots = roots_of q in
  let q_c = embed q in
  let q'_c = embed (Q_poly.differentiate q) in
  let r_c = embed r_rem in
  List.fold_left (fun acc (root, mult) ->
    let term =
      match mult with
      | 1 -> simple_root_term r_c q'_c root
      | 2 -> double_root_term r_c q_c root
      | _ ->
        failwith
          ("partial_fraction.integrate: a root with multiplicity >= 3 " ^
           "is unsupported")
    in
    Elementary.add acc term
  ) (Elementary.of_rational (Elementary.of_poly (embed (integrate_poly s))))
    all_roots
