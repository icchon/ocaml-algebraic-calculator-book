module OmegaPoly = struct
  module C = Q_rootp
  module P = Polynomial.Make(C)
  let poly = [C.one; C.one; C.one]
end

include Field_extension.Make(OmegaPoly)

(* 1の原始三乗根ω *)
let omega: t = [Q_rootp.zero; Q_rootp.one]

(* 係数からの構成 *)
let of_ab (a : Q_rootp.t) (b : Q_rootp.t) : t =
  [a; b]

(* 判別式の平方根 *)
let sqrt_disc (disc : Rational.t) : t =
    let num, den = disc in
    if Integer.equal num Integer.zero then
      zero
    else if Integer.compare num Integer.zero > 0 then
      let root_d = Q_rootp.of_rational_sqrt disc in
      of_ab root_d Q_rootp.zero
    else
      let abs_disc = (Integer.abs num, den) in
      let abs_disc_over_3 = Rational.div abs_disc (Rational.of_integer 3) in
      let k = Q_rootp.of_rational_sqrt abs_disc_over_3 in
      let a0 = k in
      let a1 = Q_rootp.mul (Q_rootp.of_rational (2, 1)) k in
      of_ab a0 a1

(* 有理数の埋め込み *)
let of_rational (x: Rational.t): t =
  of_ab (Q_rootp.of_rational x) Q_rootp.zero

(* 係数の取り出し *)
let to_ab (x : t) : Q_rootp.t * Q_rootp.t =
  match x with
  | [] -> (Q_rootp.zero, Q_rootp.zero)
  | [a] -> (a, Q_rootp.zero)
  | [a; b] -> (a, b)
  | _ -> failwith "OmegaField: degree > 1"

(* 立方根 *)
let cbrt (x : t) : t option =
  if equal x zero then Some zero
  else
    let a_elem, b_elem = to_ab x in
    match Q_rootp.to_rational a_elem, Q_rootp.to_rational b_elem with
    | Some a, Some b -> (
      if Rational.equal b Rational.zero then
        match Q_rootp.cbrt a_elem with
          | Some res_a -> Some (of_ab res_a Q_rootp.zero)
          | None -> None
      else
        (* ノルム N(x) = a^2 - ab + b^2 *)
        let a2 = Rational.mul a a in
        let ab = Rational.mul a b in
        let b2 = Rational.mul b b in
        let norm = Rational.add (Rational.sub a2 ab) b2 in

        match Rational.root 3 norm with
        | None -> None
        | Some n_y ->
            (* cの方程式: 4c^3 - 3c^2 - 3(N_y - 1)c - (2a - b) = 0 *)
            let two_a = Rational.add a a in
            let const_term = Rational.sub two_a b in
            let n_y_minus_1 = Rational.sub n_y Rational.one in
            let coeff_1 =
              Rational.mul (Rational.of_integer (-3)) n_y_minus_1
            in

            let poly_c = [
              Rational.neg const_term;
              coeff_1;
              Rational.of_integer (-3);
              Rational.of_integer 4
            ] in

            let c_candidates = Q_poly.solve_equation poly_c in

            (* dの復元と検算 *)
            let try_construct q_c =
              let q_c2 = Rational.mul q_c q_c in
              let four_ny = Rational.mul (Rational.of_integer 4) n_y in
              let three_c2 = Rational.mul (Rational.of_integer 3) q_c2 in
              let disc_d = Rational.sub four_ny three_c2 in

              match Rational.root 2 disc_d with
              | None -> None
              | Some sqrt_disc ->
                  let two = Rational.of_integer 2 in
                  let d1 = Rational.div (Rational.add q_c sqrt_disc) two in
                  let d2 = Rational.div (Rational.sub q_c sqrt_disc) two in

                  let check_d q_d =
                    let c_elem = Q_rootp.of_rational q_c in
                    let d_elem = Q_rootp.of_rational q_d in
                    let candidate_y = of_ab c_elem d_elem in
                    let y3 = mul (mul candidate_y candidate_y) candidate_y in
                    if equal y3 x then Some candidate_y else None
                  in

                  (match check_d d1 with
                    | Some res -> Some res
                    | None -> check_d d2)
            in
            List.find_map try_construct c_candidates
      )
    | _ -> None
