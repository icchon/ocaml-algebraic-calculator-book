include Polynomial.Make(Rational)

(* 整数係数への変換 *)
let to_integer_poly (p : t) : Integer.t list * Integer.t * Integer.t =
  let denoms = List.map snd p in
  let common_denom = List.fold_left Integer.lcm Integer.one denoms in
  let int_coeffs =
    List.map (fun (num, den) ->
      Integer.div_rem (Integer.mul num common_denom) den |> fst
    ) p
  in
  let a0 = List.hd int_coeffs in
  let an = List.hd (List.rev int_coeffs) in
  (int_coeffs, a0, an)

(* 約数の列挙 *)
let divisors (n : Integer.t) : Integer.t list =
  let abs_n = Integer.abs n in
  if Integer.equal abs_n Integer.zero then []
  else
    let rec aux curr acc =
      if Integer.compare (Integer.mul curr curr) abs_n > 0 then acc
      else
        let acc' =
          if Integer.equal (Integer.div_rem abs_n curr |> snd) Integer.zero
          then
            let div = Integer.div_rem abs_n curr |> fst in
            if Integer.equal curr div then
              curr :: acc
            else
              curr :: div :: acc
          else acc
        in
        aux (Integer.add curr Integer.one) acc'
    in
    let pos_divs = aux Integer.one [] in
    List.concat_map (fun d -> [d; Integer.neg d]) pos_divs

(* 求解 *)
let rec solve_equation (p : t) : Rational.t list =
  if degree p < 1 then []
  else
    let (_, a0, an) = to_integer_poly p in
    if Integer.equal a0 Integer.zero then
      Rational.zero ::
        solve_equation (div_rem p [Rational.zero; Rational.one] |> fst)
    else
      let p_divs = divisors a0 in
      let q_divs =
        divisors an
        |> List.filter (fun d -> Integer.compare d Integer.zero > 0)
      in
      let candidates =
        List.concat_map (fun q ->
          List.map (fun p_val -> (p_val, q)) p_divs
        ) q_divs
        |> List.sort_uniq Rational.compare
      in
      List.filter
        (fun c -> Rational.equal (eval p c) Rational.zero) candidates
