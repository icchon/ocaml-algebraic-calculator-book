(* 積分 *)
let integrate (a : Rational.t) (p : Q_poly.t) : Elementary.t =
  if Rational.equal a Rational.zero then
    failwith
      ("integrate_exp.integrate: a must be non-zero " ^
       "(use partial_fraction for a = 0)");
  let n = Q_poly.degree p in
  if n < 0 then Elementary.zero
  else
    let r = Array.make (n + 1) Rational.zero in
    for i = n downto 0 do
      let p_i = List.nth p i in
      let higher =
        if i = n then Rational.zero
        else Rational.mul (Rational.of_integer (i + 1)) r.(i + 1)
      in
      r.(i) <- Rational.div (Rational.sub p_i higher) a
    done;
    let r_poly : Quadratic_poly.t =
      Array.to_list r |> List.map Quadratic.of_rational
    in
    Elementary.exp_term (Elementary.of_poly r_poly) (Quadratic.of_rational a)
