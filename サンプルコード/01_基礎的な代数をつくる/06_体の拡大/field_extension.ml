module type IrreduciblePoly = sig
  module C : Field.S
  module P : Polynomial.S with type coeff = C.t
  val poly : P.t
end

module type FUNCTOR_TYPE =
  functor (I: IrreduciblePoly) -> (Field.S with type t = I.P.t)

module Make: FUNCTOR_TYPE = functor (I: IrreduciblePoly) -> struct
  include I.P
  (* 正規化 *)
  let normalize (p: t): t =
    div_rem p I.poly |> snd
  (* 等価判定 *)
  let equal (x: t) (y: t): bool =
    equal (normalize x) (normalize y)
  (* 加法 *)
  let add (x: t) (y: t): t =
    add x y |> normalize
  (* 符号反転 *)
  let neg (x: t): t =
    neg x |> normalize
  (* 乗法 *)
  let mul (x: t) (y: t): t   =
    mul x y |> normalize
  (* 逆元 *)
  let inv (x: t): t =
    let x_norm = normalize x in
    if equal x_norm I.P.zero then
      failwith "Division by zero in extension field"
    else
      let (g, u, _) = ext_gcd x_norm I.poly in
      let lc = I.P.leading_coeff g in
      let u_monic = div_scalar lc u in
      normalize u_monic

  include Add_group.Extend(struct
      type nonrec t = t
      let add, neg = add, neg
    end)
  include Mul_monoid.Extend(struct
    type nonrec t = t
    let one, mul = one, mul
  end)
  include Mul_group.Extend(struct
    type nonrec t = t
    let one, mul, inv = one, mul, inv
  end)
end
