module type Ring = sig
  include Ring.S
  val compare: t -> t -> int
  val gcd: t -> t -> t
  val div_rem: t -> t -> t * t
end

module type FUNCTOR_TYPE = functor (R: Ring) ->
  (sig
     include Field.S
     val compare: t -> t -> int
   end with type t = R.t * R.t)

module Make: FUNCTOR_TYPE = functor (R: Ring) -> struct
  type t = R.t * R.t
  let one = (R.one, R.one)
  let zero = (R.zero, R.one)
  (* 約分と符号の正規化 *)
  let normalize (n, d) =
    let make_positive x =
      if R.compare x R.zero >= 0 then x else R.neg x
    in
    let g = R.gcd (make_positive n) (make_positive d) in
    let n' = R.div_rem n g |> fst in
    let d' = R.div_rem d g |> fst in
    if R.compare d' R.zero < 0 then
      (R.neg n', R.neg d')
    else
      (n', d')
  (* 符号反転 *)
  let neg (n, d) =
    (R.neg n, d) |> normalize
  (* 加法 *)
  let add (n1, d1) (n2, d2) =
    let d = R.mul d1 d2 in
    let n = R.add (R.mul n1 d2) (R.mul n2 d1) in
    (n, d) |> normalize
  (* 乗法 *)
  let mul (n1, d1) (n2, d2) =
    let n = R.mul n1 n2 in
    let d = R.mul d1 d2 in
    (n, d) |> normalize
  (* 逆数 *)
  let inv (n, d) =
    if R.equal n R.zero then
      failwith "Division by zero"
    else
      (d, n) |> normalize
  (* 大小比較 *)
  let compare (n1, d1) (n2, d2) =
    R.compare (R.mul n1 d2) (R.mul n2 d1)
  (* 等価判定 *)
  let equal x1 x2 = compare x1 x2 = 0

  include Mul_monoid.Extend(struct
    type nonrec t = t
    let one, mul, equal = one, mul, equal
  end)
  include Add_group.Extend(struct
    type nonrec t = t
    let add, neg = add, neg
  end)
  include Mul_group.Extend(struct
    type nonrec t = t
    let one, mul, inv = one, mul, inv
  end)
end
