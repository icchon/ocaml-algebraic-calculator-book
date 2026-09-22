
module type Ring = sig
  include Ring.S
  val compare: t -> t -> int
  val gcd: t -> t -> t
  val div_rem: t -> t -> t * t
  val differentiate: t -> t (* 微分・積分の章で追加 *)
end

module type FUNCTOR_TYPE = functor (R: Ring) ->
  (sig
     include Field.S
     val differentiate: t -> t (* 微分・積分の章で追加 *)
     val compare: t -> t -> int
   end with type t = R.t * R.t)

module Make: FUNCTOR_TYPE = functor (R: Ring) -> struct
  type t = R.t * R.t
  let one: t = (R.one, R.one)
  let zero: t = (R.zero, R.one)
  (* 約分と符号の正規化 *)
  let normalize ((n, d): t): t =
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
  let neg ((n, d): t): t =
    (R.neg n, d) |> normalize
  (* 加法 *)
  let add ((n1, d1): t) ((n2, d2): t): t =
    let d = R.mul d1 d2 in
    let n = R.add (R.mul n1 d2) (R.mul n2 d1) in
    (n, d) |> normalize
  (* 乗法 *)
  let mul ((n1, d1): t) ((n2, d2): t): t =
    let n = R.mul n1 n2 in
    let d = R.mul d1 d2 in
    (n, d) |> normalize
  (* 逆数 *)
  let inv ((n, d): t): t =
    if R.equal n R.zero then
      failwith "Division by zero"
    else
      (d, n) |> normalize

  (* 大小比較 *)
  let compare ((n1, d1): t) ((n2, d2): t): int =
    R.compare (R.mul n1 d2) (R.mul d1 n2)
  (* 等価判定 *)
  let equal (x1: t) (x2: t): bool  =
    compare x1 x2 = 0

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

  (* ここから先は微分・積分の章で追加 *)

  (* 微分 *)
  let differentiate ((n, d): t): t =
    let n' = R.differentiate n in
    let d' = R.differentiate d in
    (R.sub (R.mul n' d) (R.mul n d'), R.mul d d) |> normalize
end
