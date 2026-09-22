module type Field = sig
  include Field.S
  val conj: t -> t
  val differentiate: t -> t
end

module type FUNCTOR_TYPE =
  functor (F: Field) -> (Field with type t = F.t * F.t)

module Make(F: Field) = struct
  type t = F.t * F.t
  (* 等価判定 *)
  let equal (a1, b1) (a2, b2) =
    (F.equal a1 a2) && (F.equal b1 b2)
  let zero = (F.zero, F.zero)
  let one = (F.one, F.zero)
  (* 加法 *)
  let add (a1, b1) (a2, b2) = (F.add a1 a2, F.add b1 b2)
  (* 符号反転 *)
  let neg (a, b) = (F.neg a, F.neg b)
  (* 共役 *)
  let conj (a, b) = (a, F.neg b)
  (* 乗法 *)
  let mul (a1, b1) (a2, b2) =
    (F.sub (F.mul a1 a2) (F.mul (F.conj b2) b1),
    F.add (F.mul b2 a1) (F.mul b1 (F.conj a2)))
  (* ノルムの2乗 *)
  let norm2 x = mul x (conj x) |> fst
  (* 逆元 *)
  let inv x =
    let norm = norm2 x in
    let (a, b) = conj x in
    (F.div a norm, F.div b norm)

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

  (* 微分 *)
  let differentiate ((a, b): t): t = (F.differentiate a, F.differentiate b)
end
