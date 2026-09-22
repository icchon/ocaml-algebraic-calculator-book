module type Field = sig
  include Field.S
  val conj: t -> t
  val differentiate: t -> t (* 微分・積分の章で追加 *)
end

module type FUNCTOR_TYPE =
  functor (F: Field) -> (Field with type t = F.t * F.t)

module Make: FUNCTOR_TYPE = functor (F: Field) -> struct
  type t = F.t * F.t
  (* 等価判定 *)
  let equal ((a1, b1): t) ((a2, b2): t): bool =
    (F.equal a1 a2) && (F.equal b1 b2)
  let zero: t = (F.zero, F.zero)
  let one: t = (F.one, F.zero)
  (* 加法 *)
  let add ((a1, b1): t) ((a2, b2): t): t =
    (F.add a1 a2, F.add b1 b2)
  (* 符号反転 *)
  let neg ((a, b): t): t = (F.neg a, F.neg b)
  (* 共役 *)
  let conj ((a, b): t): t = (a, F.neg b)
  (* 乗法 *)
  let mul ((a1, b1): t) ((a2, b2): t): t =
    (F.sub (F.mul a1 a2) (F.mul (F.conj b2) b1),
    F.add (F.mul b2 a1) (F.mul b1 (F.conj a2)))

  (* 内積 *)
  let dot (x1: t) (x2: t): F.t = mul x1 (conj x2) |> fst
  (* ノルムの2乗 *)
  let norm2 (x: t): F.t = dot x x
  (* 逆元 *)
  let inv (x: t): t =
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

  (* ここから先は微分・積分の章で追加 *)

  (* 微分 *)
  let differentiate ((a, b): t): t = (F.differentiate a, F.differentiate b)
end
