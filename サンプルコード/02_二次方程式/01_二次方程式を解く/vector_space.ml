module type Container = sig
  type 'a t
  val make: 'a -> 'a t
  val map: ('a -> 'b) -> 'a t -> 'b t
  val map2: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val for_all2: ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
end

module type S = sig
    type t
    type scalar

    val equal: t -> t -> bool
    include Add_group.S with type t := t

    val mul_scalar: scalar -> t -> t
    val div_scalar: scalar -> t -> t
end

module type FUNCTOR_TYPE =
  functor (F: Field.S) (C: Container) ->
    (S with type scalar = F.t and type t = F.t C.t)

module Make: FUNCTOR_TYPE = functor (F: Field.S) (C: Container) -> struct
    type t = F.t C.t
    type scalar = F.t
    (* 等価判定 *)
    let equal (x1: t) (x2: t): bool = C.for_all2 F.equal x1 x2
    (* 零ベクトル *)
    let zero: t = C.make F.zero
    (* 加法 *)
    let add (x1: t) (x2: t): t = C.map2 F.add x1 x2
    (* 符号反転 *)
    let neg (x: t): t = C.map F.neg x

    include Add_group.Extend(struct
      type nonrec t = t
      let zero, add, neg = zero, add, neg
    end)
    (* スカラー倍 *)
    let mul_scalar (s: scalar) (v: t): t = C.map (fun x -> F.mul s x) v
    (* スカラーでの除法 *)
    let div_scalar (s: scalar) (v: t): t =
      C.map (fun x -> F.mul (F.inv s) x) v
end
