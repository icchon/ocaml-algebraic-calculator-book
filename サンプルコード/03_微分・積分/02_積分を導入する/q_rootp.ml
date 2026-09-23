module PrimeSet = Set.Make(Integer)
module Coeff = Rational

module BasisMap = struct
  include Map.Make(PrimeSet)
  (* 単項コンテナの生成 *)
  let make (v : 'a) : 'a t = singleton PrimeSet.empty v
  let map = map
  (* Zip合成 *)
  let map2 (f : 'a -> 'b -> 'c) (m1 : 'a t) (m2 : 'b t) : 'c t =
    merge (fun _ o1 o2 ->
      match o1, o2 with
      | Some v1, Some v2 -> Some (f v1 v2)
      | _ -> invalid_arg "BasisMap.map2: maps have different domain/basis"
    ) m1 m2
  (* 全称判定 *)
  let for_all2 (p : 'a -> 'b -> bool) (m1 : 'a t) (m2 : 'b t) : bool =
    if cardinal m1 <> cardinal m2 then false
    else
      for_all (fun k v1 ->
        match find_opt k m2 with
        | Some v2 -> p v1 v2
        | None -> false
      ) m1
end

type t = Coeff.t BasisMap.t
let zero: t = BasisMap.empty
let one: t = BasisMap.singleton PrimeSet.empty Coeff.one

(* 加法 *)
let add (v1 : t) (v2 : t) : t =
  BasisMap.merge (fun _ c1_opt c2_opt ->
    match c1_opt, c2_opt with
    | None, None -> None
    | Some c, None | None, Some c -> Some c
    | Some c1, Some c2 -> Some (Coeff.add c1 c2)
  ) v1 v2

(* 符号反転 *)
let neg (v : t) : t =
  BasisMap.map Coeff.neg v

(* 基底同士の積 *)
let mul_basis
    (s1 : PrimeSet.t) (s2 : PrimeSet.t) (c1 : Coeff.t) (c2 : Coeff.t)
  : Coeff.t * PrimeSet.t =
  let common = PrimeSet.inter s1 s2 in
  let diff = PrimeSet.union (PrimeSet.diff s1 s2) (PrimeSet.diff s2 s1) in
  let factor_int =
    PrimeSet.fold (fun p acc -> Integer.mul acc p) common Integer.one
  in
  let new_coeff = Coeff.mul (Coeff.mul c1 c2) ((factor_int, Integer.one)) in
  (new_coeff, diff)

(* 基底への係数加算 *)
let add_term (c : Coeff.t) (s : PrimeSet.t) (acc : t) : t =
  let current_c =
    BasisMap.find_opt s acc |> Option.value ~default:Coeff.zero
  in
  BasisMap.add s (Coeff.add current_c c) acc

(* 乗法 *)
let mul (v1 : t) (v2 : t) : t =
  BasisMap.fold (fun s1 c1 acc1 ->
    BasisMap.fold (fun s2 c2 acc2 ->
      let (new_c, new_s) = mul_basis s1 s2 c1 c2 in
      add_term new_c new_s acc2
    ) v2 acc1
  ) v1 zero

include Add_group.Extend(struct
  type nonrec t = t
  let add, neg = add, neg
end)
include Mul_monoid.Extend(struct
  type nonrec t = t
  let one, mul = one, mul
end)

(* 有理数の埋め込み *)
let of_rational (q : Coeff.t) : t =
  if Coeff.equal q Coeff.zero then BasisMap.empty
  else BasisMap.singleton PrimeSet.empty q

(* 無平方数を、素因数の集合に分解する *)
let prime_factors (n : Integer.t) : PrimeSet.t =
  let rec aux d remaining acc =
    let d2 = Integer.mul d d in
    if Integer.compare d2 remaining > 0 then
      PrimeSet.add remaining acc
    else
      let (q, r) = Integer.div_rem remaining d in
      if Integer.equal r Integer.zero then
        aux (Integer.succ d) q (PrimeSet.add d acc)
      else
        aux (Integer.succ d) remaining acc
  in
  aux (Integer.succ Integer.one) n PrimeSet.empty

(** 平方根 *)
let of_rational_sqrt (q : Coeff.t) : t =
  if Coeff.equal q Coeff.zero then zero
  else
    match Rational.try_sqrt q with
    | Ok r -> of_rational r
    | Error (square_free, coeff) ->
        BasisMap.singleton (prime_factors square_free) coeff

(* 正規化：係数が0になった基底を取り除く *)
let normalize (v: t): t =
  BasisMap.filter (fun _ c -> not (Coeff.equal c Coeff.zero)) v

(* 等価判定 *)
let equal (x1: t) (x2: t): bool =
  BasisMap.equal Coeff.equal (normalize x1) (normalize x2)

(* 大小比較（自然な順序ではなく、符号の正規化のための便宜的な全順序） *)
let compare (x1: t) (x2: t): int =
  BasisMap.compare Coeff.compare (normalize x1) (normalize x2)

(* 登場する素数全体 *)
let collect_primes (v : t) : PrimeSet.t =
  BasisMap.fold (fun s _ acc -> PrimeSet.union s acc) v PrimeSet.empty

(* 素数pの有無による分解 *)
let split_by_prime (p : Integer.t) (v : t) : t * t =
  BasisMap.fold (fun s c (a, b) ->
    if PrimeSet.mem p s then
      let s' = PrimeSet.remove p s in
      (a, add_term c s' b)
    else
      (add_term c s a, b)
  ) v (zero, zero)

(* 逆元 *)
let rec inv (v : t) : t =
  let v = normalize v in
  if BasisMap.is_empty v then
    failwith "Division by zero"
  else
    let primes = collect_primes v in
    if PrimeSet.is_empty primes then
      let c = BasisMap.find PrimeSet.empty v in
      BasisMap.singleton PrimeSet.empty (Coeff.inv c)
    else
      let p = PrimeSet.min_elt primes in
      let a, b = split_by_prime p v in
      let sqrt_p = BasisMap.singleton (PrimeSet.singleton p) Coeff.one in
      let conj = sub a (mul b sqrt_p) in
      let norm = mul v conj in
      mul conj (inv norm)

include Mul_group.Extend(struct
  type nonrec t = t
  let one, mul, inv = one, mul, inv
end)

(* 共役 *)
let conj (x: t): t = x

(* 微分 *)
let differentiate (_: t): t = zero
