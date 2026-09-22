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
  BasisMap.merge (fun _ key1 key2 ->
    match key1, key2 with
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

(** 平方根 *)
let of_rational_sqrt (q : Coeff.t) : t =
  if Coeff.equal q Coeff.zero then zero
  else
    match Rational.try_sqrt q with
    | Ok r -> of_rational r
    | Error (square_free, coeff) ->
        BasisMap.singleton (PrimeSet.singleton square_free) coeff

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

(* Vector_space.Makeをそのままincludeすると、係数ごとの加減算
   （C.map2由来のadd/sub/neg/equal/zero）が、すでに定義済みの
   同名の演算を上書きしてしまう。ここで欲しいのはmul_scalar・
   div_scalarだけなので、シグネチャで絞ってincludeする *)
include (Vector_space.Make(Coeff)(BasisMap) : sig
  type t
  val mul_scalar: Coeff.t -> t -> t
  val div_scalar: Coeff.t -> t -> t
end with type t := t)

(* 登場する素数の最大値 *)
let max_prime (x : t) : Integer.t option =
  BasisMap.fold
    (fun set _ acc ->
      if PrimeSet.is_empty set then acc
      else
        let max_in_set = PrimeSet.max_elt set in
        match acc with
        | None -> Some max_in_set
        | Some m ->
          Some (if Integer.compare max_in_set m > 0 then max_in_set else m))
    x None

(* √pの生成 *)
let make_sqrt_prime (p : Integer.t) : t =
  BasisMap.singleton (PrimeSet.singleton p) Coeff.one

(* 有理数への変換（有理数として書けない場合はNone） *)
let to_rational (x : t) : Coeff.t option =
  if BasisMap.is_empty x then Some Coeff.zero
  else if BasisMap.cardinal x = 1 && BasisMap.mem PrimeSet.empty x then
    Some (BasisMap.find PrimeSet.empty x)
  else None

(* 有理数判定 *)
let is_rational (x : t) : bool =
  match to_rational x with
  | Some _ -> true
  | None -> false

(* 立方根 *)
let rec cbrt (x : t) : t option =
  if equal x zero then Some zero
  else if is_rational x then
    match to_rational x with
    | Some q -> (
        match Rational.root 3 q with
        | Some r -> Some (of_rational r)
        | None -> None
      )
    | None -> None
  else
    match max_prime x with
    | None -> None
    | Some p ->
        let a_elem, b = split_by_prime p x in
        let sqrt_p = make_sqrt_prime p in
        match to_rational a_elem with
        | None -> None
        | Some a ->
            (* ノルム N(x) = a^2 - b^2 * p *)
            let b2 = mul b b in
            let p_elem = mul b2 (of_rational (Coeff.of_integer p)) in
            let norm = sub (of_rational (Coeff.mul a a)) p_elem in

            match cbrt norm with
            | None -> None
            | Some n_y_elem -> (
                match to_rational n_y_elem with
                | None -> None
                | Some n_y ->
                    (* cの方程式: 4c^3 - 3*N_y*c - a = 0 *)
                    let four = Coeff.of_integer 4 in
                    let three = Coeff.of_integer 3 in
                    let poly_c = [
                      Coeff.neg a;
                      Coeff.neg (Coeff.mul three n_y);
                      Coeff.zero;
                      four
                    ] in

                    let c_candidates = Q_poly.solve_equation poly_c in

                    (* dの復元と検算 *)
                    let try_construct q_c =
                      if Coeff.equal q_c Coeff.zero then None
                      else
                        let c = of_rational q_c in
                        let q_c2 = Coeff.mul q_c q_c in
                        let q_denom = Coeff.sub (Coeff.mul four q_c2) n_y in
                        if Coeff.equal q_denom Coeff.zero then None
                        else
                          let d = div_scalar q_denom b in
                          let result = add c (mul d sqrt_p) in
                          let result3 = mul (mul result result) result in
                          if equal result3 x then Some result
                          else None
                    in

                    List.find_map try_construct c_candidates
              )
