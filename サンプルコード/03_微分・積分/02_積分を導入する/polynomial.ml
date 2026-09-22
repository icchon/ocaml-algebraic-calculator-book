module type Coeff = sig
  include Field.S
  val differentiate: t -> t
end

module type S = sig
  type coeff
  type t = coeff list
  val zero: t
  val one: t
  val normalize: t -> t
  val equal: t -> t -> bool
  val degree: t -> int
  val mul: t -> t -> t
  val add: t -> t -> t
  val neg: t -> t
  val eval: t -> coeff -> coeff
  val div_rem: t -> t -> t * t
  val pow: int -> t -> t
  val sub: t -> t -> t
  val gcd: t -> t -> t
  val ext_gcd: t -> t -> t * t * t
  val leading_coeff: t -> coeff
  val div_scalar: coeff -> t -> t
  val mul_scalar: coeff -> t -> t
  val differentiate: t -> t
end

module type FUNCTOR_TYPE = functor (C: Coeff) -> (S with type coeff = C.t)

module Make: FUNCTOR_TYPE = functor (C: Coeff) -> struct
  type t = C.t list
  type coeff = C.t
  let zero: t = []
  let one: t = [C.one]
  (* 正規化 *)
  let normalize (x: t): t =
    let rec remove_leading_zeros = function
      | [] -> []
      | c :: cs ->
        if C.equal c C.zero then remove_leading_zeros cs
        else c :: cs
    in
    List.rev x |> remove_leading_zeros |> List.rev

  (* 等価判定 *)
  let equal (x1: t) (x2: t): bool =
    let p1 = normalize x1 in
    let p2 = normalize x2 in
    let rec aux l1 l2 =
      match l1, l2 with
      | [], [] -> true
      | [], _ | _, [] -> false
      | c1 :: cs1, c2 :: cs2 -> C.equal c1 c2 && aux cs1 cs2
    in
    aux p1 p2

  (* 次数 *)
  let degree (p: t) : int =
    match normalize p with
    | [] -> -1
    | p' -> List.length p' - 1

  (* 最高次係数 *)
  let leading_coeff (p: t): C.t =
    match List.rev p with
    | [] -> C.zero
    | x::xs -> x

  (* 乗法 *)
  let mul (x1: t) (x2: t): t =
    let p1 = normalize x1 in
    let p2 = normalize x2 in
    if p1 = [] || p2 = [] then []
    else
      let res_len = List.length p1 + List.length p2 - 1 in
      let res = Array.make res_len C.zero in
      List.iteri (fun i c1 ->
        List.iteri (fun j c2 ->
          res.(i + j) <- C.add res.(i + j) (C.mul c1 c2)
        ) p2
      ) p1;
      Array.to_list res |> normalize

  (* 加法 *)
  let rec add (x1: t) (x2: t): t =
    match x1, x2 with
    | [], l | l, [] -> l
    | c1 :: cs1, c2 :: cs2 -> C.add c1 c2 :: add cs1 cs2

  (* 符号反転 *)
  let neg (x: t): t =
    List.map C.neg x

  (* 評価 *)
  let eval (xs: t) (x: C.t): C.t =
    List.mapi (fun i c -> C.mul c (C.pow i x)) xs
    |> List.fold_left (fun acc s -> C.add acc s) C.zero

  include Mul_monoid.Extend(struct
    type nonrec t = t
    let one, mul = one, mul
  end)

  include Add_group.Extend(struct
    type nonrec t = t
    let add, neg = add, neg
  end)

  (* 商と余り *)
  let div_rem (num: t) (den: t) : t * t =
    let den_norm = normalize den in
    if den_norm = [] then failwith "Division by zero polynomial"
    else
      let lc_den = List.nth den_norm (List.length den_norm - 1) in
      let deg_den = degree den_norm in
      let rec aux rem quo_acc =
        let rem_norm = normalize rem in
        let deg_rem = degree rem_norm in
        if deg_rem < deg_den then
          (normalize quo_acc, rem_norm)
        else
          let lc_rem = List.nth rem_norm deg_rem in
          let factor = C.div lc_rem lc_den in
          let shift = deg_rem - deg_den in
          let term =
            List.init (shift + 1)
              (fun i -> if i = shift then factor else C.zero)
          in
          let new_rem = sub rem_norm (mul term den_norm) in
          let new_quo = add quo_acc term in
          aux new_rem new_quo
      in
      aux num []

  include Euclidean.Extend(struct
    type nonrec t = t
    let zero, one, sub, mul, div_rem, equal =
      zero, one, sub, mul, div_rem, equal
  end)

  include (Vector_space.Make(C)(struct
    include List
    let make x = []
  end) : sig
    type t
    val mul_scalar: C.t -> t -> t
    val div_scalar: C.t -> t -> t
  end with type t := t)

  (* 整数から係数への変換 *)
  let rec int_to_coeff (n: int): C.t =
    if n <= 0 then C.zero else C.add C.one (int_to_coeff (n - 1))

  (* 微分 *)
  let differentiate (p: t): t =
    let coeff_deriv = List.map C.differentiate p in
    let power_deriv =
      match p with
      | [] -> []
      | _ :: rest -> List.mapi (fun i c -> C.mul c (int_to_coeff (i + 1))) rest
    in
    add coeff_deriv power_deriv
end
