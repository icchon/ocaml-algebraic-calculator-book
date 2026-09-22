type t = int
(* 等価判定 *)
let equal (x1: t) (x2: t): bool =
   x1 = x2
let zero: t = 0
let one: t = 1
(* 加法 *)
let add (x1: t) (x2: t): t =
  x1 + x2
(* 後者関数 *)
let succ (x: t): t =
  add x one
(* 符号反転 *)
let neg (x: t): t =
  -x
(* 乗法 *)
let mul (x1: t) (x2: t): t =
  x1 * x2
(* 商と余り *)
let div_rem (x1: t) (x2: t): t * t =
  (x1 / x2, x1 mod x2)

(* 大小比較 *)
let compare (x1: t) (x2: t): int =
  x1 - x2

include Mul_monoid.Extend(struct
  type nonrec t = t
  let one, mul = one, mul
end)
include Add_group.Extend(struct
  type nonrec t = t
  let add, neg = add, neg
end)
include Euclidean.Extend(struct
  type nonrec t = t
  let zero, one, sub, mul, div_rem, equal = zero, one, sub, mul, div_rem, equal
end)

(* ここから先は2次方程式の章で追加 *)

(* n乗根 *)
let rec root (n : int) (x : t) : t option =
    if n < 0 then None
    else if equal x zero then Some zero
    else if compare x zero < 0 then
      if n mod 2 = 0 then None
      else
        match root n (neg x) with
        | Some k -> Some (neg k)
        | None -> None
    else
      let rec search low high =
        if compare low high > 0 then None
        else
          let mid = div_rem (add low high) 2 |> fst in
          let mid_pow = pow n mid in
          let cmp = compare mid_pow x in
          if cmp = 0 then Some mid
          else if cmp < 0 then search (add mid one) high
          else search low (sub mid one)
      in
      search one x

(* ここから先は微分・積分の章で追加 *)

(* 絶対値 *)
let abs (x: t): t =
  if compare x zero >= 0 then x
  else neg x

(* 微分 *)
let differentiate (_: t): t = zero
