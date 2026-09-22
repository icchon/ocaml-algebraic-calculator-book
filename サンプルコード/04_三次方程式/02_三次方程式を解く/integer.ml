module BASE = struct
  type t = int
  (* 等価判定 *)
  let equal x1 x2 = x1 = x2
  (* 大小比較 *)
  let compare x1 x2 = x1 - x2
  let zero = 0
  let one = 1
  (* 加法 *)
  let add x1 x2 = x1 + x2
  (* 符号反転 *)
  let neg x = -x
  (* 乗法 *)
  let mul x1 x2 = x1 * x2
  (* 商と余り *)
  let div_rem x1 x2 = (x1 / x2, x1 mod x2)
end

include BASE
include Mul_monoid.Extend(BASE)
include Add_group.Extend(BASE)
include Euclidean.Extend(struct
  type nonrec t = t
  let zero, one, sub, mul, div_rem, equal =
    zero, one, sub, mul, div_rem, equal
end)

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
    search zero x

(* 後者関数 *)
let succ (x: t): t =
  add x one

(* 微分 *)
let differentiate (_: t): t = zero

(* 絶対値 *)
let abs x =
  if compare x zero >= 0 then x
  else neg x
