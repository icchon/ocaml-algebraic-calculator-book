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
