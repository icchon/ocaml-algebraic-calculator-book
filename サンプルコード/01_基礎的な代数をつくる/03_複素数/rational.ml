include Fraction.Make(Integer)

(* 整数の埋め込み *)
let of_integer (x: Integer.t): t =
  (x, Integer.one)

(* 共役 *)
let conj (x: t): t = x
