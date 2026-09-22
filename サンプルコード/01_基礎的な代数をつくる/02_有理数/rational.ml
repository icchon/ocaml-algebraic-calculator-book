include Fraction.Make(Integer)

(* 整数の埋め込み *)
let of_integer (x: Integer.t): t =
  (x, Integer.one)
