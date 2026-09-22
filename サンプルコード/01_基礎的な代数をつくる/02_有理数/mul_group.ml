module type MIN = sig
  type t
  val one: t
  val mul: t -> t -> t
  val inv: t -> t
end

module type S = sig
  include MIN
  include Mul_monoid.S with type t := t
  val div: t -> t -> t
  val pow: int -> t -> t
end

module Extend(M: MIN) = struct
  (* 除法 *)
  let div x1 x2 = M.mul x1 (M.inv x2)
  (* 累乗 *)
  let pow n x =
    let base = if n >= 0 then x else M.inv x in
    let rec aux acc n =
      if n = 0 then acc
      else aux (M.mul acc base) (n - 1)
    in
    aux M.one (abs n)
end
