module type MIN = sig
  type t
  val mul: t -> t -> t
  val one: t
end

module type S = sig
  include MIN
  val pow: int -> t -> t
end

module Extend(M: MIN) = struct
  (* 累乗 *)
  let pow n x =
    let rec aux acc n =
      if n = 0 then acc
      else aux (M.mul acc x) (n-1)
    in
    aux M.one n
end
