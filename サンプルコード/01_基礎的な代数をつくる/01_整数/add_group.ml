module type MIN = sig
  type t
  val add: t -> t -> t
  val neg: t -> t
end

module Extend (M: MIN) = struct
  (* 減法 *)
  let sub x1 x2 = M.add x1 (M.neg x2)
end
