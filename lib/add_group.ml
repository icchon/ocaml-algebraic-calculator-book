module type MIN = sig
  type t

  val add : t -> t -> t
  val neg : t -> t
end

module type S = sig
  include MIN
  include Add_monoid.S with type t := t

  val sub : t -> t -> t
end

module Extend (M : MIN) = struct
  (* 減法 *)
  let sub (x1 : M.t) (x2 : M.t) : M.t = M.add x1 (M.neg x2)
end
