module type S = sig
  type t
  include Add_group.S with type t := t
  include Mul_monoid.S with type t := t
  val equal : t -> t -> bool
end
