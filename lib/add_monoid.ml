

module type S = sig
  type t
  val zero: t
  val add: t -> t -> t
end
