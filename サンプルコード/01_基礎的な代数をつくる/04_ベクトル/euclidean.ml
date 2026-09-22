module type MIN = sig
  type t
  val zero: t
  val one: t
  val sub: t -> t -> t
  val mul: t -> t -> t
  val div_rem: t -> t -> t * t
  val equal: t -> t -> bool
end

module Extend(M: MIN) = struct
  (* 最大公約数 *)
  let rec gcd (a: M.t) (b: M.t): M.t =
    if M.equal b M.zero then a
    else gcd b (M.div_rem a b |> snd)

  (* 拡張ユークリッドの互除法 *)
  let rec ext_gcd (a: M.t) (b: M.t): M.t * M.t * M.t =
    if M.equal b M.zero then
      (a, M.one, M.zero)
    else
      let q, r = M.div_rem a b in
      let (g, u1, v1) = ext_gcd b r in
      (g, v1, M.sub u1 (M.mul q v1))
end
