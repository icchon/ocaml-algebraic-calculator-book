

module type MIN = sig
  type t
  val zero: t
  val one: t
  val sub: t -> t -> t
  val mul: t -> t -> t
  val div_rem: t -> t -> t * t
  val equal: t -> t -> bool
end

module type S = sig
  include MIN
  val gcd: t -> t -> t
  val ext_gcd : t -> t -> t * t * t
end

module Extend(M: MIN) = struct
  (* 最大公約数 *)
  let rec gcd (a: M.t) (b: M.t): M.t =
    if M.equal b M.zero then a
    else gcd b (M.div_rem a b |> snd)

  (* 最小公倍数 *)
  let lcm (a: M.t) (b: M.t): M.t =
    M.div_rem (M.mul a b) (gcd a b) |> fst

  (* 拡張ユークリッドの互除法 *)
  let rec ext_gcd (a: M.t) (b: M.t): (M.t * M.t * M.t) =
    if M.equal b M.zero then
      (a, M.one, M.zero)
    else
      let q, r = M.div_rem a b in
      let (g, u1, v1) = ext_gcd b r in
      let u = v1 in
      let v = M.sub u1 (M.mul q v1) in
      (g, u, v)
end
