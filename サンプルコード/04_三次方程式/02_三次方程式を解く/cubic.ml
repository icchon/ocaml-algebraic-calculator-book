module type Field = sig
  val u : Omega_field.t
end

module type FUNCTOR_TYPE =
  functor (F : Field) ->
    (Polynomial.Coeff with type t = Omega_field.t list)

module Make: FUNCTOR_TYPE = functor (F : Field) -> struct
  module I = struct
    module C = Omega_field
    module P = Polynomial.Make(C)
    (* x^3 - u の係数リスト *)
    let poly =
      [ Omega_field.neg F.u; Omega_field.zero;
        Omega_field.zero; Omega_field.one ]
  end
  include Field_extension.Make(I)
end
