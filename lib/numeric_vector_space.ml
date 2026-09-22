
module type Dim = sig
  val dim: int
end

module type FUNCTOR_TYPE =
  functor (D: Dim) (F: Field.S) ->
    (Vector_space.S with type scalar = F.t and type t = F.t list)

module Make: FUNCTOR_TYPE = functor (D: Dim) (F: Field.S) -> struct
  module Container = struct
    include List
    (* 定数ベクトルの生成 *)
    let make (x: 'a): 'a t =
      List.init D.dim (fun _ -> x)
  end
  include Vector_space.Make(F)(Container)
end
