open Expr_core

(* Q_rootpの内部表現（PrimeSet・BasisMap）はSet.Make/Map.Makeの出力という
   抽象型なので、Integer・Rationalのように構造的に等価な型を直接書くことが
   できない。そこで、必要な操作だけを要求するファンクタにする。こうして
   おけば、Q_rootpにこの先cbrtのような関数が増えても、このファンクタが
   要求するインターフェースは変わらないので、to_tex.cmxaとのリンクに
   影響しない。呼び出す側は、そのときどきの実体のQ_rootpモジュールを渡して
   その場でインスタンス化する *)
module type Q_ROOTP_LIKE = sig
  type t
  module PrimeSet: sig
    type t
    val is_empty: t -> bool
    val elements: t -> int list
  end
  module BasisMap: sig
    val bindings: t -> (PrimeSet.t * (int * int)) list
  end
end

module Make (Q: Q_ROOTP_LIKE) = struct
  type t = Q.t

  let sqrt_term (s: Q.PrimeSet.t): expr option =
    if Q.PrimeSet.is_empty s then None
    else
      Q.PrimeSet.elements s
      |> List.map (fun p -> atom (Printf.sprintf "\\sqrt{%s}" (to_string (Expr_integer.to_expr p))))
      |> product
      |> Option.some

  let to_expr (x: t): expr =
    Q.BasisMap.bindings x
    |> List.map (fun (s, c) ->
         match sqrt_term s with
         | None -> Expr_rational.to_expr c
         | Some sq -> product [Expr_rational.to_expr c; sq])
    |> sum
end
