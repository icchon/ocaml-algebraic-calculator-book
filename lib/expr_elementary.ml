(* Elementary.tはQF.t BasisMap.t（Basis.tをキーとするMap.Make(Basis)の出力）
   というQ_rootpと同じく抽象的なMap型なので、expr_q_rootp.mlと同じ要領で
   「必要な操作だけを要求するファンクタ」にする。Basis.tの中身（Exp/Log）は
   Quadratic.t（= Q_rootp.t * Q_rootp.t）そのものなので、その形はQパラメータ
   から導ける。ただし次の2点はQ_rootpの表示情報だけでは導けないので、
   実体のQuadratic・Quadratic_polyから直接もらう:
     - 指数の項が「有理関数部分（Exp 0）」かどうかを判定するための
       Quadratic.equal/zero
     - 分母（Quadratic_poly.t）がちょうど1かどうかを判定するための
       Quadratic_poly.equal/one *)
module type QUADRATIC_LIKE = sig
  type t
  val equal: t -> t -> bool
  val zero: t
end

module type QUADRATIC_POLY_LIKE = sig
  type coeff
  type t = coeff list
  val equal: t -> t -> bool
  val one: t
end

(* Elementary.Basis.tの中身（Exp/Log）はQuadratic.t（= Q.t * Q.t）そのもの
   なので、この形はQをすでに受け取ったあとでないと書けない。そのため、
   他のLIKEシグネチャのように独立した名前をつけず、Makeの引数の場所に
   直接書く（抽象型quadraticを別途用意して"with type"で束ねようとすると、
   実体のElementaryモジュール自身がquadraticという名前のフィールドを
   持たないため一致判定に失敗する） *)
module Make
    (Q: Expr_q_rootp.Q_ROOTP_LIKE)
    (QD: QUADRATIC_LIKE with type t = Q.t * Q.t)
    (QP: QUADRATIC_POLY_LIKE with type coeff = Q.t * Q.t)
    (E: sig
       module Basis: sig
         type t = Exp of (Q.t * Q.t) | Log of (Q.t * Q.t)
       end
       type t
       module BasisMap: sig
         val bindings: t -> (Basis.t * ((Q.t * Q.t) list * (Q.t * Q.t) list)) list
       end
     end) = struct
  open Expr_core
  module Quadratic_expr = Expr_quadratic.Make (Q)
  module Quadratic_poly_expr = Expr_quadratic_poly.Make (Q)

  type t = E.t

  let qf_to_expr ((n, d): QP.t * QP.t): expr =
    if QP.equal d QP.one then Quadratic_poly_expr.to_expr n
    else frac (Quadratic_poly_expr.to_expr n) (Quadratic_poly_expr.to_expr d)

  let term_expr (key: E.Basis.t) (value: QP.t * QP.t): expr =
    let coeff = qf_to_expr value in
    match key with
    | E.Basis.Exp a when QD.equal a QD.zero -> coeff
    | E.Basis.Exp a ->
      product [coeff; pow (atom "e") (product [Quadratic_expr.to_expr a; atom "x"])]
    | E.Basis.Log a ->
      product [coeff; fn "log" [sum [atom "x"; neg (Quadratic_expr.to_expr a)]]]

  let to_expr (v: t): expr =
    E.BasisMap.bindings v
    |> List.map (fun (k, c) -> term_expr k c)
    |> sum
end
