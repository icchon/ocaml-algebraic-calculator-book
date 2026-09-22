open Expr_core

(* 四元数・八元数・十六元数は、Complexical.Makeの入れ子構造をそのまま
   Sum/Productで表すこともできる（Make_paired_exprを重ねればよい）が、
   本書では伝統的な i, j, k / e_1, ..., e_15 という基底名に展開した
   表示を採用しているため、その形をここで再現する。
   実体のQuaternionモジュールは参照せず、構造的に等価な型を直接書く
   （Rational・Complexにこの先関数が増えても影響しない） *)
type rational_t = int * int
type complex_t = rational_t * rational_t
type t = complex_t * complex_t

let to_expr ((a, b): t): expr =
  let (r0, r1) = a in
  let (r2, r3) = b in
  sum [
    Expr_rational.to_expr r0;
    product [Expr_rational.to_expr r1; atom "i"];
    product [Expr_rational.to_expr r2; atom "j"];
    product [Expr_rational.to_expr r3; atom "k"];
  ]
