open Expr_core

(* 実体のRational・Integerモジュールは参照せず、構造的に等価な型
   （int * int）を直接書く。Rationalにこの先関数が増えても影響しない *)
type t = int * int
let to_expr ((n, d): t): expr =
  if d = 1 then num n
  else frac (num n) (num d)
