open Expr_core

(* 実体のIntegerモジュールは参照せず、構造的に等価な型（int）を直接書く。
   こうしておけば、この先Integerに関数が増えても（succ・root・differentiate
   など）、このファイルが要求するインターフェースは変わらず、Integerの
   完成度によらずto_tex.cmxaとリンクできる *)
type t = int
let to_expr (n: t): expr = num n
