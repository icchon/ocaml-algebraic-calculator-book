open Expr_core

(* 実体のHexadecanionモジュールは参照せず、構造的に等価な型を直接書く *)
type rational_t = int * int
type complex_t = rational_t * rational_t
type quaternion_t = complex_t * complex_t
type octonion_t = quaternion_t * quaternion_t
type t = octonion_t * octonion_t

let to_expr ((a, b): t): expr =
  let (((r0, r1), (r2, r3)), ((r4, r5), (r6, r7))) = a in
  let (((r8, r9), (r10, r11)), ((r12, r13), (r14, r15))) = b in
  [ r0, ""; r1, "e_1"; r2, "e_2"; r3, "e_3"; r4, "e_4"; r5, "e_5"; r6, "e_6"; r7, "e_7";
    r8, "e_8"; r9, "e_9"; r10, "e_{10}"; r11, "e_{11}"; r12, "e_{12}"; r13, "e_{13}";
    r14, "e_{14}"; r15, "e_{15}" ]
  |> List.map (fun (c, label) ->
       if label = "" then Expr_rational.to_expr c
       else product [Expr_rational.to_expr c; atom label])
  |> sum
