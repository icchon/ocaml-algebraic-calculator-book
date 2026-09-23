include Fraction.Make(Integer)
(* 整数の埋め込み *)
let of_integer (x: Integer.t): t =
  (x, Integer.one)
(* 共役 *)
let conj (x: t): t = x
(* ここから先は2次方程式の章で追加 *)

(* k乗根 *)
let root (k : int) ((n, d) : t) : t option =
  if Integer.equal n Integer.zero then Some zero
  else
    let d_pow = Integer.pow (k - 1) d in
    let target = Integer.mul n d_pow in
    match Integer.root k target with
    | Some num -> Some (mul (num, d) one)
    | None -> None

(* 整数mを、平方因子を持たない無平方数rと、r * s^2 = mを満たすsの組に分解する *)
let extract_square_factor (m : Integer.t) : Integer.t * Integer.t =
  let rec aux d remaining square_part =
    let d2 = Integer.mul d d in
    if Integer.compare d2 remaining > 0 then
      (remaining, square_part)
    else
      let (q, r) = Integer.div_rem remaining d2 in
      if Integer.equal r Integer.zero then
        aux d q (Integer.mul square_part d)
      else
        aux (Integer.add d Integer.one) remaining square_part
  in
  aux (Integer.succ Integer.one) m Integer.one

(* 平方根が有理数として開けるか判定する。開ければその値を、開けなければ
   無平方数（下の体で添加する基底そのもの）と、その平方根の係数を返す。
   sqrt(n/d) = sqrt(n*d)/d = s*sqrt(r)/d （n*d = r * s^2、rは無平方）という
   変形にもとづく *)
let try_sqrt (q : t) : (t, Integer.t * t) result =
  match root 2 q with
  | Some r -> Ok r
  | None ->
      let (num, den) = q in
      let (square_free, s) = extract_square_factor (Integer.mul num den) in
      Error (square_free, div (of_integer s) (of_integer den))
