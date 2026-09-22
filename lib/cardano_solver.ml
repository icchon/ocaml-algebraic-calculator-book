(* cardano_solver.ml *)

module type ROOT_SET = sig
  module C : Polynomial.Coeff
  val embed : Rational.t -> C.t
  val roots : C.t list
  (* Cが呼び出し側からは抽象型（実行時にしか具体的な型が決まらない）なので、
     文字列化の手段（Expr.to_exprに相当するもの）も、C自身が一緒に
     持ってきてもらう必要がある *)
  val to_expr : C.t -> Expr.expr
end

(** チルンハウス標準形のp, qを計算 *)
let compute_depressed_pq (poly : Q_poly.t) : Rational.t * Rational.t =
  (* 係数の取得 *)
  let d = List.nth poly 0 in
  let c = List.nth poly 1 in
  let b = List.nth poly 2 in
  let a = List.nth poly 3 in

  (* p = (3ac - b^2) / (3a^2) *)
  let three = (3, 1) in
  let p =
    Rational.div
      (Rational.sub
         (Rational.mul three (Rational.mul a c)) (Rational.mul b b))
      (Rational.mul three (Rational.mul a a))
  in

  (* q = (2b^3 - 9abc + 27a^2d) / (27a^3) *)
  let two = (2, 1) in
  let nine = (9, 1) in
  let twenty_seven = (27, 1) in
  let num =
    Rational.add
      (Rational.sub
         (Rational.mul two (Rational.pow 3 b))
         (Rational.mul nine (Rational.mul a (Rational.mul b c))))
      (Rational.mul twenty_seven (Rational.mul (Rational.mul a a) d))
  in
  let den = Rational.mul twenty_seven (Rational.pow 3 a) in
  let q = Rational.div num den in
  (p, q)

module Cubic_expr = Expr.Cubic.Make (Q_rootp)

(** 3次方程式の求解 *)
let solve (poly : Q_poly.t) : (module ROOT_SET) =
  let (p, q) = compute_depressed_pq poly in

  let b = (List.nth poly 2) in
  let a = (List.nth poly 3) in
  let shift_o = Rational.div (Rational.neg b) (Rational.mul (3, 1) a) |> Omega_field.of_rational in

  (* カルダノの u = -q/2 + sqrt((q/2)^2 + (p/3)^3) *)
  let half_q = Rational.div q (2, 1) in
  let third_p = Rational.div p (3, 1) in
  let disc = Rational.add (Rational.mul half_q half_q) (Rational.pow 3 third_p) in
  let sqrt_disc = Omega_field.sqrt_disc disc in
  let u_formula = Omega_field.add (Omega_field.neg (Omega_field.of_rational half_q)) sqrt_disc in
  (* uは、t^2+qt-(p/3)^3=0というレゾルベント方程式の2つの根のうち
     一方（t1）。u=0になるのはp=0のとき（このときt1*t2=-(p/3)^3=0なので
     t1=0はレゾルベントの根として辻褄が合う）だが、その場合
     alpha=cbrt(u)=0となり、alpha*v=-p/3の関係が0=0で自明になって
     しまい、vを決められなくなる。この場合は、レゾルベントのもう一方の根
     t2=-q-u=-qを改めてuとして使う（t1・t2はレゾルベントの根として
     対称なので、どちらを使ってもよい） *)
  let u =
    if Omega_field.equal u_formula Omega_field.zero
    then Omega_field.of_rational (Rational.neg q)
    else u_formula
  in

  (* 1の原始3乗根 ω *)
  let omega = Omega_field.omega in
  let omega2 = Omega_field.mul omega omega in

  match Omega_field.cbrt u with
  | Some alpha ->
      (* パターンA: Omega_field内で解ける場合 *)
      (* ダミー拡大体 *)
      let module F = struct let u = Omega_field.one end in
      let module C = Cubic.Make(F) in

      (* v = -p / (3 * alpha)。alpha=0になるのは、上でuを選び直しても
         なおu=0のまま、つまりp=0かつq=0（3重根0）のときだけで、
         このときはv=0が正しい *)
      let v =
        if Omega_field.equal alpha Omega_field.zero then Omega_field.zero
        else Omega_field.div (Omega_field.neg (Omega_field.of_rational third_p)) alpha
      in

      let y1 = Omega_field.add alpha v in
      let y2 = Omega_field.add (Omega_field.mul alpha omega) (Omega_field.mul v omega2) in
      let y3 = Omega_field.add (Omega_field.mul alpha omega2) (Omega_field.mul v omega) in

      let x1 = [ Omega_field.add y1 shift_o ] in
      let x2 = [ Omega_field.add y2 shift_o ] in
      let x3 = [ Omega_field.add y3 shift_o ] in

      let module Res = struct
        module C = C
        let embed (r : Rational.t) : C.t = [ Omega_field.of_rational r ]
        let roots = [x1; x2; x3]
        let to_expr = Cubic_expr.to_expr
      end in
      (module Res)

  | None ->
      (* パターンB: 還元不能ケース（1回拡大K(∛u)） *)
      let module F = struct let u = u end in
      let module C = Cubic.Make(F) in

      (* 単項式 X ≡ ∛u *)
      let alpha = [ Omega_field.zero; Omega_field.one ] in

      (* 定数埋め込み *)
      let embed c = [ c ] in

      (* v = -p / (3 * alpha) *)
      let alpha_inv = C.inv alpha in
      let v = C.mul (embed (Omega_field.neg (Omega_field.of_rational third_p))) alpha_inv in

      let shift_c = embed shift_o in
      let omega_c = embed omega in
      let omega2_c = embed omega2 in

      let y1 = C.add alpha v in
      let y2 = C.add (C.mul alpha omega_c) (C.mul v omega2_c) in
      let y3 = C.add (C.mul alpha omega2_c) (C.mul v omega_c) in

      let x1 = C.add y1 shift_c in
      let x2 = C.add y2 shift_c in
      let x3 = C.add y3 shift_c in

      let module Res = struct
        module C = C
        let embed (r : Rational.t) : C.t = [ Omega_field.of_rational r ]
        let roots = [x1; x2; x3]
        let to_expr = Cubic_expr.to_expr
      end in
      (module Res)
