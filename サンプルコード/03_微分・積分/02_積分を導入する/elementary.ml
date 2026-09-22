module QF = Fraction.Make(Quadratic_poly)

(* 基底: exp(ax)（a=0のときは有理関数そのもの）と log(x-a) *)
module Basis = struct
  type t = Exp of Quadratic.t | Log of Quadratic.t
  let compare (x1: t) (x2: t): int =
    match x1, x2 with
    | Exp a, Exp b -> Quadratic.compare a b
    | Log a, Log b -> Quadratic.compare a b
    | Exp _, Log _ -> -1
    | Log _, Exp _ -> 1
end
module BasisMap = Map.Make(Basis)

type t = QF.t BasisMap.t

let zero: t = BasisMap.empty

(* 多項式pを分子、1を分母とする有理関数 *)
let of_poly (p: Quadratic_poly.t): QF.t = (p, Quadratic_poly.one)

(* Quadratic.t の定数を有理関数として埋め込む *)
let of_scalar (q: Quadratic.t): QF.t =
  if Quadratic.equal q Quadratic.zero then QF.zero
  else of_poly [q]

(* x - a を表す多項式 *)
let linear (a: Quadratic.t): Quadratic_poly.t =
  [Quadratic.neg a; Quadratic.one]

(* 基底への値の加算 *)
let add_term (key: Basis.t) (v: QF.t) (acc: t): t =
  let current =
    BasisMap.find_opt key acc |> Option.value ~default:QF.zero
  in
  BasisMap.add key (QF.add current v) acc

(* 有理関数 u(x) の項をつくる *)
let of_rational (u: QF.t): t = add_term (Basis.Exp Quadratic.zero) u zero

(* u(x)*exp(ax) の項をつくる *)
let exp_term (u: QF.t) (a: Quadratic.t): t = add_term (Basis.Exp a) u zero

(* w(x)*log(x-a) の項をつくる *)
let log_term (w: QF.t) (a: Quadratic.t): t = add_term (Basis.Log a) w zero

(* 加法 *)
let add (v1: t) (v2: t): t =
  BasisMap.fold add_term v2 v1

(* 符号反転 *)
let neg (v: t): t = BasisMap.map QF.neg v

(* 定数倍 *)
let scale (k: Quadratic.t) (v: t): t =
  BasisMap.map (fun u -> QF.mul (of_scalar k) u) v

(* 正規化: 係数が0の基底を取り除く *)
let normalize (v: t): t =
  BasisMap.filter (fun _ c -> not (QF.equal c QF.zero)) v

(* 等価判定 *)
let equal (x1: t) (x2: t): bool =
  BasisMap.equal QF.equal (normalize x1) (normalize x2)

(* 微分 *)
let differentiate (v: t): t =
  BasisMap.fold (fun key value acc ->
    match key with
    | Basis.Exp a ->
      (* (U*exp(ax))' = (U' + a*U)*exp(ax) *)
      let u' = QF.differentiate value in
      let au = QF.mul (of_scalar a) value in
      add_term (Basis.Exp a) (QF.add u' au) acc
    | Basis.Log a ->
      (* (U*log(x-a))' = U'*log(x-a) + U/(x-a) *)
      let w' = QF.differentiate value in
      let acc = add_term (Basis.Log a) w' acc in
      let pole = QF.div value (of_poly (linear a)) in
      add_term (Basis.Exp Quadratic.zero) pole acc
  ) v zero

(* ここから先は積分の節で追加。差分をとった結果が同じ型（Elementary.t）に
   戻ってくる微分と違い、積分は特定の形の入力（有理関数、多項式×指数関数、
   定数×log）だけを受け取る、それぞれ独立した計算になる。積分できない
   入力にはfailwithで応じる。integrate/differentiateが同じモジュールに
   揃っているので、計算した結果を微分すると本当に元の被積分関数に戻るかを、
   その場でcheck_derivativeを使って検算できる *)

(* 積分結果を微分すると、もとの被積分関数（target）に戻ることを検算する。
   一致しなければ、積分の実装自体にバグがあることを意味するのでfailwithする *)
let check_derivative (name: string) (target: t) (result: t): unit =
  let diff = add (differentiate result) (neg target) in
  if not (equal diff zero) then
    failwith (name ^ ": differentiate(result)が元の被積分関数と一致しません")

(* 係数の埋め込み *)
let embed (p: Q_poly.t): Quadratic_poly.t = List.map Quadratic.of_rational p

(* 多項式部分の積分 *)
let integrate_poly (s: Q_poly.t): Q_poly.t =
  Rational.zero ::
    List.mapi (fun i c -> Rational.div c (Rational.of_integer (i + 1))) s

(* 単根aの寄与: r(a)/q'(a) を係数とするlog項 *)
let simple_root_term
    (r: Quadratic_poly.t) (q': Quadratic_poly.t) (a: Quadratic.t)
  : t =
  let c =
    Quadratic.div (Quadratic_poly.eval r a) (Quadratic_poly.eval q' a)
  in
  log_term (of_scalar c) a

(* 二重根aの寄与: q = (x-a)^2 h(x) と分解し、
   r/q = A/(x-a) + B/(x-a)^2 + (hの他の根からの項) の A・Bを求める。
   Bの項の不定積分は B/(x-a)^2 dx = -B/(x-a) なので、符号を反転して足す *)
let double_root_term
    (r: Quadratic_poly.t) (q: Quadratic_poly.t) (a: Quadratic.t)
  : t =
  let lin = linear a in
  let h =
    fst (Quadratic_poly.div_rem (fst (Quadratic_poly.div_rem q lin)) lin)
  in
  let h' = Quadratic_poly.differentiate h in
  let r' = Quadratic_poly.differentiate r in
  let r_a = Quadratic_poly.eval r a in
  let h_a = Quadratic_poly.eval h a in
  let r'_a = Quadratic_poly.eval r' a in
  let h'_a = Quadratic_poly.eval h' a in
  let b = Quadratic.div r_a h_a in
  let a_coeff =
    Quadratic.div
      (Quadratic.sub (Quadratic.mul r'_a h_a) (Quadratic.mul r_a h'_a))
      (Quadratic.mul h_a h_a)
  in
  let log_part = log_term (of_scalar a_coeff) a in
  let pole =
    QF.div (of_scalar (Quadratic.neg b)) (of_poly lin)
  in
  add log_part (of_rational pole)

(* 分母qの次数から、直接すべての根を求める。qの次数が2以下でさえあれば、
   2次方程式の求解がそのまま使える（判別式が0なら重根、つまり多重度2）ので、
   有理数根定理のような別の道具は要らない *)
let roots_of (q: Q_poly.t): (Quadratic.t * int) list =
  match Q_poly.degree q with
  | d when d <= 0 -> []
  | 1 ->
    let c0 = List.nth q 0 in
    let c1 = List.nth q 1 in
    [ (Quadratic.of_rational (Rational.div (Rational.neg c0) c1), 1) ]
  | 2 ->
    let c0 = List.nth q 0 in
    let c1 = List.nth q 1 in
    let c2 = List.nth q 2 in
    let (x1, x2) = Quadratic.solve c2 c1 c0 in
    if Quadratic.equal x1 x2 then [ (x1, 2) ] else [ (x1, 1); (x2, 1) ]
  | _ ->
    failwith "Elementary.integrate_rational: denominator has degree >= 3"

(* 有理関数 p/q の積分 *)
let integrate_rational (p: Q_poly.t) (q: Q_poly.t): t =
  let (s, r_rem) = Q_poly.div_rem p q in
  let all_roots = roots_of q in
  let q_c = embed q in
  let q'_c = embed (Q_poly.differentiate q) in
  let r_c = embed r_rem in
  let result =
    List.fold_left (fun acc (root, mult) ->
      let term =
        match mult with
        | 1 -> simple_root_term r_c q'_c root
        | 2 -> double_root_term r_c q_c root
        | _ ->
          failwith
            ("Elementary.integrate_rational: a root with multiplicity >= 3 " ^
             "is unsupported")
      in
      add acc term
    ) (of_rational (of_poly (embed (integrate_poly s))))
      all_roots
  in
  check_derivative "Elementary.integrate_rational" (of_rational (embed p, embed q)) result;
  result

(* 多項式×指数関数 p(x)*exp(ax) の積分（aは0でない） *)
let integrate_exp (a: Rational.t) (p: Q_poly.t): t =
  if Rational.equal a Rational.zero then
    failwith
      ("Elementary.integrate_exp: a must be non-zero " ^
       "(use integrate_rational for a = 0)");
  let n = Q_poly.degree p in
  let result =
    if n < 0 then zero
    else
      let r = Array.make (n + 1) Rational.zero in
      for i = n downto 0 do
        let p_i = List.nth p i in
        let higher =
          if i = n then Rational.zero
          else Rational.mul (Rational.of_integer (i + 1)) r.(i + 1)
        in
        r.(i) <- Rational.div (Rational.sub p_i higher) a
      done;
      let r_poly : Quadratic_poly.t =
        Array.to_list r |> List.map Quadratic.of_rational
      in
      exp_term (of_poly r_poly) (Quadratic.of_rational a)
  in
  let target = exp_term (of_poly (embed p)) (Quadratic.of_rational a) in
  check_derivative "Elementary.integrate_exp" target result;
  result

(* 多項式×log(x-a) の積分。部分積分を使う。S(x) = ∫p(x)dx とすると、
   P(x)log(x-a)の不定積分は S(x)*log(x-a) - ∫S(x)/(x-a)dx となる。
   残った積分はただの有理関数の積分なので、integrate_rationalにそのまま
   帰着できる *)
let integrate_log (p: Q_poly.t) (a: Rational.t): t =
  let s = integrate_poly p in
  let q_lin : Q_poly.t = [Rational.neg a; Rational.one] in
  let rest = integrate_rational s q_lin in
  let a_q = Quadratic.of_rational a in
  let log_part = log_term (of_poly (embed s)) a_q in
  let result = add log_part (neg rest) in
  let target = log_term (of_poly (embed p)) a_q in
  check_derivative "Elementary.integrate_log" target result;
  result

(* 被積分関数: 有理関数・多項式×log(x-a)・多項式×exp(ax)の3種類しか
   受け取らない。どの場合も、答えの型と同じくa=0のexpを有理関数の別名として
   使っているので、logとexpの2種類だけで表せる *)
type integrand =
  | Rational of Q_poly.t * Q_poly.t
  | Log of Q_poly.t * Rational.t
  | Exp of Rational.t * Q_poly.t

(* 積分。被積分関数の形に応じて、3つのアルゴリズムのどれを使うかを
   その場で振り分けるだけで、呼び出す側は分岐を意識しなくてよい *)
let integrate (i: integrand): t =
  match i with
  | Rational (p, q) -> integrate_rational p q
  | Log (p, a) -> integrate_log p a
  | Exp (a, p) -> integrate_exp a p
