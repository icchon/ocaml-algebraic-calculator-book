module Elementary_expr = Expr.Elementary.Make (Q_rootp) (Quadratic) (Quadratic_poly) (Elementary)

let integrand_expr = function
  | Elementary.Rational (p, q) -> Expr.frac (Expr.Q_poly.to_expr p) (Expr.Q_poly.to_expr q)
  | Elementary.Log (p, a) ->
    Expr.product [
      Expr.Q_poly.to_expr p;
      Expr.fn "log" [Expr.sum [Expr.atom "x"; Expr.neg (Expr.Rational.to_expr a)]];
    ]
  | Elementary.Exp (a, p) ->
    Expr.product [
      Expr.Q_poly.to_expr p;
      Expr.pow (Expr.atom "e") (Expr.product [Expr.Rational.to_expr a; Expr.atom "x"]);
    ]

let show i =
  (try
     let r = Elementary.integrate i in
     Expr.put (Expr.integral_to_string "x" (integrand_expr i)
       (Expr.to_string_of (module Elementary_expr) r))
   with Failure msg -> print_endline ("failwith: " ^ msg))

(* 有理関数・多項式かけるlog・多項式かけるexpがすべて足し合わされた被積分関数を、
   項ごとにintegrateしてから足し合わせる（積分の線形性） *)
let show_sum items =
  let combined = List.fold_left
    (fun acc i -> Elementary.add acc (Elementary.integrate i))
    Elementary.zero items
  in
  let integrand = Expr.sum (List.map integrand_expr items) in
  Expr.put (Expr.integral_to_string "x" integrand
    (Expr.to_string_of (module Elementary_expr) combined))

let () =
  show_sum [
    Elementary.Rational ([Rational.one], [(2,1); (-3,1); (1,1)]);
    Elementary.Log ([Rational.zero; Rational.zero; Rational.one], (1, 1));
    Elementary.Exp ((1, 1), [Rational.zero; Rational.one]);
  ];
  (* 1/(x^3+x+1) (次数3以上、非対応) *)
  show (Elementary.Rational ([Rational.one], [(1,1); (1,1); (0,1); (1,1)]))
