module Elementary_expr = Expr.Elementary.Make (Q_rootp) (Quadratic) (Quadratic_poly) (Elementary)

let show i =
  (try
     let r = Elementary.integrate i in
     Expr.put (Expr.to_string_of (module Elementary_expr) r)
   with Failure msg -> print_endline ("failwith: " ^ msg))

let () =
  show (Elementary.Rational ([Rational.one], [(2,1); (-3,1); (1,1)]));
  show (Elementary.Log ([Rational.zero; Rational.zero; Rational.one], (1, 1)));
  show (Elementary.Exp ((1, 1), [Rational.zero; Rational.one]));
  (* 1/(x^3+x+1) (次数3以上、非対応) *)
  show (Elementary.Rational ([Rational.one], [(1,1); (1,1); (0,1); (1,1)]))
