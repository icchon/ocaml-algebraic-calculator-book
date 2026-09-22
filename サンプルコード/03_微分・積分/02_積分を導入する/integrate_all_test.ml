module Elementary_expr = Expr.Elementary.Make (Q_rootp) (Quadratic) (Quadratic_poly) (Elementary)

let show name i =
  print_endline ("--- " ^ name ^ " ---");
  (try
     let r = Elementary.integrate i in
     Expr.put (Expr.to_string_of (module Elementary_expr) r)
   with Failure msg -> print_endline ("failwith: " ^ msg))

let () =
  show "1/((x-1)(x-2))"
    (Elementary.Rational ([Rational.one], [(2,1); (-3,1); (1,1)]));
  show "x^2 log(x-1)"
    (Elementary.Log ([Rational.zero; Rational.zero; Rational.one], (1, 1)));
  show "x e^x"
    (Elementary.Exp ((1, 1), [Rational.zero; Rational.one]));
  show "1/(x^3+x+1) (次数3以上、非対応)"
    (Elementary.Rational ([Rational.one], [(1,1); (1,1); (0,1); (1,1)]))
