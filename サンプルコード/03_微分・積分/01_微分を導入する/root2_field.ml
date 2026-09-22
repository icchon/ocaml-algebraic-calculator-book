module Root2Poly = struct
  module C = Rational
  module P = Polynomial.Make(C)
  let poly = [C.of_integer (-2); C.zero; C.one] (* x^2 - 2 *)
end

module K = Field_extension.Make(Root2Poly)
