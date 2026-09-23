module Dim = struct
  let dim = 3
end
include Numeric_vector_space.Make(Dim)(Rational)
