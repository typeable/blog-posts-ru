with import ./mul.nix;
let
  fac = n:
    if n == 0
    then 1
    else mul n (fac (n - 1));
in { inherit fac; }