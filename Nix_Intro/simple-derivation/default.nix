{ pkgs ? import <nixpkgs> {} }:

derivation {
  name = "hello-world";
  builder = pkgs.writeShellScript "build-hello" ''
    ${pkgs.coreutils}/bin/mkdir -p $out/bin
    ${pkgs.gcc}/bin/gcc $src -o $out/bin/hello -O2
  '';
  src = ./hello.c;
  system = builtins.currentSystem;
}
