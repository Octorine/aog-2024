{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell rec {
  nativeBuildInputs = [
  aoc-cli
  guile
  guile-gnutls
  emacsPackages.geiser-guile
  ];

}
