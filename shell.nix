{ pkgs, ... }:
let
  elp = pkgs.callPackage ./nix/elp.nix { };
in
pkgs.mkShell {
  buildInputs =
    (with pkgs.beam.packages.erlang_27; [
      erlang
      rebar3
      erlang-ls
    ])
    ++ (with pkgs; [
      twitch-cli
      elp
      nixfmt-rfc-style
    ]);
}
