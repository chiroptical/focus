{pkgs, ...}:
pkgs.mkShell {
  buildInputs = with pkgs.beam.packages.erlang_27; [
    erlang
    rebar3
    erlang-ls
  ];
}
