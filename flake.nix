{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

  outputs =
    { nixpkgs, ... }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = [
            (pkgs.dune_3.overrideAttrs (_: rec {
              version = "3.24.2";
              src = pkgs.fetchurl {
                url = "https://github.com/ocaml/dune/releases/download/${version}/dune-${version}.tbz";
                hash = "sha256-RyeYaRsCFtr1OHCfD0cDs2F+8krQhmyQlgaLqrpNdio=";
              };
            }))
            pkgs.git
            pkgs.curl
            pkgs.pkg-config
            pkgs.nil
            pkgs.nixfmt
          ];
          buildInputs = [ pkgs.gmp ];
        };
      });

      formatter = forAllSystems (pkgs: pkgs.nixfmt);
    };
}
