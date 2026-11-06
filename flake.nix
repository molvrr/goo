{
  inputs = {
    nixpkgs.url = "github:nix-ocaml/nix-overlays";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      ocamlPkgs = pkgs.ocamlPackages;
    in {
      formatter = pkgs.alejandra;

      devShells.default = pkgs.mkShell {
        nativeBuildInputs = with ocamlPkgs; [
          ocaml
          dune_3
          ocaml-lsp
          ocamlformat
          findlib
          pkgs.pkg-config
        ];

        # Tests: a throwaway Postgres for caqti-goo, curl for test_httpun_goo.
        buildInputs = [pkgs.postgresql pkgs.curl];

        propagatedBuildInputs = with ocamlPkgs; [
          # web, and the vendored httpun / httpun-ws / gluten (see vendor/)
          uri
          logs
          fmt
          digestif
          base64
          angstrom
          faraday
          bigstringaf
          httpun-types
          # datastar
          yojson
          # caqti-goo
          caqti
          caqti-driver-postgresql
          mtime
          ipaddr
          domain-name
        ];
      };
    });
}
