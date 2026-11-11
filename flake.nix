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

      deps = p:
        with p; [
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
          tls
          x509
          ca-certs-nss
          mirage-crypto-rng
          # datastar
          yojson
          # caqti-goo
          caqti
          caqti-driver-postgresql
          mtime
          ipaddr
          domain-name
        ];

      packageNames = ["goo" "goo-server" "html" "csp" "datastar" "web" "caqti-goo"];
      # Always built (dune never masks vendored packages); installed alongside
      # [web], which is compiled against them.
      vendoredNames = ["httpun" "httpun-ws" "gluten"];

      # All of it as one derivation: the vendored libraries can only be built
      # from inside this workspace. Built with [p], so consumers can match
      # their own OCaml package set.
      mkGoo = p:
        p.buildDunePackage {
          pname = "goo";
          version = "dev";
          src = ./.;
          propagatedBuildInputs = deps p;
          buildPhase = ''
            runHook preBuild
            dune build -p ${pkgs.lib.concatStringsSep "," packageNames} -j $NIX_BUILD_CORES \
              @install ${pkgs.lib.concatMapStringsSep " " (n: "vendor/${n}/${n}.install") vendoredNames}
            runHook postBuild
          '';
          installPhase = ''
            runHook preInstall
            dune install --prefix $out --libdir $OCAMLFIND_DESTDIR ${pkgs.lib.concatStringsSep " " (packageNames ++ vendoredNames)}
            runHook postInstall
          '';
        };
    in {
      formatter = pkgs.alejandra;

      packages.default = mkGoo ocamlPkgs;
      lib = {inherit mkGoo;};

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

        propagatedBuildInputs = deps ocamlPkgs;
      };
    });
}
