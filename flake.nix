{
    description = "Vriom text editor";

    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    };

    outputs = { self, nixpkgs } :
        let
            forAllSystems = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
        in
        {
            devShells = forAllSystems (system:
                let
                    pkgs = nixpkgs.legacyPackages.${system};
                    ocamlPackages = pkgs.ocamlPackages;

                    bogue = ocamlPackages.buildDunePackage {
                        pname = "bogue";
                        version = "20250815";
                        src = pkgs.fetchFromGitHub {
                            owner = "sanette";
                            repo = "bogue";
                            rev = "20250815";
                            hash = "sha256-kXAKMBbqFuk3A+anEQ8sT8C+fRsV/rF//g8XTyiQ160=";
                        };
                        propagatedBuildInputs = with ocamlPackages; [
                            xdg
                            tsdl
                            tsdl-ttf
                            tsdl-image
                        ];
                    };
                in
                {
                    default = pkgs.mkShell {
                        BOGUE_DIR = "${bogue}/share/bogue/themes";

                        packages = with pkgs; [
                            pkg-config
                            ocaml
                            SDL2
                            SDL2_ttf
                        ] ++ (with ocamlPackages; [
                            bogue
                            dune_3
                            utop
                            findlib
                            ocaml-lsp
                            ocamlformat
                            tsdl
                            tsdl-ttf
                            uutf
                            uuseg
                        ]);
                    };
                });
        };
}
