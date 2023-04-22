let
    # tb = builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/refs/tags/22.11.tar.gz;
    tb = builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz;
    pkgs = import tb {};

    shell = pkgs.mkShell {
        buildInputs = [
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-format
            pkgs.nodejs-18_x
            pkgs.git
        ];
    };
in shell
