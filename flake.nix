{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }@inputs:

    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs
          {
            inherit system;
          };
      in
      {
        devShell = with pkgs;
          mkShell {
            inherit system;
            packages = [
              shfmt
              shellcheck
              nodePackages.bash-language-server
            ];
          };
      }
    );
}
