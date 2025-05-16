{
  description = "eshetsrv";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        beamPackages = pkgs.beam_nox.packages.erlang;
        # to update, run: nix run .#rebar3 as test nix lock
        deps = import ./rebar-deps.nix {
          inherit (pkgs) fetchHex fetchFromGitHub fetchgit;
          builder = pkgs.lib.makeOverridable beamPackages.buildRebar3;
          overrides = final: prev: {
            jiffy = prev.jiffy.override { buildPlugins = [ beamPackages.pc ]; };
          };
        };
      in
      rec {
        packages.eshetsrv = beamPackages.rebar3Relx rec {
          pname = "eshetsrv";
          version = "git";
          src = self;

          releaseType = "release";
          profile = "prod";
          beamDeps = builtins.attrValues deps;

          checkPhase = ''
            HOME=. epmd -daemon
            HOME=. rebar3 ct
            HOME=. rebar3 eunit
          '';
          doCheck = true;

          meta.mainProgram = "eshetsrv_release";
        };
        packages.default = packages.eshetsrv;

        devShells.eshetsrv = packages.eshetsrv.overrideAttrs (attrs: {
          nativeBuildInputs = attrs.buildInputs ++ [
            pkgs.nixfmt-rfc-style
          ];
        });
        devShells.default = devShells.eshetsrv;

        packages.rebar3 =
          (beamPackages.rebar3WithPlugins { globalPlugins = [ beamPackages.rebar3-nix ]; }).overrideAttrs
            { meta.mainProgram = "rebar3"; };
      }
    );
}
