{
  description = "debugger for Hubris";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, rust-overlay }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "armv7l-linux" "x86_64-darwin" "aarch64-darwin" ];
      overlays = [ (import rust-overlay) ];
      forAllSystems = function:
        nixpkgs.lib.genAttrs systems
          (system:
            let
              pkgs = import nixpkgs {
                inherit system overlays;
              };
            in
            function pkgs);
      pname = "humility";
      build-humility =
        (pkgs: with pkgs; let

          udevFilename = "69-probe-rs.rules";
          # fetch the probe-rs website to use their list of udev rules for all
          # debug probes supported by probe-rs
          probeRsWebsite = fetchFromGitHub
            {
              owner = "probe-rs";
              repo = "webpage";
              rev = "2617971b3584bae9c227cd026aaaabdc45a308a3";
              hash = "sha256-ur5oGb6aypce6h0YPvCKc/8/X7FBfzOQS9zlkLP2UGY=";
            };

          # use the Rust toolchain specified in the project's rust-toolchain.toml
          rustToolchain =
            let
              file = pkgsBuildHost.rust-bin.fromRustupToolchainFile
                ./rust-toolchain.toml;
            in
            file.override {
              extensions = [
                "rust-src" # for rust-analyzer
              ];
            };

          configuredRustPlatform = makeRustPlatform {
            cargo = rustToolchain;
            rustc = rustToolchain;
          };

          src = nix-gitignore.gitignoreSource [ ] ./.;
          cargoTOML = lib.importTOML "${src}/Cargo.toml";
        in
        configuredRustPlatform.buildRustPackage {
          inherit src pname;
          inherit (cargoTOML.package) version;

          buildInputs =
            if stdenv.isLinux then [
              libudev-zero
            ] else [ ];
          nativeBuildInputs = if stdenv.isLinux then [ pkg-config ] else [ ];

          cargoLock.lockFile = ./Cargo.lock;
          cargoLock.outputHashes = {
            "capstone-0.10.0" = "sha256-x0p005W6u3QsTKRupj9HEg+dZB3xCXlKb9VCKv+LJ0U=";
            "gimlet-inspector-protocol-0.1.0" = "sha256-NLKiYL1CMkQaaTP0ePwEK49Y9lckkOrzw7371SHHEWQ=";
            "hidapi-1.4.1" = "sha256-2SBQu94ArGGwPU3wJYV0vwwVOXMCCq+jbeBHfKuE+pA=";
            "hif-0.3.1" = "sha256-o3r1akaSARfqIzuP86SJc6/s0b2PIkaZENjYO3DPAUo=";
            "humpty-0.1.3" = "sha256-efeb+RaAjQs9XU3KkfVo8mVK2dGyv+2xFKSVKS0vyTc=";
            "idol-0.3.0" = "sha256-s6ZM/EyBE1eOySPah5GtT0/l7RIQKkeUPybMmqUpmt8=";
            "idt8a3xxxx-0.1.0" = "sha256-S36fS9hYTIn57Tt9msRiM7OFfujJEf8ED+9R9p0zgK4=";
            "libusb1-sys-0.5.0" = "sha256-7Bb1lpZvCb+OrKGYiD6NV+lMJuxFbukkRXsufaro5OQ=";
            "pmbus-0.1.2" = "sha256-NFSrh4yD7PCqYhGuioRYWFmFIcpFvDO1qh6Lp9tsJ9E=";
            "probe-rs-0.12.0" = "sha256-/L+85K6uxzUmz/TlLLFbMlyekoXC/ClO33EQ/yYjQKU=";
            "spd-0.1.0" = "sha256-X6XUx+huQp77XF5EZDYYqRqaHsdDSbDMK8qcuSGob3E=";
            "tlvc-0.2.0" = "sha256-HiqDRqmKOTxz6UQSXNMOZdWdc5W+cFGuKBkNrqFvIIE=";
            "vsc7448-info-0.1.0" = "sha256-otNLdfGIzuyu03wEb7tzhZVVMdS0of2sU/AKSNSsoho=";
          };

          # Copy the probe-rs udev rules into `etc/udev/rules.d` so that this
          # package can be added to `services.udev.packages` on NixOS.
          postInstall = ''
            mkdir -p $out/etc/udev/rules.d
            cp ${probeRsWebsite}/src/static/files/${udevFilename} $out/etc/udev/rules.d/${udevFilename}
          '';

          meta = {
            inherit (cargoTOML.package) homepage license;
            description = "debugger for Hubris";
            mainProgram = pname;
          };
        });
    in
    {
      ########################################################################
      #### Packages
      ########################################################################
      packages = forAllSystems (pkgs: with pkgs; {
        humility = build-humility pkgs;
        default = self.packages.${system}.humility;
        humility-cross-armv7l-linux =
          build-humility pkgsCross.armv7l-hf-multiplatform;
        humility-cross-aarch64-linux =
          build-humility pkgsCross.aarch64-multiplatform;
      });

      ########################################################################
      #### Flake app (for `nix run`)
      ########################################################################
      apps = forAllSystems
        (pkgs: with pkgs; {
          humility = {
            type = "app";
            program = "${self.packages.${system}.humility}/bin/${pname}";
          };
          default = self.apps.${system}.humility;
        });

      ########################################################################
      #### Dev shell (for `nix develop`)
      ########################################################################
      devShells = forAllSystems
        (pkgs: with pkgs; let humilityPackage = self.packages.${system}.humility; in {
          default = mkShell {
            buildInputs = [ humilityPackage humilityPackage.buildInputs ];
            nativeBuildInputs = [ humilityPackage.nativeBuildInputs ];
          };
        });

      ########################################################################
      #### NixOS module (`programs.humility`)
      ########################################################################
      nixosModules.default = { config, lib, pkgs, ... }: with lib; let
        cfg = config.programs.humility;
      in
      {
        options.programs.humility = {
          enable = mkEnableOption "Humility, the debugger for Hubris";
          users = mkOption {
            type = types.listOf types.str;
            default = [ ];
            description = mdDoc ''
              List of users added to the "plugdev" group,
              allowing access to debug probes used by Humility.
            '';
          };
        };

        config = mkIf cfg.enable {
          environment.systemPackages = [ pkgs.humility ];
          services.udev.packages = [ pkgs.humility ];
          users.groups.plugdev = {
            members = cfg.users;
          };
        };
      };

    };
}
