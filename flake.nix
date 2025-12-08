{
  description = "WP Praxis - Symbolic Workflow System for WordPress";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };

        # Rust toolchain
        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" "rust-analyzer" ];
        };

        # Common build inputs across all languages
        commonInputs = with pkgs; [
          # Version control
          git

          # Build tools
          just
          gnumake

          # Shell
          bash
        ];

        # Rust dependencies
        rustInputs = with pkgs; [
          rustToolchain
          pkg-config
          openssl
        ];

        # Elixir dependencies
        elixirInputs = with pkgs; [
          elixir
          erlang
        ];

        # TypeScript/Bun dependencies
        typescriptInputs = with pkgs; [
          bun
          nodejs # For compatibility
        ];

        # PHP dependencies
        phpInputs = with pkgs; [
          php82
          php82Packages.composer
        ];

        # PowerShell dependencies
        powershellInputs = with pkgs; [
          powershell
        ];

        # Database dependencies
        databaseInputs = with pkgs; [
          postgresql_15
          sqlite
          mysql80
        ];

        # LFE/Racket dependencies
        lispInputs = with pkgs; [
          rebar3
          racket
        ];

        # All development dependencies
        devInputs = commonInputs
          ++ rustInputs
          ++ elixirInputs
          ++ typescriptInputs
          ++ phpInputs
          ++ powershellInputs
          ++ databaseInputs
          ++ lispInputs;

      in
      {
        # Development shell
        devShells.default = pkgs.mkShell {
          buildInputs = devInputs;

          shellHook = ''
            echo "WP Praxis Development Environment"
            echo "=================================="
            echo ""
            echo "Installed tools:"
            echo "  - Rust: $(rustc --version | cut -d' ' -f2)"
            echo "  - Elixir: $(elixir --version | grep Elixir | cut -d' ' -f2)"
            echo "  - Bun: $(bun --version)"
            echo "  - PHP: $(php --version | head -1 | cut -d' ' -f2)"
            echo "  - PowerShell: $(pwsh --version)"
            echo "  - PostgreSQL: $(psql --version | cut -d' ' -f3)"
            echo "  - Just: $(just --version | cut -d' ' -f2)"
            echo ""
            echo "Quick start:"
            echo "  just install-deps   # Install all dependencies"
            echo "  just build-all      # Build all components"
            echo "  just test-all       # Run all tests"
            echo "  just --list         # Show all available tasks"
            echo ""
            export RUST_SRC_PATH="${rustToolchain}/lib/rustlib/src/rust/library"
          '';

          # Environment variables
          DATABASE_URL = "postgresql://localhost/wp_praxis_dev";
          MIX_ENV = "dev";
          NODE_ENV = "development";
        };

        # CI shell (minimal dependencies for CI/CD)
        devShells.ci = pkgs.mkShell {
          buildInputs = [
            rustToolchain
            pkgs.elixir
            pkgs.bun
            pkgs.php82
            pkgs.powershell
            pkgs.just
            pkgs.postgresql_15
          ];

          shellHook = ''
            echo "WP Praxis CI Environment"
          '';
        };

        # Production build (future - not yet implemented)
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "wp-praxis";
          version = "0.1.0";

          src = ./.;

          buildInputs = devInputs;

          buildPhase = ''
            echo "Building WP Praxis..."
            just build-all
          '';

          installPhase = ''
            mkdir -p $out/bin
            # Copy built artifacts
            cp -r wp_injector/target/release/wp_injector $out/bin/ || true
            # Copy other components
            cp -r SymbolicEngine $out/
            cp -r Core $out/
            cp -r plugin $out/
            cp -r engine $out/
          '';

          meta = with pkgs.lib; {
            description = "Symbolic Workflow System for WordPress";
            homepage = "https://github.com/hyperpolymath/wp-praxis";
            license = licenses.agpl3Plus;
            maintainers = [ ];
            platforms = platforms.unix;
          };
        };

        # Formatter for nix files
        formatter = pkgs.nixpkgs-fmt;

        # Apps that can be run with `nix run`
        apps = {
          # Run the symbolic engine
          engine = flake-utils.lib.mkApp {
            drv = pkgs.writeShellScriptBin "wp-praxis-engine" ''
              ${pkgs.powershell}/bin/pwsh SymbolicEngine/core/symbolic.ps1 -Operation Interactive
            '';
          };

          # Run tests
          test = flake-utils.lib.mkApp {
            drv = pkgs.writeShellScriptBin "wp-praxis-test" ''
              ${pkgs.just}/bin/just test-all
            '';
          };

          # Run validation
          validate = flake-utils.lib.mkApp {
            drv = pkgs.writeShellScriptBin "wp-praxis-validate" ''
              ${pkgs.just}/bin/just validate-rsr
            '';
          };
        };
      }
    );
}
