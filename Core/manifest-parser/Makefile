# Makefile for WP Praxis Manifest Parser
# LFE/Erlang build automation

.PHONY: all compile test clean dialyzer xref docs shell release help

# Default target
all: compile

# Help target
help:
	@echo "WP Praxis Manifest Parser - Build Targets:"
	@echo ""
	@echo "  make compile    - Compile LFE modules"
	@echo "  make test       - Run test suite"
	@echo "  make clean      - Clean build artifacts"
	@echo "  make dialyzer   - Run Dialyzer type checking"
	@echo "  make xref       - Run cross-reference analysis"
	@echo "  make docs       - Generate documentation"
	@echo "  make shell      - Start LFE REPL with project loaded"
	@echo "  make release    - Build production release"
	@echo "  make check      - Run all checks (dialyzer + xref)"
	@echo "  make ci         - Run CI pipeline (compile + test + check)"
	@echo "  make examples   - Parse example manifests"
	@echo "  make install    - Install dependencies"
	@echo ""

# Compile LFE modules
compile:
	@echo "==> Compiling LFE modules..."
	rebar3 compile

# Run tests
test: compile
	@echo "==> Running test suite..."
	rebar3 lfe test

# Clean build artifacts
clean:
	@echo "==> Cleaning build artifacts..."
	rebar3 clean
	rm -rf _build ebin .rebar3 rebar.lock
	find . -name "*.beam" -delete
	find . -name "*.app" -delete

# Run Dialyzer (static analysis)
dialyzer:
	@echo "==> Running Dialyzer..."
	rebar3 dialyzer

# Run cross-reference checks
xref:
	@echo "==> Running cross-reference analysis..."
	rebar3 xref

# Combined static analysis
check: dialyzer xref
	@echo "==> All checks completed"

# Generate documentation
docs:
	@echo "==> Generating documentation..."
	rebar3 edoc
	@echo "Documentation generated in doc/"

# Start LFE REPL
shell: compile
	@echo "==> Starting LFE REPL..."
	rebar3 lfe repl

# Build production release
release: compile test
	@echo "==> Building production release..."
	rebar3 as prod release
	@echo "Release created in _build/prod/rel/manifest_parser"

# Install dependencies
install:
	@echo "==> Installing dependencies..."
	rebar3 get-deps
	rebar3 compile

# Upgrade dependencies
upgrade:
	@echo "==> Upgrading dependencies..."
	rebar3 upgrade

# CI pipeline
ci: clean install compile test check
	@echo "==> CI pipeline completed successfully"

# Parse example manifests (for testing)
examples: compile
	@echo "==> Parsing example manifests..."
	@echo "Parsing simple.yaml..."
	rebar3 lfe run -- parse examples/simple.yaml
	@echo ""
	@echo "Parsing complex.toml..."
	rebar3 lfe run -- parse examples/complex.toml

# Validate example manifests
validate-examples: compile
	@echo "==> Validating example manifests..."
	rebar3 lfe run -- validate examples/simple.yaml
	rebar3 lfe run -- validate examples/complex.toml

# Format code (if LFE formatter available)
format:
	@echo "==> Formatting LFE code..."
	@echo "Note: LFE formatter not standard, manual formatting recommended"

# Code coverage
coverage: compile
	@echo "==> Running tests with coverage..."
	rebar3 cover
	@echo "Coverage report in _build/test/cover/"

# Watch mode (requires entr or similar)
watch:
	@echo "==> Watching for changes..."
	find src test -name "*.lfe" | entr -c make test

# Create distribution archive
dist: release
	@echo "==> Creating distribution archive..."
	tar -czf manifest-parser-0.1.0.tar.gz \
		-C _build/prod/rel \
		manifest_parser
	@echo "Distribution created: manifest-parser-0.1.0.tar.gz"

# Docker build (if Dockerfile exists)
docker:
	@echo "==> Building Docker image..."
	docker build -t wp-praxis/manifest-parser:latest .

# Benchmarks (if available)
benchmark: compile
	@echo "==> Running benchmarks..."
	rebar3 lfe run -- benchmark

# Profile application
profile: compile
	@echo "==> Profiling application..."
	rebar3 lfe run -- profile

# Version bump
bump-version:
	@echo "Current version: 0.1.0"
	@echo "Update version in:"
	@echo "  - src/manifest-parser.app.src"
	@echo "  - README.md"

# Installation verification
verify: compile test
	@echo "==> Verifying installation..."
	rebar3 lfe eval '(application:ensure_all_started (quote manifest_parser))'
	@echo "Installation verified successfully"
