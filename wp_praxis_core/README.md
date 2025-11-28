# WP Praxis Core

**Offline-capable core library for WP Praxis symbolic workflows**

[![License: AGPL-3.0](https://img.shields.io/badge/License-AGPL%203.0-blue.svg)](LICENSE)
[![Rust](https://img.shields.io/badge/rust-1.70+-orange.svg)](https://www.rust-lang.org/)
[![RSR Bronze](https://img.shields.io/badge/RSR-Bronze-orange)](../RSR_COMPLIANCE.md)

## Overview

`wp_praxis_core` is a zero-dependency Rust library for parsing, validating, and working with WP Praxis symbolic workflow manifests. It is designed to work completely **offline** with no network dependencies in its default configuration.

## Features

- ✅ **Zero Dependencies** (default mode): No network, no database, pure Rust
- ✅ **Offline-First**: Embedded YAML/TOML parsers
- ✅ **Type Safety**: Full Rust type system enforcement
- ✅ **Memory Safety**: No `unsafe` code
- ✅ **Validation Engine**: Comprehensive manifest validation
- ✅ **Dependency Resolution**: DAG validation for symbol dependencies
- ✅ **Feature Flags**: Optional network and database support

## Feature Flags

### Default (`offline`)
```toml
[dependencies]
wp_praxis_core = "0.1.0"
```
**Zero dependencies** - Offline manifest parsing and validation only.

### Network Features
```toml
[dependencies]
wp_praxis_core = { version = "0.1.0", features = ["network"] }
```
Adds HTTP client for fetching remote manifests.

### Full-Stack Features
```toml
[dependencies]
wp_praxis_core = { version = "0.1.0", features = ["full-stack"] }
```
Adds database support (SQLx, SQLite, MySQL).

## Usage

### Basic Example (Offline)

```rust
use wp_praxis_core::{Manifest, Symbol, SymbolType, Operation, ValidationEngine};

// Parse a manifest from YAML (completely offline)
let yaml = r#"
name: example
version: 1.0.0
symbols:
  - name: set_site_title
    type: option
    operation: set
    target: blogname
    value: My Awesome Site
"#;

let manifest = Manifest::from_yaml(yaml)?;

// Validate the manifest
let engine = ValidationEngine::new();
let result = engine.validate(&manifest)?;

if result.is_valid {
    println!("✓ Manifest is valid");
} else {
    for error in result.errors() {
        eprintln!("ERROR: {}", error);
    }
}
```

### Creating Manifests Programmatically

```rust
use wp_praxis_core::{Manifest, Symbol, SymbolType, Operation};

let mut manifest = Manifest::new("my-workflow", "1.0.0");
manifest.description = Some("My WordPress workflow".to_string());

let symbol = Symbol::new("set_option", SymbolType::Option, Operation::Set)
    .with_target("my_option")
    .with_value("my_value")
    .with_tag("configuration");

manifest.add_symbol(symbol);

// Serialize to YAML or TOML
let yaml = manifest.to_yaml()?;
let toml = manifest.to_toml()?;
```

### Validation

```rust
use wp_praxis_core::{Manifest, ValidationEngine};

let manifest = Manifest::from_yaml(yaml_content)?;
let engine = ValidationEngine::new();
let result = engine.validate(&manifest)?;

// Check for errors
if !result.is_valid {
    for error in result.errors() {
        println!("ERROR: {}", error);
    }
}

// Check for warnings
for warning in result.warnings() {
    println!("WARN: {}", warning);
}
```

The validation engine checks:
- ✅ Required fields (name, version)
- ✅ Symbol validity
- ✅ Duplicate symbol names
- ✅ Circular dependencies (DAG validation)
- ✅ Missing dependencies
- ✅ Semantic versioning format
- ✅ Operation-specific requirements (e.g., `set` needs a value)
- ✅ Destructive operations have rollback strategies

## Examples

Run the examples with:

```bash
# Offline validation example
cargo run --example validate_offline

# Parsing example (YAML and TOML)
cargo run --example parse_manifest
```

## Architecture

```
wp_praxis_core/
├── src/
│   ├── lib.rs              # Main library entry point
│   ├── manifest.rs         # Manifest data structures
│   ├── symbol.rs           # Symbol types and operations
│   ├── validation.rs       # Validation engine
│   ├── parser/
│   │   ├── mod.rs          # Parser interface
│   │   ├── yaml.rs         # Embedded YAML parser
│   │   └── toml.rs         # Embedded TOML parser
│   ├── network.rs          # Network features (optional)
│   └── database.rs         # Database features (optional)
├── examples/               # Usage examples
└── tests/                  # Integration tests
```

## Symbol Types

Supported WordPress symbol types:
- `Option` - WordPress options
- `PostMeta` - Post metadata
- `UserMeta` - User metadata
- `TermMeta` - Term metadata
- `CustomPostType` - Custom post types
- `Taxonomy` - Taxonomies
- `Plugin` - Plugin activation/deactivation
- `Theme` - Theme modifications
- `Query` - Database queries
- `Action` - WordPress actions
- `Filter` - WordPress filters

## Operations

Supported operations:
- `Set` - Create/set a value
- `Update` - Modify an existing value
- `Delete` - Remove a value (destructive)
- `Get` - Query a value (read-only)
- `Register` - Register a component
- `Unregister` - Unregister a component (destructive)
- `Activate` - Activate a feature
- `Deactivate` - Deactivate a feature (destructive)

## Rollback Strategies

For destructive operations:
- `None` - No rollback
- `StorePrevious` - Store previous value for restoration
- `Backup` - Create a backup before modification
- `Custom(String)` - Custom rollback logic

## RSR Compliance

This crate is designed to achieve **RSR Bronze** compliance:

- ✅ **Type Safety**: Full Rust type system
- ✅ **Memory Safety**: No `unsafe` code
- ✅ **Offline-First**: Zero network dependencies (default)
- ✅ **Zero Dependencies**: No external crates in default mode
- ✅ **Reproducible**: Deterministic parsing and validation
- ✅ **Well-Documented**: Comprehensive docs and examples

## Testing

```bash
# Run all tests
cargo test

# Run with coverage
cargo test --all-features

# Run integration tests
cargo test --test '*'
```

## License

GNU AGPL-3.0-or-later

## Contributing

See [CONTRIBUTING.md](../CONTRIBUTING.md) for contribution guidelines.

## Security

See [SECURITY.md](../SECURITY.md) for security policy and vulnerability reporting.

## See Also

- [WP Praxis](../) - Main project repository
- [RSR Compliance Report](../RSR_COMPLIANCE.md) - Full RSR assessment
- [Rhodium Platinum Roadmap](../RHODIUM_PLATINUM_ROADMAP.md) - Compliance roadmap
