# Contributing to WP Praxis

Thank you for your interest in contributing to WP Praxis! This document provides guidelines for contributing to the project.

## Code of Conduct

This project adheres to a [Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code.

## TPCF: Tri-Perimeter Contribution Framework

WP Praxis uses the **Tri-Perimeter Contribution Framework (TPCF)** for graduated trust-based access control.

### Perimeter 3: Community Sandbox (CURRENT)

**Status**: **Open to all contributors**

- ✅ **No barriers to entry** - Anyone can contribute
- ✅ **Public forks** - Fork and submit PRs freely
- ✅ **Community review** - Maintainers review all contributions
- ✅ **No signing requirements** - DCO or CLA not required (AGPL already ensures openness)

**What you can contribute**:
- Bug fixes
- Documentation improvements
- New examples and tutorials
- Test coverage improvements
- Performance optimizations
- New features (with discussion first)

### Perimeter 2: Trusted Contributors (FUTURE)

**Status**: Not yet implemented

When the project matures, we may establish a trusted contributor tier with:
- Direct commit access to development branches
- Ability to approve PRs
- Participation in roadmap discussions

**Criteria** (tentative):
- 10+ merged PRs
- 6+ months of consistent contribution
- Domain expertise in one or more components
- Demonstrated alignment with project values

### Perimeter 1: Core Team (FUTURE)

**Status**: Not yet implemented

Future core team privileges:
- Merge access to master branch
- Release management
- Security incident response
- Governance participation

## Getting Started

### 1. Set Up Development Environment

#### Prerequisites

Install the required tools:

```bash
# Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Elixir (via asdf or your package manager)
asdf install elixir latest

# Bun
curl -fsSL https://bun.sh/install | bash

# PowerShell Core
# See https://docs.microsoft.com/en-us/powershell/scripting/install/installing-powershell

# PostgreSQL
# Your OS package manager

# Optional: Racket, LFE/Rebar3
```

#### Clone and Build

```bash
git clone https://github.com/hyperpolymath/wp-praxis.git
cd wp-praxis

# Build all components
./examples/quickstart/quickstart.sh
```

### 2. Find an Issue

- Check [open issues](https://github.com/hyperpolymath/wp-praxis/issues)
- Look for `good first issue` or `help wanted` labels
- Ask questions in [Discussions](https://github.com/hyperpolymath/wp-praxis/discussions)

### 3. Create a Branch

```bash
git checkout -b feature/my-feature
# or
git checkout -b fix/bug-description
```

**Branch naming**:
- `feature/` - New features
- `fix/` - Bug fixes
- `docs/` - Documentation only
- `test/` - Test improvements
- `refactor/` - Code refactoring
- `perf/` - Performance improvements

### 4. Make Changes

Follow the coding standards for each language (see below).

### 5. Test Your Changes

```bash
# Run all tests
pwsh tests/run-tests.ps1 -Suite all

# Run component-specific tests
cd wp_injector && cargo test        # Rust
cd Core/db-schema && mix test       # Elixir
cd plugin && composer test          # PHP
cd SymbolicEngine/swarm && bun test # TypeScript
```

### 6. Commit Your Changes

Use [Conventional Commits](https://www.conventionalcommits.org/):

```bash
git commit -m "feat: add new symbolic operation type"
git commit -m "fix: resolve database connection timeout"
git commit -m "docs: update GraphQL API examples"
```

**Format**:
```
<type>: <subject>

<body>

<footer>
```

**Types**: `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`

### 7. Push and Create PR

```bash
git push origin feature/my-feature
```

Then create a Pull Request on GitHub.

## Coding Standards

### Rust (`wp_injector/`, `Core/injectors/`)

- ✅ Use `cargo fmt` for formatting
- ✅ Run `cargo clippy` and fix all warnings
- ✅ Follow Rust 2021 edition idioms
- ✅ Use proper error types (`anyhow`, `thiserror`)
- ✅ No `unwrap()` in production code
- ✅ Add documentation comments (`///`)
- ✅ Include unit tests

**Example**:
```rust
/// Parses a YAML manifest from a file path.
///
/// # Errors
///
/// Returns an error if the file cannot be read or is invalid YAML.
pub fn parse_manifest(path: &Path) -> Result<Manifest, Error> {
    // Implementation
}
```

### Elixir (`Core/db-schema/`, `Core/cli-wrapper/`)

- ✅ Use `mix format`
- ✅ Follow Elixir style guide
- ✅ Document public functions with `@doc`
- ✅ Use `@spec` for type specifications
- ✅ Use pattern matching
- ✅ Include ExUnit tests

**Example**:
```elixir
@doc """
Creates a new symbol in the database.

## Examples

    iex> SymbolQueries.create_symbol(%{name: "test"})
    {:ok, %Symbol{}}
"""
@spec create_symbol(map()) :: {:ok, Symbol.t()} | {:error, Ecto.Changeset.t()}
def create_symbol(attrs) do
  # Implementation
end
```

### TypeScript (`SymbolicEngine/swarm/`, `SymbolicEngine/dashboard/`, `SymbolicEngine/graphql/`)

- ✅ Use Bun for runtime and tooling
- ✅ Follow TypeScript strict mode
- ✅ Prefer explicit types over `any`
- ✅ Use modern ES modules
- ✅ Add JSDoc comments for complex functions
- ✅ Include Bun tests

**Example**:
```typescript
/**
 * Dispatches a symbolic workflow to the swarm coordinator.
 *
 * @param workflow - The workflow definition
 * @returns A promise resolving to the execution ID
 */
export async function dispatchWorkflow(workflow: Workflow): Promise<string> {
  // Implementation
}
```

### PHP (`plugin/`, `engine/php/`)

- ✅ Follow WordPress coding standards
- ✅ Use proper WordPress hooks (actions/filters)
- ✅ Sanitize and validate all inputs
- ✅ Use WordPress nonce verification
- ✅ Escape all outputs
- ✅ Include PHPUnit tests
- ✅ Add docblocks

**Example**:
```php
/**
 * Executes a symbolic operation.
 *
 * @param Symbol $symbol The symbol to execute.
 * @return array{success: bool, result: mixed} Execution result.
 * @throws SymbolicException If execution fails.
 */
public function execute_symbol( Symbol $symbol ): array {
    // Implementation
}
```

### PowerShell (`SymbolicEngine/core/`)

- ✅ Use PascalCase for function names (Verb-Noun)
- ✅ Include comment-based help
- ✅ Use approved PowerShell verbs
- ✅ Support `-Verbose` and `-WhatIf`
- ✅ Include Pester tests
- ✅ Handle errors with try/catch

**Example**:
```powershell
<#
.SYNOPSIS
    Validates symbolic role definitions.

.DESCRIPTION
    Validates role constraints, permissions, and hierarchy.

.PARAMETER RolePath
    Path to the role definition file.

.EXAMPLE
    Test-SymbolicRole -RolePath "roles.yaml" -Verbose
#>
function Test-SymbolicRole {
    [CmdletBinding(SupportsShouldProcess)]
    param(
        [Parameter(Mandatory)]
        [string]$RolePath
    )

    # Implementation
}
```

### LFE (`Core/manifest-parser/`)

- ✅ Follow LFE/Erlang style
- ✅ Use proper OTP principles
- ✅ Pattern matching for parsing
- ✅ Tail recursion
- ✅ Use specs with Dialyzer support
- ✅ Include LTest tests

### Racket (`Core/introspection/`)

- ✅ Use contracts for function specifications
- ✅ Use proper module system (`#lang racket`)
- ✅ Functional programming style
- ✅ Pattern matching with `match`
- ✅ Include rackunit tests

## Pull Request Process

### Before Submitting

1. ✅ **Tests Pass**: All tests must pass locally
2. ✅ **Linting**: No linter warnings
3. ✅ **Documentation**: Update docs if behavior changes
4. ✅ **Changelog**: Add entry to CHANGELOG.md (under "Unreleased")
5. ✅ **Examples**: Add/update examples if adding features

### PR Description Template

```markdown
## Description
Brief description of the change.

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Breaking change
- [ ] Documentation update

## How Has This Been Tested?
Describe the tests you ran.

## Checklist
- [ ] My code follows the style guidelines
- [ ] I have performed a self-review
- [ ] I have commented my code where necessary
- [ ] I have updated the documentation
- [ ] My changes generate no new warnings
- [ ] I have added tests that prove my fix/feature works
- [ ] New and existing tests pass locally
- [ ] I have updated CHANGELOG.md
```

### Review Process

1. **Automated Checks**: CI must pass (tests, linting)
2. **Code Review**: At least one maintainer review required
3. **Discussion**: Address review comments
4. **Approval**: Maintainer approves PR
5. **Merge**: Squash and merge or rebase

## Reporting Bugs

### Before Reporting

1. Check [existing issues](https://github.com/hyperpolymath/wp-praxis/issues)
2. Try the latest version
3. Search [discussions](https://github.com/hyperpolymath/wp-praxis/discussions)

### Bug Report Template

```markdown
## Describe the Bug
A clear description of what the bug is.

## To Reproduce
Steps to reproduce:
1. Go to '...'
2. Click on '....'
3. See error

## Expected Behavior
What you expected to happen.

## Actual Behavior
What actually happened.

## Environment
- OS: [e.g. Ubuntu 22.04]
- Component: [e.g. Rust injector, PowerShell engine]
- Version: [e.g. 0.1.0]

## Additional Context
Logs, screenshots, or other context.
```

## Requesting Features

### Feature Request Template

```markdown
## Problem Statement
What problem does this feature solve?

## Proposed Solution
How should this work?

## Alternatives Considered
What other solutions did you consider?

## Additional Context
Any other context or examples.
```

## Communication

- **GitHub Issues**: Bug reports, feature requests
- **GitHub Discussions**: Questions, ideas, general discussion
- **Pull Requests**: Code contributions
- **Security**: security@wp-praxis.dev (private)

## License

By contributing, you agree that your contributions will be licensed under the GNU AGPL-3.0 license.

All contributions must be original work or properly attributed with compatible licenses.

## Recognition

Contributors are recognized in:

- Git commit history
- CHANGELOG.md (for significant contributions)
- `.well-known/humans.txt`
- Project website (future)

Thank you for contributing to WP Praxis! 🎉

---

**Questions?** Ask in [Discussions](https://github.com/hyperpolymath/wp-praxis/discussions)
