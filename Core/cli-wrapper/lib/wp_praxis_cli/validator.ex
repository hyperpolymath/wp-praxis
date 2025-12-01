# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 WP Praxis Contributors

defmodule WpPraxisCli.Validator do
  @moduledoc """
  Validates manifests using wp_praxis_core (Rust) via Port.

  This module acts as a bridge between Elixir and the Rust
  validation engine.
  """

  @doc """
  Validate a manifest file.

  Returns `{:ok, result}` or `{:error, errors}`.
  """
  def validate_file(path, opts \\ []) do
    strict = Keyword.get(opts, :strict, false)

    # Call wp_praxis_core validation (via FFI or Port)
    # For now, return placeholder
    # TODO: Integrate with wp_praxis_core Rust library

    {:ok, %{
      name: "placeholder",
      version: "0.0.0",
      symbols: [],
      warnings: []
    }}
  end
end
