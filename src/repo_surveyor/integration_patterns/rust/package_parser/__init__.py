"""Rust package parser (Cargo.toml)."""

from repo_surveyor.integration_patterns.rust.package_parser.cargo_toml import (
    parse as parse_cargo_toml,
)

__all__ = ["parse_cargo_toml"]
