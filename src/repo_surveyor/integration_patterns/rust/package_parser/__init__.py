"""Rust package parser (Cargo.toml)."""

from .cargo_toml import parse as parse_cargo_toml

__all__ = ["parse_cargo_toml"]
