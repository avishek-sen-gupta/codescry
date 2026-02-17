"""Go package parser (go.mod)."""

from .go_mod import parse as parse_go_mod

__all__ = ["parse_go_mod"]
