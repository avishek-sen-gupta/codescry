"""JavaScript package parser (package.json)."""

from .package_json import parse as parse_package_json

__all__ = ["parse_package_json"]
