"""C# package parsers (.csproj, packages.config)."""

from .csproj import parse as parse_csproj
from .packages_config import parse as parse_packages_config

__all__ = ["parse_csproj", "parse_packages_config"]
