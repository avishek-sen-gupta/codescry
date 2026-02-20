"""C# package parsers (.csproj, packages.config)."""

from repo_surveyor.integration_patterns.csharp.package_parser.csproj import (
    parse as parse_csproj,
)
from repo_surveyor.integration_patterns.csharp.package_parser.packages_config import (
    parse as parse_packages_config,
)

__all__ = ["parse_csproj", "parse_packages_config"]
