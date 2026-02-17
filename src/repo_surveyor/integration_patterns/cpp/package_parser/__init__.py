"""C++ package parsers (vcpkg.json, conanfile.txt)."""

from .vcpkg_json import parse as parse_vcpkg_json
from .conanfile_txt import parse as parse_conanfile_txt

__all__ = ["parse_vcpkg_json", "parse_conanfile_txt"]
