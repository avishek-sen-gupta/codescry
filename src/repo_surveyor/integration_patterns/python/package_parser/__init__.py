"""Python package parsers (pyproject.toml, requirements.txt, Pipfile, setup.py)."""

from .pyproject_toml import parse as parse_pyproject_toml
from .requirements_txt import parse as parse_requirements_txt
from .pipfile import parse as parse_pipfile
from .setup_py import parse as parse_setup_py

__all__ = [
    "parse_pyproject_toml",
    "parse_requirements_txt",
    "parse_pipfile",
    "parse_setup_py",
]
