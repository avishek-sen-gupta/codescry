"""Shared types for package parsers."""

from dataclasses import dataclass


@dataclass(frozen=True)
class ParsedDependency:
    name: str  # e.g. "spring-boot-starter-web", "fastapi", "gin-gonic/gin"
    source: str  # e.g. "pyproject.toml", "pom.xml"
