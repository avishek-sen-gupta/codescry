"""DOT-to-SVG rendering via Graphviz CLI.

Provides a Protocol for DOT rendering and two implementations:
- ``GraphvizCliRenderer`` — calls the ``dot`` binary via subprocess.
- ``NullDotRenderer`` — null-object that returns input unchanged (for tests).
"""

import subprocess
from dataclasses import dataclass
from typing import Protocol


class DotRenderer(Protocol):
    """Protocol for rendering DOT source into SVG."""

    def render_svg(self, dot_source: str) -> str: ...


@dataclass(frozen=True)
class GraphvizCliRenderer:
    """Renders DOT source to SVG by invoking the Graphviz ``dot`` binary."""

    dot_binary: str = "dot"

    def render_svg(self, dot_source: str) -> str:
        """Run ``dot -Tsvg`` and return the SVG string."""
        result = subprocess.run(
            [self.dot_binary, "-Tsvg"],
            input=dot_source,
            capture_output=True,
            text=True,
            check=True,
        )
        return result.stdout


@dataclass(frozen=True)
class NullDotRenderer:
    """No-op renderer that returns the input unchanged."""

    def render_svg(self, dot_source: str) -> str:
        return dot_source
