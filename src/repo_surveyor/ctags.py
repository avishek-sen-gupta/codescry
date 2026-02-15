"""CTags integration for extracting code symbols from repositories."""

import json
import subprocess
from dataclasses import dataclass, field
from pathlib import Path


@dataclass
class CTagsConfig:
    """Configuration for running CTags."""

    languages: list[str] = field(default_factory=list)
    exclude_patterns: list[str] = field(
        default_factory=lambda: [
            ".git",
            ".idea",
            "target",
            "node_modules",
            "__pycache__",
            ".venv",
            "venv",
        ]
    )
    # extra_fields: str = "+n+k+S+z+K+l"
    extra_fields: str = "*"
    extras: str = "+q"
    verbose: bool = False


@dataclass
class CTagsEntry:
    """A single CTags entry representing a code symbol."""

    name: str
    path: str
    kind: str
    line: int
    scope: str
    scope_kind: str
    signature: str
    language: str

    @classmethod
    def from_json(cls, data: dict) -> "CTagsEntry":
        """Create a CTagsEntry from CTags JSON output."""
        return cls(
            name=data.get("name", ""),
            path=data.get("path", ""),
            kind=data.get("kind", ""),
            line=data.get("line"),
            scope=data.get("scope"),
            scope_kind=data.get("scopeKind"),
            signature=data.get("signature"),
            language=data.get("language"),
        )


@dataclass
class CTagsResult:
    """Result of running CTags on a repository."""

    entries: list[CTagsEntry]
    raw_output: str
    return_code: int
    error_output: str = ""

    @property
    def success(self) -> bool:
        """Check if CTags ran successfully."""
        return self.return_code == 0


def run_ctags(repo_path: Path, config: CTagsConfig) -> CTagsResult:
    """Run CTags on a repository and return parsed results.

    Args:
        repo_path: Path to the repository to analyze.
        config: Optional configuration for CTags. Uses defaults if not provided.

    Returns:
        CTagsResult containing parsed entries and execution metadata.

    Raises:
        FileNotFoundError: If ctags is not installed or not in PATH.
    """

    cmd = _build_ctags_command(config)

    try:
        result = subprocess.run(
            cmd,
            cwd=repo_path,
            capture_output=True,
            text=True,
            timeout=300,
        )
    except FileNotFoundError:
        raise FileNotFoundError(
            "ctags not found. Please install Universal CTags: "
            "https://github.com/universal-ctags/ctags"
        )
    except subprocess.TimeoutExpired:
        return CTagsResult(
            entries=[],
            raw_output="",
            return_code=-1,
            error_output="CTags timed out after 300 seconds",
        )

    entries = _parse_ctags_json_output(result.stdout)

    return CTagsResult(
        entries=entries,
        raw_output=result.stdout,
        return_code=result.returncode,
        error_output=result.stderr,
    )


def _build_ctags_command(config: CTagsConfig) -> list[str]:
    """Build the ctags command from configuration."""
    cmd = [
        "ctags",
        "--output-format=json",
        f"--fields={config.extra_fields}",
        f"--extras={config.extras}",
        "-R",
    ]

    if config.languages:
        cmd.append(f"--languages={','.join(config.languages)}")

    cmd.extend(f"--exclude={pattern}" for pattern in config.exclude_patterns)

    if config.verbose:
        cmd.append("--verbose=yes")

    return cmd


def _try_parse_json(line: str) -> dict | None:
    """Attempt to parse a JSON line, returning None on failure."""
    try:
        return json.loads(line)
    except json.JSONDecodeError:
        return None


def _parse_ctags_json_output(output: str) -> list[CTagsEntry]:
    """Parse CTags JSON output into CTagsEntry objects.

    CTags JSON output is one JSON object per line (JSON Lines format).
    """
    return [
        CTagsEntry.from_json(data)
        for line in output.strip().split("\n")
        if line and (data := _try_parse_json(line)) and data.get("_type") == "tag"
    ]
