"""Evidence check infrastructure for integration signal classification.

Provides contextual evidence checks that **boost** scores (positive weights)
when context confirms I/O, and **suppress** (negative weights) when context
indicates noise.  The framework is extensible: framework-specific checks can
be composed with the universal suppression checks defined here.

Usage:
    from repo_surveyor.integration_concretiser.evidence import (
        EvidenceContext,
        EvidenceCheck,
        evaluate_evidence,
        UNIVERSAL_CHECKS,
    )

    ctx = EvidenceContext(file_path="src/app.py", line_number=10,
                          line_content="requests.get(url)",
                          source_lines=("import requests", "", "requests.get(url)"))
    verdict = evaluate_evidence(ctx, UNIVERSAL_CHECKS, raw_score=0.65)
    print(verdict.adjusted_score)
"""

import re
from collections.abc import Callable
from dataclasses import dataclass

# ---------------------------------------------------------------------------
# Data types
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class EvidenceContext:
    """Minimal context passed to every evidence check."""

    file_path: str  # relative path to file
    line_number: int  # 1-indexed
    line_content: str  # the matched line, stripped
    source_lines: tuple[str, ...]  # all lines of the file


@dataclass(frozen=True)
class EvidenceCheck:
    """A single evidence check: a named callable with a weight.

    Positive weight = boost (confirms integration).
    Negative weight = suppress (indicates noise).
    The callable closes over any parameters it needs.
    """

    name: str
    check: Callable[[EvidenceContext], bool]
    weight: float  # positive = boost, negative = suppress


@dataclass(frozen=True)
class EvidenceCheckResult:
    """Result of a single evidence check evaluation."""

    name: str
    fired: bool
    weight: float


@dataclass(frozen=True)
class EvidenceVerdict:
    """Aggregated result of all evidence checks for one signal."""

    results: tuple[EvidenceCheckResult, ...]
    score_adjustment: float
    original_score: float
    adjusted_score: float


# ---------------------------------------------------------------------------
# Core evaluator
# ---------------------------------------------------------------------------


def evaluate_evidence(
    ctx: EvidenceContext,
    checks: tuple[EvidenceCheck, ...],
    raw_score: float,
) -> EvidenceVerdict:
    """Evaluate all evidence checks and return a clamped adjusted score.

    Iterates *checks*, accumulates weights for fired checks, and clamps
    the resulting score to [0.0, 1.0].
    """
    results: list[EvidenceCheckResult] = []
    adjustment = 0.0

    for ec in checks:
        fired = ec.check(ctx)
        results.append(EvidenceCheckResult(name=ec.name, fired=fired, weight=ec.weight))
        if fired:
            adjustment += ec.weight

    adjusted = max(0.0, min(1.0, raw_score + adjustment))
    return EvidenceVerdict(
        results=tuple(results),
        score_adjustment=adjustment,
        original_score=raw_score,
        adjusted_score=adjusted,
    )


# ---------------------------------------------------------------------------
# Universal suppression checks
# ---------------------------------------------------------------------------

_TEST_PATH_RE = re.compile(
    r"(^|/)(tests?|__tests?__|spec|__spec__|test_|_test\b)" r"|\.test\." r"|\.spec\.",
    re.IGNORECASE,
)

_VENDOR_PATH_RE = re.compile(
    r"(^|/)(vendor|third[_-]?party|node_modules|\.bundle|external)/",
    re.IGNORECASE,
)

_GENERATED_PATH_RE = re.compile(
    r"(^|/)(generated|_generated|auto[_-]?gen|\.generated\.)",
    re.IGNORECASE,
)

_GENERATED_HEADER_RE = re.compile(
    r"(auto[- ]?generated|do not edit|machine generated|code generated)",
    re.IGNORECASE,
)

_CONFIG_PATH_RE = re.compile(
    r"(^|/)\.?config(/|$)" r"|(^|/)(conf|settings)(\.|\b)" r"|(^|/)\.env$",
    re.IGNORECASE,
)

_CONFIG_EXT_RE = re.compile(
    r"\.(cfg|ini|toml|yaml|yml|json|env|properties|conf)$",
    re.IGNORECASE,
)

_STRING_LITERAL_RE = re.compile(
    r"""(['"]).*\1\s*[,;)\]}]?\s*$""" r"|" r'^\s*["\']',
)

_LOG_CALL_RE = re.compile(
    r"\b(log(ger)?|logging|console)\s*\.\s*(debug|info|warn(ing)?|error|critical|log|trace)\s*\(",
    re.IGNORECASE,
)

_CONSTANT_DECL_RE = re.compile(
    r"^\s*[A-Z][A-Z_0-9]+\s*[:=]" r"|" r"\bconst\s+[A-Z][A-Z_0-9]+\s*=",
)


def _in_test_file(ctx: EvidenceContext) -> bool:
    return bool(_TEST_PATH_RE.search(ctx.file_path))


def _in_vendor_dir(ctx: EvidenceContext) -> bool:
    return bool(_VENDOR_PATH_RE.search(ctx.file_path))


def _in_generated_file(ctx: EvidenceContext) -> bool:
    if _GENERATED_PATH_RE.search(ctx.file_path):
        return True
    # Check header comment (first 5 lines)
    header = ctx.source_lines[:5]
    return any(_GENERATED_HEADER_RE.search(line) for line in header)


def _in_config_dir(ctx: EvidenceContext) -> bool:
    return bool(
        _CONFIG_PATH_RE.search(ctx.file_path) or _CONFIG_EXT_RE.search(ctx.file_path)
    )


def _in_string_literal(ctx: EvidenceContext) -> bool:
    return bool(_STRING_LITERAL_RE.search(ctx.line_content))


def _in_log_statement(ctx: EvidenceContext) -> bool:
    return bool(_LOG_CALL_RE.search(ctx.line_content))


def _in_constant_decl(ctx: EvidenceContext) -> bool:
    return bool(_CONSTANT_DECL_RE.search(ctx.line_content))


UNIVERSAL_CHECKS: tuple[EvidenceCheck, ...] = (
    EvidenceCheck(name="in_test_file", check=_in_test_file, weight=-0.15),
    EvidenceCheck(name="in_vendor_dir", check=_in_vendor_dir, weight=-0.25),
    EvidenceCheck(name="in_generated_file", check=_in_generated_file, weight=-0.30),
    EvidenceCheck(name="in_config_dir", check=_in_config_dir, weight=-0.10),
    EvidenceCheck(name="in_string_literal", check=_in_string_literal, weight=-0.30),
    EvidenceCheck(name="in_log_statement", check=_in_log_statement, weight=-0.25),
    EvidenceCheck(name="in_constant_decl", check=_in_constant_decl, weight=-0.20),
)
