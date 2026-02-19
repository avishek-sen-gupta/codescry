"""Harvest real code examples from GitHub to build classifier training data.

Uses the pattern registry as the source of search queries: each HIGH-confidence,
non-AMBIGUOUS pattern becomes a GitHub code-search query.  The pattern's
SignalDirection provides the label directly (INWARD → DEFINITE_INWARD,
OUTWARD → DEFINITE_OUTWARD), so no LLM call is required.

Harvested examples are validated and exported with the same stratified
train/val/test split used by the synthetic generator, ready to be merged
with or used in place of the synthetic dataset.

Usage:
    poetry run python scripts/harvest_github.py --token $GITHUB_TOKEN
    poetry run python scripts/harvest_github.py --token $GITHUB_TOKEN --languages Java Python
    poetry run python scripts/harvest_github.py --token $GITHUB_TOKEN --dry-run
    poetry run python scripts/harvest_github.py --token $GITHUB_TOKEN --resume
"""

import argparse
import base64
import hashlib
import json
import logging
import re
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Iterator
from urllib.parse import quote
from urllib.request import Request, urlopen
from urllib.error import HTTPError, URLError

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from repo_surveyor.integration_patterns import (
    LANGUAGE_MODULES,
    get_patterns_for_language,
)
from repo_surveyor.integration_patterns.types import (
    Confidence,
    IntegrationType,
    Language,
    SignalDirection,
)
from repo_surveyor.training.exporter import export_training_data
from repo_surveyor.training.types import TrainingExample, TrainingLabel
from repo_surveyor.training.validator import validate_batch

logger = logging.getLogger(__name__)

_DEFAULT_OUTPUT_DIR = Path("data/github_harvest")
_DEFAULT_MAX_FILES = 10
_DEFAULT_CONTEXT_LINES = 15
_CHECKPOINT_FILE = "checkpoint.json"

# Map Language enum values to GitHub search language qualifiers.
_GITHUB_LANGUAGE: dict[str, str] = {
    "Java": "Java",
    "Python": "Python",
    "Go": "Go",
    "Rust": "Rust",
    "TypeScript": "TypeScript",
    "JavaScript": "JavaScript",
    "C#": "C#",
    "C++": "C++",
    "C": "C",
    "Kotlin": "Kotlin",
    "Scala": "Scala",
    "Ruby": "Ruby",
    "PHP": "PHP",
    "COBOL": "COBOL",
    "PL/I": "PLI",
}

_DIRECTION_TO_LABEL: dict[SignalDirection, TrainingLabel] = {
    SignalDirection.INWARD: TrainingLabel.DEFINITE_INWARD,
    SignalDirection.OUTWARD: TrainingLabel.DEFINITE_OUTWARD,
}

# Seconds to wait between GitHub search API requests (rate limit: 30/min).
_SEARCH_SLEEP = 2.5
# Seconds to wait between file content requests (rate limit: 5000/hr).
_CONTENT_SLEEP = 0.3

# Search terms that reliably return non-integration code by language.
# Files found via these queries are scanned for coincidental integration-pattern
# matches — those lines become NOT_DEFINITE training examples.
_NOT_DEFINITE_SEARCH_TERMS: dict[str, list[str]] = {
    "Java": [
        "implements Serializable",
        "Objects.requireNonNull",
        "Collections.unmodifiableList",
        "@FunctionalInterface",
        "LocalDateTime.now()",
        "BigDecimal.ZERO",
        "Optional.empty()",
        "Comparator.comparing",
        "UUID.randomUUID()",
        "Arrays.asList(",
        "StringBuilder append",
        "throws IllegalArgumentException",
        "extends AbstractMap",
        "Stream.of(",
        "Math.max(",
    ],
    "Python": [
        "dataclasses.dataclass",
        "isinstance(",
        "collections.defaultdict",
        "functools.reduce",
        "itertools.chain",
        "typing.Optional",
        "pathlib.Path(",
        "datetime.timedelta",
    ],
    "Go": [
        "sort.Slice(",
        "strings.Builder",
        "sync.WaitGroup",
        "math.MaxInt",
        "bytes.Buffer",
    ],
}


@dataclass(frozen=True)
class HarvestSpec:
    """A single (pattern, direction) pair to search for on GitHub."""

    language: Language
    integration_type: IntegrationType
    direction: SignalDirection
    label: TrainingLabel
    pattern: str
    search_term: str
    confidence: Confidence
    source: str


def _extract_search_term(pattern: str) -> str | None:
    """Extract a plain-text search term from a regex pattern.

    Returns None if the pattern contains too many regex constructs to produce
    a meaningful literal search string.
    """
    term = pattern
    # Unescape common sequences that are fine as literals.
    term = term.replace(r"\.", ".").replace(r"\(", "(").replace(r"\)", ")")
    term = term.replace(r"\[", "[").replace(r"\]", "]").replace(r"\{", "{")
    term = re.sub(r"\\b", "", term)
    # Remove remaining backslash-prefixed shorthand classes (\w, \d, \s, …).
    term = re.sub(r"\\[wWdDsS]", "", term)
    # Remove quantifiers and anchors.
    term = re.sub(r"[*+?^$]", "", term)
    # Remove inline flags like (?i).
    term = re.sub(r"\(\?[a-zA-Z]+\)", "", term)
    # Remove capturing group parens that remain.
    term = term.replace("(", "").replace(")", "")
    term = term.strip()

    # Skip if what's left is too short or looks like a regex alternation/class.
    if len(term) < 4 or "|" in term or "[" in term:
        return None
    return term


def _build_harvest_specs(
    languages: list[Language],
    integration_types: list[IntegrationType],
) -> list[HarvestSpec]:
    """Build the list of (pattern, direction) pairs to harvest from GitHub."""
    target_langs = languages if languages else list(Language)
    target_types = set(integration_types) if integration_types else None

    specs: list[HarvestSpec] = []
    seen_terms: set[tuple[str, str, str]] = set()

    for lang in target_langs:
        lang_module = LANGUAGE_MODULES.get(lang)
        if lang_module is None:
            continue
        if lang.value not in _GITHUB_LANGUAGE:
            continue

        all_frameworks = list(lang_module.FRAMEWORK_PATTERNS.keys())
        patterns_by_type = get_patterns_for_language(lang, all_frameworks)

        for itype, pattern_tuples in patterns_by_type.items():
            if target_types and itype not in target_types:
                continue
            for regex, confidence, source, direction in pattern_tuples:
                if direction == SignalDirection.AMBIGUOUS:
                    continue
                if confidence not in (Confidence.HIGH,):
                    continue
                label = _DIRECTION_TO_LABEL[direction]
                search_term = _extract_search_term(regex)
                if search_term is None:
                    continue
                # Deduplicate: same (search_term, language, direction).
                dedup_key = (search_term, lang.value, direction.value)
                if dedup_key in seen_terms:
                    continue
                seen_terms.add(dedup_key)
                specs.append(
                    HarvestSpec(
                        language=lang,
                        integration_type=itype,
                        direction=direction,
                        label=label,
                        pattern=regex,
                        search_term=search_term,
                        confidence=confidence,
                        source=source,
                    )
                )

    return specs


@dataclass(frozen=True)
class NotDefiniteSpec:
    """A non-integration search term for harvesting NOT_DEFINITE examples."""

    language: Language
    search_term: str


def _spec_key(spec: HarvestSpec) -> str:
    """Stable string key for a HarvestSpec (used in checkpoint)."""
    return f"{spec.language.value}/{spec.integration_type.value}/{spec.pattern}"


def _not_definite_spec_key(spec: NotDefiniteSpec) -> str:
    """Stable string key for a NotDefiniteSpec (used in checkpoint)."""
    return f"nd/{spec.language.value}/{spec.search_term}"


def _build_not_definite_specs(languages: list[Language]) -> list[NotDefiniteSpec]:
    """Build NOT_DEFINITE harvest specs for the requested languages."""
    target_langs = (
        languages
        if languages
        else [
            lang
            for lang in Language
            if lang.value in _NOT_DEFINITE_SEARCH_TERMS
        ]
    )
    return [
        NotDefiniteSpec(language=lang, search_term=term)
        for lang in target_langs
        if lang.value in _NOT_DEFINITE_SEARCH_TERMS
        for term in _NOT_DEFINITE_SEARCH_TERMS[lang.value]
    ]


class GitHubClient:
    """Minimal GitHub REST v3 client for code search and content fetching."""

    _BASE = "https://api.github.com"

    def __init__(self, token: str) -> None:
        self._headers = {
            "Authorization": f"token {token}",
            "Accept": "application/vnd.github.v3+json",
            "X-GitHub-Api-Version": "2022-11-28",
        }

    def _get(self, url: str, extra_headers: dict[str, str] = {}) -> bytes | None:
        headers = {**self._headers, **extra_headers}
        req = Request(url, headers=headers)
        try:
            with urlopen(req, timeout=20) as resp:
                return resp.read()
        except HTTPError as exc:
            if exc.code == 422:
                logger.debug("GitHub 422 (unprocessable) for %s", url)
            elif exc.code == 403:
                logger.warning("GitHub 403 (rate limit or forbidden) for %s", url)
            else:
                logger.warning("GitHub HTTP %d for %s", exc.code, url)
            return None
        except URLError as exc:
            logger.warning("URL error for %s: %s", url, exc)
            return None

    def search_code(
        self, term: str, language: str, per_page: int = 10
    ) -> list[dict]:
        """Search GitHub code for a literal term in files of a given language."""
        query = quote(f"{term} language:{language}", safe=":")
        url = f"{self._BASE}/search/code?q={query}&per_page={per_page}"
        time.sleep(_SEARCH_SLEEP)
        data = self._get(url)
        if data is None:
            return []
        try:
            payload = json.loads(data)
        except json.JSONDecodeError:
            return []
        return payload.get("items", [])

    def fetch_file_content(self, url: str) -> str | None:
        """Fetch the raw text content of a file by its contents-API URL."""
        time.sleep(_CONTENT_SLEEP)
        data = self._get(url)
        if data is None:
            return None
        try:
            payload = json.loads(data)
        except json.JSONDecodeError:
            return None
        encoded = payload.get("content", "")
        if not encoded:
            return None
        try:
            return base64.b64decode(encoded).decode("utf-8", errors="replace")
        except Exception:
            return None


def _extract_snippet(
    lines: list[str], match_line_idx: int, context: int
) -> tuple[str, int]:
    """Extract a code snippet centred on the matching line.

    Returns (snippet_text, signal_line_index_within_snippet).
    """
    start = max(0, match_line_idx - context)
    end = min(len(lines), match_line_idx + context + 1)
    snippet_lines = lines[start:end]
    return "\n".join(snippet_lines), match_line_idx - start


def _make_example_id(spec: HarvestSpec, file_url: str, line_idx: int) -> str:
    raw = f"{spec.language.value}__{spec.integration_type.value}__{line_idx}__{file_url}"
    digest = hashlib.sha1(raw.encode()).hexdigest()[:8]
    return f"github__{spec.language.value}__{spec.integration_type.value}__{spec.label.value}__{spec.source}__{digest}"


def _examples_from_file(
    content: str,
    spec: HarvestSpec,
    file_url: str,
    context: int,
    seen: set[tuple[str, int]],
) -> list[TrainingExample]:
    """Extract TrainingExample objects from a file's content for a given spec."""
    lines = content.splitlines()
    examples: list[TrainingExample] = []
    for idx, line in enumerate(lines):
        if not re.search(spec.pattern, line):
            continue
        dedup_key = (file_url, idx)
        if dedup_key in seen:
            continue
        seen.add(dedup_key)
        snippet, signal_idx = _extract_snippet(lines, idx, context)
        examples.append(
            TrainingExample(
                id=_make_example_id(spec, file_url, idx),
                language=spec.language.value,
                integration_type=spec.integration_type.value,
                label=spec.label.value,
                code_snippet=snippet,
                signal_line_index=signal_idx,
                signal_line_content=line.strip(),
                matched_pattern=spec.pattern,
                ast_node_type="unknown",
                framework=spec.source,
            )
        )
    return examples


def _examples_from_file_not_definite(
    content: str,
    spec: NotDefiniteSpec,
    file_url: str,
    context: int,
    seen: set[tuple[str, int]],
    integration_patterns: dict[IntegrationType, list[tuple[str, Confidence, str, SignalDirection]]],
) -> list[TrainingExample]:
    """Extract NOT_DEFINITE examples from a non-integration file.

    Finds lines that coincidentally match an integration pattern and labels
    them NOT_DEFINITE — these are the false-positive cases the model must learn
    to reject.  Only the first matching pattern per line is used.
    """
    lines = content.splitlines()
    examples: list[TrainingExample] = []
    for idx, line in enumerate(lines):
        for itype, pattern_tuples in integration_patterns.items():
            for regex, _conf, source, _direction in pattern_tuples:
                if not re.search(regex, line):
                    continue
                dedup_key = (file_url, idx)
                if dedup_key in seen:
                    break
                seen.add(dedup_key)
                snippet, signal_idx = _extract_snippet(lines, idx, context)
                raw_id = f"{spec.language.value}__{itype.value}__{idx}__{file_url}"
                digest = hashlib.sha1(raw_id.encode()).hexdigest()[:8]
                examples.append(
                    TrainingExample(
                        id=f"github_nd__{spec.language.value}__{itype.value}__NOT_DEFINITE__{source}__{digest}",
                        language=spec.language.value,
                        integration_type=itype.value,
                        label=TrainingLabel.NOT_DEFINITE.value,
                        code_snippet=snippet,
                        signal_line_index=signal_idx,
                        signal_line_content=line.strip(),
                        matched_pattern=regex,
                        ast_node_type="unknown",
                        framework=source,
                    )
                )
                break  # one example per line
    return examples


def _harvest_not_definite_spec(
    spec: NotDefiniteSpec,
    client: GitHubClient,
    max_files: int,
    context: int,
    seen_dedup: set[tuple[str, int]],
    integration_patterns: dict[IntegrationType, list[tuple[str, Confidence, str, SignalDirection]]],
) -> list[TrainingExample]:
    """Harvest NOT_DEFINITE examples for a single NotDefiniteSpec."""
    github_lang = _GITHUB_LANGUAGE[spec.language.value]
    items = client.search_code(spec.search_term, github_lang, per_page=max_files)
    return [
        ex
        for item in items
        if (url := item.get("url", "")) and (content := client.fetch_file_content(url))
        for ex in _examples_from_file_not_definite(
            content, spec, url, context, seen_dedup, integration_patterns
        )
    ]


def _load_checkpoint(path: Path) -> set[str]:
    """Load the set of already-completed spec keys."""
    if not path.exists():
        return set()
    try:
        with path.open(encoding="utf-8") as f:
            data = json.load(f)
        return set(data.get("completed", []))
    except (json.JSONDecodeError, OSError):
        return set()


def _save_checkpoint(path: Path, completed: set[str]) -> None:
    with path.open("w", encoding="utf-8") as f:
        json.dump({"completed": sorted(completed)}, f, indent=2)


def _harvest_spec(
    spec: HarvestSpec,
    client: GitHubClient,
    max_files: int,
    context: int,
    seen_dedup: set[tuple[str, int]],
) -> list[TrainingExample]:
    """Harvest examples for a single HarvestSpec."""
    github_lang = _GITHUB_LANGUAGE[spec.language.value]
    items = client.search_code(spec.search_term, github_lang, per_page=max_files)
    examples: list[TrainingExample] = []
    for item in items:
        url = item.get("url", "")
        if not url:
            continue
        content = client.fetch_file_content(url)
        if not content:
            continue
        examples.extend(
            _examples_from_file(content, spec, url, context, seen_dedup)
        )
    return examples


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--token",
        default="",
        help="GitHub personal access token (or set GITHUB_TOKEN env var).",
    )
    parser.add_argument(
        "--languages",
        nargs="+",
        default=[],
        metavar="LANG",
        help="Languages to harvest (e.g. Java Python). Default: all.",
    )
    parser.add_argument(
        "--integration-types",
        nargs="+",
        default=[],
        metavar="TYPE",
        help="Integration types to harvest. Default: all.",
    )
    parser.add_argument(
        "--max-files",
        type=int,
        default=_DEFAULT_MAX_FILES,
        metavar="N",
        help=f"Max files to fetch per pattern (default: {_DEFAULT_MAX_FILES}).",
    )
    parser.add_argument(
        "--context-lines",
        type=int,
        default=_DEFAULT_CONTEXT_LINES,
        metavar="N",
        help=f"Lines of context around each match (default: {_DEFAULT_CONTEXT_LINES}).",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=_DEFAULT_OUTPUT_DIR,
        metavar="DIR",
        help=f"Directory to write output (default: {_DEFAULT_OUTPUT_DIR}).",
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=42,
        help="Random seed for train/val/test split (default: 42).",
    )
    parser.add_argument(
        "--resume",
        action="store_true",
        help="Skip specs already in the checkpoint file.",
    )
    parser.add_argument(
        "--not-definite",
        action="store_true",
        help="Harvest NOT_DEFINITE examples from non-integration code.",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Print the harvest plan without making any API calls.",
    )
    parser.add_argument(
        "--verbose", "-v", action="store_true", help="Enable verbose logging."
    )
    return parser.parse_args()


def _resolve_token(args: argparse.Namespace) -> str:
    if args.token:
        return args.token
    import os
    token = os.environ.get("GITHUB_TOKEN", "")
    if not token:
        print("Error: provide --token or set GITHUB_TOKEN.", file=sys.stderr)
        sys.exit(1)
    return token


def _resolve_languages(names: list[str]) -> list[Language]:
    result: list[Language] = []
    for name in names:
        lang = Language.from_name(name)
        if lang is None:
            valid = [l.value for l in Language]
            print(f"Unknown language: {name!r}. Valid: {valid}", file=sys.stderr)
            sys.exit(1)
        result.append(lang)
    return result


def _resolve_integration_types(names: list[str]) -> list[IntegrationType]:
    result: list[IntegrationType] = []
    for name in names:
        try:
            result.append(IntegrationType(name))
        except ValueError:
            valid = [t.value for t in IntegrationType]
            print(
                f"Unknown integration type: {name!r}. Valid: {valid}", file=sys.stderr
            )
            sys.exit(1)
    return result


def main() -> None:
    args = _parse_args()

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(asctime)s %(levelname)s %(name)s: %(message)s",
    )

    languages = _resolve_languages(args.languages)
    integration_types = _resolve_integration_types(args.integration_types)

    specs = _build_harvest_specs(languages, integration_types)
    nd_specs = _build_not_definite_specs(languages) if args.not_definite else []

    inward = sum(1 for s in specs if s.direction == SignalDirection.INWARD)
    outward = sum(1 for s in specs if s.direction == SignalDirection.OUTWARD)
    print(f"\nHarvest plan: {len(specs)} INWARD/OUTWARD patterns + {len(nd_specs)} NOT_DEFINITE search terms")
    print(f"  INWARD:       {inward}")
    print(f"  OUTWARD:      {outward}")
    print(f"  NOT_DEFINITE: {len(nd_specs)}")
    total_searches = len(specs) + len(nd_specs)
    print(
        f"  Estimated API calls: {total_searches} search + up to {total_searches * args.max_files} content fetches"
    )
    by_lang: dict[str, int] = {}
    for s in specs:
        by_lang[s.language.value] = by_lang.get(s.language.value, 0) + 1
    for s in nd_specs:
        by_lang[s.language.value] = by_lang.get(s.language.value, 0) + 1
    for lang, count in sorted(by_lang.items()):
        print(f"    {lang}: {count} patterns")

    if args.dry_run:
        print("\nDry run — skipping API calls.")
        return

    token = _resolve_token(args)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    checkpoint_path = args.output_dir / _CHECKPOINT_FILE
    completed = _load_checkpoint(checkpoint_path) if args.resume else set()

    pending = [s for s in specs if _spec_key(s) not in completed]
    if args.resume:
        print(f"\nResuming: {len(completed)} done, {len(pending)} remaining.")

    client = GitHubClient(token)
    seen_dedup: set[tuple[str, int]] = set()
    run_examples: list[TrainingExample] = []

    raw_path = args.output_dir / "raw.jsonl"
    # Append when resuming (preserve prior results); overwrite on a fresh run.
    raw_fp = raw_path.open("a" if args.resume else "w", encoding="utf-8")

    pending_nd = [s for s in nd_specs if _not_definite_spec_key(s) not in completed]

    # Pre-compute integration patterns once for NOT_DEFINITE scanning.
    nd_integration_patterns = (
        {
            lang: get_patterns_for_language(lang, list(LANGUAGE_MODULES[lang].FRAMEWORK_PATTERNS.keys()))
            for lang in {s.language for s in pending_nd}
            if lang in LANGUAGE_MODULES
        }
        if pending_nd
        else {}
    )

    try:
        for i, spec in enumerate(pending, 1):
            print(
                f"[{i}/{len(pending)}] {spec.language.value}/{spec.integration_type.value} "
                f"{spec.direction.value!r:8s} — {spec.search_term!r} (source: {spec.source})"
            )
            examples = _harvest_spec(spec, client, args.max_files, args.context_lines, seen_dedup)
            for ex in examples:
                raw_fp.write(json.dumps(ex.to_dict(), ensure_ascii=False) + "\n")
            raw_fp.flush()
            run_examples.extend(examples)
            completed.add(_spec_key(spec))
            _save_checkpoint(checkpoint_path, completed)
            print(f"       → {len(examples)} examples")

        for i, nd_spec in enumerate(pending_nd, 1):
            print(
                f"[nd {i}/{len(pending_nd)}] {nd_spec.language.value} NOT_DEFINITE"
                f" — {nd_spec.search_term!r}"
            )
            lang_patterns = nd_integration_patterns.get(nd_spec.language, {})
            examples = _harvest_not_definite_spec(
                nd_spec, client, args.max_files, args.context_lines, seen_dedup, lang_patterns
            )
            for ex in examples:
                raw_fp.write(json.dumps(ex.to_dict(), ensure_ascii=False) + "\n")
            raw_fp.flush()
            run_examples.extend(examples)
            completed.add(_not_definite_spec_key(nd_spec))
            _save_checkpoint(checkpoint_path, completed)
            print(f"       → {len(examples)} examples")
    finally:
        raw_fp.close()

    # Read everything from raw.jsonl (includes prior runs when resuming).
    raw_lines = [
        line for line in raw_path.read_text(encoding="utf-8").splitlines() if line.strip()
    ]
    print(f"\nTotal raw examples (this + prior runs): {len(raw_lines)}")

    validation = validate_batch(
        [TrainingExample(**json.loads(line)) for line in raw_lines]
    )
    print(f"Validation: {validation.valid}/{validation.total} passed")
    if validation.invalid > 0:
        print(f"  {validation.invalid} examples failed validation (discarded)")

    export_result = export_training_data(
        examples=list(validation.valid_examples),
        output_dir=args.output_dir,
        seed=args.seed,
    )

    print(f"\nExported to {export_result.output_dir}:")
    print(f"  Total: {export_result.total_examples}")
    print(f"  Train: {export_result.train_count}")
    print(f"  Val:   {export_result.val_count}")
    print(f"  Test:  {export_result.test_count}")


if __name__ == "__main__":
    main()
