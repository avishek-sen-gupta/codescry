"""One-off script to rewrite pattern descriptions using Claude API.

Reads all integration pattern files, extracts description strings,
sends them to Claude for rewriting in the passive-voice template style,
and replaces them in the source files.

Usage:
    # Dry-run (show proposed changes, no writes):
    poetry run python scripts/rewrite_pattern_descriptions.py --dry-run

    # Apply changes:
    poetry run python scripts/rewrite_pattern_descriptions.py

    # Use a specific model:
    poetry run python scripts/rewrite_pattern_descriptions.py --model claude-sonnet-4-20250514
"""

import argparse
import json
import logging
import re
import sys
import time
from dataclasses import dataclass
from pathlib import Path

import anthropic

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s %(levelname)s %(message)s",
)
logger = logging.getLogger(__name__)

PATTERNS_DIR = (
    Path(__file__).resolve().parent.parent
    / "src"
    / "repo_surveyor"
    / "integration_patterns"
)

EXCLUDED_FILENAMES = {"types.py", "loader.py", "__init__.py", "constants.py"}

BATCH_SIZE = 60

SYSTEM_PROMPT = """\
You are rewriting integration pattern descriptions to maximise cosine similarity \
in a code embedding model. Follow these rules strictly:

1. **Subject-first passive voice**: "X is Y-ed by Z"
   The subject noun is the *result* or *primary object* of the code operation.
   - Good: "Database is accessed by opening connection with credentials"
   - Bad: "Opens a connection to access the database with credentials"

2. **Domain concepts over API names**: Use the concept-level noun the code is *about*, \
not the specific class/framework name. "database" not "DriverManager", "cache" not \
"CacheBuilder", "HTTP endpoint" not "HttpServletRequest".

3. **Key parameters as bare nouns**: Echo semantic roles without naming variables.
   - Good: "...with credentials"
   - Bad: "...with url, user, password variables"

4. **Brevity**: Every word must earn its place. No articles ("a", "the"), qualifiers \
("specific", "designated"), or elaboration ("in order to", "which allows").

5. **Match action verb semantics**:
   | Code pattern      | Best verb        |
   |-------------------|------------------|
   | new X()           | "is created"     |
   | .getConnection()  | "is opened"      |
   | write*()          | "is written"     |
   | Builder pattern   | "is built"       |
   | import / require  | "is imported"    |
   | @annotation       | "is annotated"   |
   | route handler     | "is registered"  |

6. **Template**: `[Result noun] is [verb past participle] [preposition] [key parameters]`

7. **Keep under 12 words**.

8. **Preserve semantic meaning**: The rewritten description must describe the same \
integration type (HTTP, database, messaging, etc.) and the same direction \
(inward/outward/ambiguous).

You will receive a JSON array of old description strings. Return a JSON array of \
the same length with rewritten descriptions, in the same order. Return ONLY the \
JSON array, no other text."""

EXAMPLE_REWRITES = """
Example before/after pairs:
- "Flask @app.route decorator defining an inbound HTTP REST endpoint" → "HTTP route is registered with path handler"
- "This code uses Flask to expose an inbound REST API endpoint" → "REST endpoint is exposed for inbound requests"
- "Javalin .get() route registration for handling HTTP GET requests" → "GET route is registered for HTTP request handling"
- "This code uses Javalin to handle incoming HTTP GET requests" → "HTTP GET request is handled by route handler"
- "Generic HTTP keyword indicating web communication" → "HTTP communication is indicated"
- "This code references HTTP suggesting web-based integration" → "Web integration is referenced via HTTP"
- "Java HttpURLConnection for making outbound HTTP requests" → "HTTP connection is opened for outbound request"
- "PHP GuzzleHttp\\Client HTTP client being instantiated" → "HTTP client is created for outbound requests"
- "Spring @RestController annotation defining a REST API controller" → "REST controller is annotated for inbound API"
- "JDBC DriverManager.getConnection for connecting to a relational database" → "Database connection is opened with credentials"
"""


@dataclass(frozen=True)
class DescriptionLocation:
    """A description string and its location in a source file."""

    file_path: Path
    old_text: str
    line_hint: int


def _collect_pattern_files() -> list[Path]:
    """Glob all pattern .py files, excluding non-pattern files."""
    all_py = sorted(PATTERNS_DIR.rglob("*.py"))
    return [
        p
        for p in all_py
        if p.name not in EXCLUDED_FILENAMES and "package_parser" not in p.parts
    ]


# Regex to match the description tuple inside a pattern entry.
# Captures: two quoted strings inside a tuple that follows SignalDirection.
_DESC_TUPLE_RE = re.compile(
    r"""
    SignalDirection\.\w+\s*,\s*         # SignalDirection.INWARD,
    \(\s*                               # opening paren of description tuple
        "((?:[^"\\]|\\.)*)"             # first description string
        \s*,\s*
        "((?:[^"\\]|\\.)*)"             # second description string
        \s*,?\s*
    \)                                  # closing paren
    """,
    re.VERBOSE | re.DOTALL,
)


def _extract_descriptions(file_path: Path, source: str) -> list[DescriptionLocation]:
    """Extract all description strings from a pattern file."""
    locations = []
    for match in _DESC_TUPLE_RE.finditer(source):
        line_no = source[: match.start()].count("\n") + 1
        locations.append(
            DescriptionLocation(
                file_path=file_path,
                old_text=match.group(1),
                line_hint=line_no,
            )
        )
        locations.append(
            DescriptionLocation(
                file_path=file_path,
                old_text=match.group(2),
                line_hint=line_no,
            )
        )
    return locations


MAX_RETRIES = 3


def _rewrite_batch(
    client: anthropic.Anthropic,
    descriptions: list[str],
    model: str,
) -> list[str]:
    """Send a batch of descriptions to Claude for rewriting, with retries."""
    for attempt in range(1, MAX_RETRIES + 1):
        user_msg = json.dumps(descriptions, ensure_ascii=False)
        response = client.messages.create(
            model=model,
            max_tokens=4096,
            system=SYSTEM_PROMPT + "\n\n" + EXAMPLE_REWRITES,
            messages=[{"role": "user", "content": user_msg}],
        )
        raw = response.content[0].text.strip()
        # Strip markdown code fences if present
        if raw.startswith("```"):
            raw = re.sub(r"^```(?:json)?\s*\n?", "", raw)
            raw = re.sub(r"\n?```\s*$", "", raw)
        rewritten = json.loads(raw)
        if len(rewritten) == len(descriptions):
            return rewritten
        logger.warning(
            "Attempt %d: expected %d descriptions, got %d. Retrying...",
            attempt,
            len(descriptions),
            len(rewritten),
        )
    raise ValueError(
        f"After {MAX_RETRIES} retries, still got wrong count "
        f"(expected {len(descriptions)}, got {len(rewritten)})"
    )


def _apply_replacements(
    file_path: Path,
    source: str,
    old_to_new: dict[str, str],
) -> str:
    """Replace old description strings with new ones in source text."""
    result = source
    for old, new in old_to_new.items():
        # Escape backslashes for the replacement to be literal
        result = result.replace(f'"{old}"', f'"{new}"')
    return result


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show proposed changes without writing files.",
    )
    parser.add_argument(
        "--model",
        default="claude-sonnet-4-20250514",
        help="Claude model to use for rewriting (default: claude-sonnet-4-20250514).",
    )
    parser.add_argument(
        "--batch-size",
        type=int,
        default=BATCH_SIZE,
        help=f"Descriptions per API call (default: {BATCH_SIZE}).",
    )
    parser.add_argument(
        "--delay",
        type=float,
        default=1.0,
        help="Seconds to wait between API calls (default: 1.0).",
    )
    parser.add_argument(
        "--output-log",
        type=str,
        default="data/description_rewrites.jsonl",
        help="Path to write old→new mapping log (default: data/description_rewrites.jsonl).",
    )
    return parser.parse_args()


def main() -> None:
    args = _parse_args()
    client = anthropic.Anthropic()

    # Phase 1: Collect all descriptions from all pattern files
    pattern_files = _collect_pattern_files()
    logger.info("Found %d pattern files to process.", len(pattern_files))

    file_sources: dict[Path, str] = {}
    all_locations: list[DescriptionLocation] = []
    for pf in pattern_files:
        source = pf.read_text()
        file_sources[pf] = source
        locations = _extract_descriptions(pf, source)
        all_locations.extend(locations)
        logger.info("  %s: %d descriptions", pf.name, len(locations))

    total = len(all_locations)
    logger.info("Total descriptions to rewrite: %d", total)

    # Phase 2: Deduplicate — many descriptions repeat across files
    unique_descs = sorted(set(loc.old_text for loc in all_locations))
    logger.info("Unique descriptions: %d (of %d total)", len(unique_descs), total)

    # Phase 3: Load existing progress (resume support)
    log_path = Path(args.output_log)
    old_to_new: dict[str, str] = {}
    if log_path.exists():
        with log_path.open() as f:
            for line in f:
                entry = json.loads(line)
                old_to_new[entry["old"]] = entry["new"]
        logger.info("Loaded %d existing mappings from %s", len(old_to_new), log_path)

    # Phase 4: Rewrite remaining in batches
    remaining = [d for d in unique_descs if d not in old_to_new]
    logger.info(
        "Descriptions to rewrite: %d (%d already done)",
        len(remaining),
        len(old_to_new),
    )

    batch_size = args.batch_size
    num_batches = (len(remaining) + batch_size - 1) // batch_size

    for i in range(0, len(remaining), batch_size):
        batch = remaining[i : i + batch_size]
        batch_num = i // batch_size + 1
        logger.info(
            "Rewriting batch %d/%d (%d descriptions)...",
            batch_num,
            num_batches,
            len(batch),
        )
        rewritten = _rewrite_batch(client, batch, args.model)
        for old, new in zip(batch, rewritten):
            old_to_new[old] = new
        # Save progress incrementally
        log_path.parent.mkdir(parents=True, exist_ok=True)
        with log_path.open("w") as f:
            for old_key, new_val in sorted(old_to_new.items()):
                f.write(
                    json.dumps({"old": old_key, "new": new_val}, ensure_ascii=False)
                    + "\n"
                )
        if i + batch_size < len(remaining):
            time.sleep(args.delay)

    logger.info("Total mappings: %d (saved to %s)", len(old_to_new), log_path)

    # Phase 5: Apply replacements
    files_changed = 0
    for pf in pattern_files:
        source = file_sources[pf]
        file_locs = [loc for loc in all_locations if loc.file_path == pf]
        file_old_to_new = {
            loc.old_text: old_to_new[loc.old_text]
            for loc in file_locs
            if loc.old_text in old_to_new
        }

        if not file_old_to_new:
            continue

        new_source = _apply_replacements(pf, source, file_old_to_new)

        if new_source == source:
            logger.info("  %s: no changes (descriptions unchanged).", pf.name)
            continue

        # Verify description count didn't change
        old_count = len(_extract_descriptions(pf, source))
        new_count = len(_extract_descriptions(pf, new_source))
        if old_count != new_count:
            logger.error(
                "  %s: description count changed (%d → %d)! Skipping.",
                pf.name,
                old_count,
                new_count,
            )
            continue

        if args.dry_run:
            changes = sum(1 for old, new in file_old_to_new.items() if old != new)
            logger.info(
                "  [DRY-RUN] %s: %d descriptions would change.", pf.name, changes
            )
            for old, new in sorted(file_old_to_new.items()):
                if old != new:
                    logger.info("    - %r", old)
                    logger.info("    + %r", new)
        else:
            pf.write_text(new_source)
            files_changed += 1
            logger.info("  %s: updated.", pf.name)

    if args.dry_run:
        logger.info("Dry-run complete. No files were modified.")
    else:
        logger.info("Done. Updated %d files.", files_changed)


if __name__ == "__main__":
    main()
