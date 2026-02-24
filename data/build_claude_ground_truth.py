#!/usr/bin/env python3
"""Build Claude's independent ground truth classification for smojol signals.

Loads all 758 signals from the Gemini survey output (as the canonical signal set),
applies rule-based classification derived from manual review, and writes JSONL
output files with the same structure as other classifiers.

Classification rules are applied in priority order. Each signal gets a
`claude_reason` field explaining which rule fired.
"""

import json
import logging
import re
from pathlib import Path

logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")
logger = logging.getLogger(__name__)

DATA_DIR = Path(__file__).parent
INPUT_DIR = DATA_DIR / "survey_output_gemini_smojol"
OUTPUT_DIR = DATA_DIR / "survey_output_claude_gt"

ALL_INPUT_FILES = [
    "inward.jsonl",
    "outward.jsonl",
    "ambiguous.jsonl",
    "not_integration.jsonl",
    "noise.jsonl",
]


class ClassificationResult:
    INWARD = "inward"
    OUTWARD = "outward"
    AMBIGUOUS = "ambiguous"
    NOT_INTEGRATION = "not_integration"


class CobolStringPatterns:
    """COBOL keywords that appear as string literals in Java test code."""

    KEYWORDS = [
        r"FILE-CONTROL",
        r"FILE SECTION",
        r"SELECT\s+\S+.*ASSIGN",
        r"CLOSE\s+FILE",
        r"GET\s+FILE",
        r"READ\s+.*FILE",
        r"OPEN\s+.*FILE",
        r"OPEN\s+INPUT",
        r"OPEN\s+OUTPUT",
        r"\bFD\s+",
        r"DATA\s+RECORD",
        r"WRITE\s+\S+.*FROM",
        r"READ\s+\S+.*INTO",
        r"INPUT-OUTPUT\s+SECTION",
    ]
    COMPILED = [re.compile(pat, re.IGNORECASE) for pat in KEYWORDS]


class FileMetadataPatterns:
    """File metadata operations that aren't meaningful I/O boundaries."""

    PATTERNS = [
        r"file\.isHidden\(\)",
        r"file\.isDirectory\(\)",
        r"file\.getName\(\)",
        r"file\.getCanonicalFile\(\)",
        r"file\.getAbsoluteFile\(\)",
        r"\.toAbsolutePath\(\)",
        r"File::getName",
        r"file\.toFile\(\)\.exists\(\)",
        r"\.toFile\(\)",
        r"Files\.isDirectory\(",
        r"\.getPath\(\)",
        r"\.getParent\(\)",
        r"Paths\.get\(",
        r"URI\.create\(",
        r"\.toUri\(\)",
        r"\.getScheme\(\)",
        r"new\s+File\(",
        r"Path\.of\(",
    ]
    COMPILED = [re.compile(pat) for pat in PATTERNS]


class ActualFileIoPatterns:
    """Patterns indicating actual file read/write operations."""

    PATTERNS = [
        r"Files\.readAllBytes",
        r"Files\.readAllLines",
        r"Files\.readString",
        r"Files\.write\b",
        r"Files\.writeString",
        r"Files\.copy\(",
        r"Files\.move\(",
        r"Files\.delete\(",
        r"Files\.createDirector",
        r"Files\.createTemp",
        r"Files\.newInputStream",
        r"Files\.newOutputStream",
        r"Files\.newBufferedReader",
        r"Files\.newBufferedWriter",
        r"FileInputStream",
        r"FileOutputStream",
        r"FileWriter",
        r"FileReader",
        r"BufferedReader\(new\s+InputStreamReader",
        r"BufferedWriter",
        r"ZipInputStream",
        r"ZipOutputStream",
        r"PrintWriter\(",
        r"\.write\(",
        r"\.readLine\(",
        r"\.read\(",
        r"ObjectMapper.*readValue",
        r"objectMapper.*readValue",
        r"gson\.fromJson",
        r"gson\.toJson",
    ]
    COMPILED = [re.compile(pat) for pat in PATTERNS]


class HttpEndpointPatterns:
    """HTTP/REST endpoint definitions and client calls."""

    PATTERNS = [
        r"\.get\(\s*\"",
        r"\.post\(\s*\"",
        r"\.put\(\s*\"",
        r"\.delete\(\s*\"",
        r"ctx\.json\(",
        r"ctx\.result\(",
        r"ctx\.status\(",
        r"HttpClient",
        r"HttpRequest",
        r"HttpResponse",
    ]
    COMPILED = [re.compile(pat) for pat in PATTERNS]


class DatabasePatterns:
    """Database operation patterns."""

    PATTERNS = [
        r"DriverManager\.getConnection",
        r"DSL\.using",
        r"\.insertInto\(",
        r"\.execute\(\)",
        r"\.select\(",
        r"\.fetch\(",
        r"\.createTable\(",
        r"\.dropTable",
        r"PreparedStatement",
        r"ResultSet\b",
        r"Statement\.execute",
    ]
    COMPILED = [re.compile(pat) for pat in PATTERNS]


class SocketPatterns:
    """Socket operation patterns."""

    PATTERNS = [
        r"ServerSocket",
        r"new\s+Socket\(",
        r"\.accept\(\)",
        r"socket\.getInputStream",
        r"socket\.getOutputStream",
    ]
    COMPILED = [re.compile(pat) for pat in PATTERNS]


class CachePatterns:
    """Cache operation patterns."""

    PATTERNS = [
        r"CacheBuilder\.newBuilder",
        r"cache\.asMap\(\)",
        r"cache\.get\(",
        r"cache\.put\(",
        r"cache\.invalidate",
        r"LoadingCache",
    ]
    COMPILED = [re.compile(pat) for pat in PATTERNS]


def _any_pattern_matches(text: str, compiled_patterns: list[re.Pattern]) -> bool:
    return any(pat.search(text) for pat in compiled_patterns)


def _is_generated_parser_code(record: dict) -> bool:
    """Rule 1: Generated parser code (Db2SqlParser.java, ANTLR-generated)."""
    file_path = record.get("file_path", "")
    file_name = Path(file_path).name
    if file_name == "Db2SqlParser.java":
        return True
    line = record.get("line_content", "")
    ast = record.get("ast_node_text", "")
    combined = line + " " + ast
    parser_indicators = [
        r"\bmatch\(DATABASE\)",
        r"\bmatch\(CACHE\)",
        r"\bmatch\(FILE\)",
        r"\bcase\s+DATABASE:",
        r"\bcase\s+CACHE:",
        r"\bcase\s+FILE:",
    ]
    if "Db2Sql" in file_path or "Parser.java" in file_name:
        return any(re.search(pat, combined) for pat in parser_indicators)
    return False


def _is_cobol_string_literal(record: dict) -> bool:
    """Rule 2: COBOL string literals in Java test/production code."""
    line = record.get("line_content", "")
    ast = record.get("ast_node_text", "")
    combined = line + " " + ast

    is_string_context = (
        line.strip().startswith('+ "')
        or line.strip().startswith('"')
        or "static final String" in ast
        or "private static final String TEXT" in ast
        or "public static final String" in ast
    )

    if not is_string_context:
        return False

    return _any_pattern_matches(combined, CobolStringPatterns.COMPILED)


def _is_string_name_only_reference(record: dict) -> bool:
    """Rule 3: String/name-only references (logging, assertions, constants)."""
    line = record.get("line_content", "")
    ast = record.get("ast_node_text", "")
    combined = line + " " + ast

    logging_patterns = [
        r"LOG\.\w+\(",
        r"LOGGER\.\w+\(",
        r"logger\.\w+\(",
        r"log\.\w+\(",
    ]
    if any(re.search(pat, line) for pat in logging_patterns):
        return True

    assertion_patterns = [
        r"assertEquals\(",
        r"assertThat\(",
        r"assertNotNull\(",
        r"assertTrue\(",
        r"assertFalse\(",
        r"verify\(",
        r"Assertions\.\w+\(",
    ]
    if any(re.search(pat, combined) for pat in assertion_patterns):
        return True

    if re.search(r'"-\w*FILE\w*-?\w*"', combined):
        return True
    if re.search(r'".*file.*option.*"', combined, re.IGNORECASE):
        return True

    if re.search(r'validateLength\(.*".*name"', combined):
        return True

    if re.search(r"NodeSymbolType\.\w+", combined):
        return True

    return False


def _is_di_config_binding(record: dict) -> bool:
    """Rule 4: DI/config binding (Guice, Spring, etc.)."""
    line = record.get("line_content", "")
    ast = record.get("ast_node_text", "")
    combined = line + " " + ast

    di_patterns = [
        r"bindConstant\(\)",
        r"Names\.named\(",
        r"@Named\(",
        r"@Inject",
        r"@Provides",
        r"@Singleton",
        r"bind\(\w+\.class\)",
    ]
    return any(re.search(pat, combined) for pat in di_patterns)


def _is_file_metadata_operation(record: dict) -> bool:
    """Rule 5: File metadata operations (not meaningful I/O boundaries)."""
    line = record.get("line_content", "")
    ast = record.get("ast_node_text", "")
    combined = line + " " + ast

    if _any_pattern_matches(combined, FileMetadataPatterns.COMPILED):
        if not _any_pattern_matches(combined, ActualFileIoPatterns.COMPILED):
            return True
    return False


def _is_boolean_download_field(record: dict) -> bool:
    """Rule 6: Boolean field/param named 'download'."""
    line = record.get("line_content", "")
    ast = record.get("ast_node_text", "")
    combined = line + " " + ast

    return bool(
        re.search(r"boolean\s+download", combined)
        or re.search(r"this\.download\s*=\s*download", combined)
    )


def _is_actual_file_io(record: dict) -> bool:
    """Rule 7: Actual file I/O operations."""
    line = record.get("line_content", "")
    ast = record.get("ast_node_text", "")
    combined = line + " " + ast
    return _any_pattern_matches(combined, ActualFileIoPatterns.COMPILED)


def _is_http_endpoint(record: dict) -> bool:
    """Rule 8: HTTP/REST endpoints."""
    line = record.get("line_content", "")
    ast = record.get("ast_node_text", "")
    combined = line + " " + ast

    file_path = record.get("file_path", "")
    if "ApiServer" in file_path or "Controller" in file_path or "Route" in file_path:
        return _any_pattern_matches(combined, HttpEndpointPatterns.COMPILED)

    javalin_patterns = [
        r"app\.get\(",
        r"app\.post\(",
        r"app\.put\(",
        r"app\.delete\(",
        r"Javalin\.create",
        r"ctx\.json\(",
        r"ctx\.result\(",
    ]
    return any(re.search(pat, combined) for pat in javalin_patterns)


def _is_database_operation(record: dict) -> bool:
    """Rule 9: Database operations."""
    line = record.get("line_content", "")
    ast = record.get("ast_node_text", "")
    combined = line + " " + ast
    return _any_pattern_matches(combined, DatabasePatterns.COMPILED)


def _is_socket_operation(record: dict) -> bool:
    """Rule 10: Socket operations."""
    line = record.get("line_content", "")
    ast = record.get("ast_node_text", "")
    combined = line + " " + ast
    return _any_pattern_matches(combined, SocketPatterns.COMPILED)


def _is_cache_operation(record: dict) -> bool:
    """Rule 11: Cache operations."""
    line = record.get("line_content", "")
    ast = record.get("ast_node_text", "")
    combined = line + " " + ast
    return _any_pattern_matches(combined, CachePatterns.COMPILED)


def _classify_direction(record: dict, classification: str) -> str:
    """Determine direction for integration signals."""
    if classification == ClassificationResult.NOT_INTEGRATION:
        return "not_integration"

    line = record.get("line_content", "")
    ast = record.get("ast_node_text", "")
    combined = line + " " + ast
    match_details = record.get("match_details", [])
    integration_type = (
        match_details[0].get("integration_type", "") if match_details else ""
    )

    inward_indicators = [
        r"ServerSocket",
        r"\.accept\(\)",
        r"BufferedReader\(new\s+InputStreamReader\(System\.in\)",
        r"FileInputStream",
        r"FileReader",
        r"Files\.readAllBytes",
        r"Files\.readAllLines",
        r"Files\.readString",
        r"\.readLine\(",
        r"ctx\.body\(",
        r"app\.post\(",
        r"InputStream",
        r"ZipInputStream",
    ]
    outward_indicators = [
        r"FileOutputStream",
        r"FileWriter",
        r"Files\.write\b",
        r"Files\.writeString",
        r"PrintWriter",
        r"\.write\(",
        r"ctx\.json\(",
        r"ctx\.result\(",
        r"app\.get\(",
        r"OutputStream",
        r"ZipOutputStream",
    ]

    inward_match = any(re.search(pat, combined) for pat in inward_indicators)
    outward_match = any(re.search(pat, combined) for pat in outward_indicators)

    if inward_match and outward_match:
        return ClassificationResult.AMBIGUOUS
    if inward_match:
        return ClassificationResult.INWARD
    if outward_match:
        return ClassificationResult.OUTWARD

    if integration_type == "socket":
        return ClassificationResult.INWARD
    if integration_type == "database":
        return ClassificationResult.OUTWARD
    if integration_type == "http":
        return ClassificationResult.AMBIGUOUS
    if integration_type == "cache":
        return ClassificationResult.AMBIGUOUS

    return ClassificationResult.AMBIGUOUS


def classify_signal(record: dict) -> tuple[str, str]:
    """Classify a signal, returning (classification, reason).

    Rules are applied in priority order. First matching rule wins.
    Returns (direction_or_not_integration, reason_string).
    """
    if _is_generated_parser_code(record):
        return (
            ClassificationResult.NOT_INTEGRATION,
            "Generated parser code (ANTLR/Db2SqlParser)",
        )

    if _is_cobol_string_literal(record):
        return (
            ClassificationResult.NOT_INTEGRATION,
            "COBOL string literal in Java code",
        )

    if _is_string_name_only_reference(record):
        return (
            ClassificationResult.NOT_INTEGRATION,
            "String/name-only reference (logging, assertion, constant)",
        )

    if _is_di_config_binding(record):
        return (
            ClassificationResult.NOT_INTEGRATION,
            "DI/config binding (not runtime I/O)",
        )

    if _is_boolean_download_field(record):
        return (
            ClassificationResult.NOT_INTEGRATION,
            "Boolean field/param named 'download'",
        )

    if _is_file_metadata_operation(record):
        return (
            ClassificationResult.NOT_INTEGRATION,
            "File metadata operation (not meaningful I/O)",
        )

    if _is_socket_operation(record):
        direction = _classify_direction(record, "integration")
        return (direction, "Socket operation")

    if _is_http_endpoint(record):
        direction = _classify_direction(record, "integration")
        return (direction, "HTTP/REST endpoint")

    if _is_database_operation(record):
        direction = _classify_direction(record, "integration")
        return (direction, "Database operation")

    if _is_actual_file_io(record):
        direction = _classify_direction(record, "integration")
        return (direction, "Actual file I/O operation")

    if _is_cache_operation(record):
        return (ClassificationResult.AMBIGUOUS, "Cache operation (ambiguous boundary)")

    gemini_direction = record.get("direction", "not_integration")
    gemini_validity = record.get("validity", "NOISE")

    if gemini_validity == "NOISE":
        return (
            ClassificationResult.NOT_INTEGRATION,
            "Gemini tiebreaker: classified as NOISE",
        )

    if gemini_direction == "not_integration":
        return (
            ClassificationResult.NOT_INTEGRATION,
            "Gemini tiebreaker: not_integration",
        )

    return (gemini_direction, f"Gemini tiebreaker: {gemini_direction}")


def load_all_signals(input_dir: Path) -> list[dict]:
    """Load all signals from all JSONL files, deduplicating by (file_path, line_number)."""
    seen: dict[tuple[str, int], dict] = {}

    for fname in ALL_INPUT_FILES:
        fpath = input_dir / fname
        if not fpath.exists():
            logger.warning("Missing input file: %s", fpath)
            continue

        for line in fpath.read_text().strip().split("\n"):
            if not line.strip():
                continue
            record = json.loads(line)
            key = (record["file_path"], record["line_number"])
            if key not in seen:
                record["_gemini_source_file"] = fname
                seen[key] = record

    logger.info("Loaded %d unique signals from %s", len(seen), input_dir)
    return list(seen.values())


def write_output(
    classified_signals: list[tuple[dict, str, str]], output_dir: Path
) -> None:
    """Write classified signals to JSONL files grouped by classification."""
    output_dir.mkdir(parents=True, exist_ok=True)

    buckets: dict[str, list[dict]] = {
        ClassificationResult.INWARD: [],
        ClassificationResult.OUTWARD: [],
        ClassificationResult.AMBIGUOUS: [],
        ClassificationResult.NOT_INTEGRATION: [],
    }

    for record, classification, reason in classified_signals:
        output_record = {k: v for k, v in record.items() if not k.startswith("_")}
        output_record["claude_classification"] = classification
        output_record["claude_reason"] = reason
        output_record["direction"] = classification
        output_record["validity"] = (
            "NOISE"
            if classification == ClassificationResult.NOT_INTEGRATION
            else "SIGNAL"
        )
        buckets[classification].append(output_record)

    for classification, records in buckets.items():
        output_path = output_dir / f"{classification}.jsonl"
        lines = [json.dumps(r, ensure_ascii=False) for r in records]
        output_path.write_text("\n".join(lines) + "\n" if lines else "")
        logger.info(
            "Wrote %d signals to %s",
            len(records),
            output_path.name,
        )


def main() -> None:
    signals = load_all_signals(INPUT_DIR)

    classified = [(record, *classify_signal(record)) for record in signals]

    write_output(classified, OUTPUT_DIR)

    counts = {}
    for _, classification, _ in classified:
        counts[classification] = counts.get(classification, 0) + 1

    logger.info("Classification summary:")
    for cls, count in sorted(counts.items()):
        logger.info("  %s: %d", cls, count)
    logger.info("  Total: %d", len(classified))

    reason_counts: dict[str, int] = {}
    for _, _, reason in classified:
        reason_counts[reason] = reason_counts.get(reason, 0) + 1

    logger.info("Rule hit counts:")
    for reason, count in sorted(reason_counts.items(), key=lambda x: -x[1]):
        logger.info("  %s: %d", reason, count)


if __name__ == "__main__":
    main()
