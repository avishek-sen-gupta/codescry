"""Orchestrator: read file -> prefilter -> chunk -> prompt -> parse."""

from pathlib import Path
from typing import Iterator

from repo_surveyor.detection.integration_detector import (
    _get_source_files,
    get_language_from_extension,
)
from repo_surveyor.integration_patterns import Language
from repo_surveyor.ml_classifier.model_protocol import LLMModel
from repo_surveyor.ml_classifier.prefilter import prefilter_lines
from repo_surveyor.ml_classifier.prompt import (
    SYSTEM_PROMPT,
    build_user_prompt,
    chunk_lines,
)
from repo_surveyor.ml_classifier.response_parser import parse_classification_response
from repo_surveyor.ml_classifier.types import (
    ClassifiedLine,
    FileClassification,
    RepositoryClassification,
)


def classify_file(
    file_path: Path,
    model: LLMModel,
) -> FileClassification | None:
    """Classify all lines in a single source file.

    Args:
        file_path: Path to the source file.
        model: An LLMModel implementation.

    Returns:
        FileClassification result, or None if the file cannot be read.
    """
    try:
        content = file_path.read_text(encoding="utf-8", errors="ignore")
    except (OSError, IOError):
        return None

    language = get_language_from_extension(str(file_path))
    numbered_lines = [(i, line) for i, line in enumerate(content.splitlines(), start=1)]

    kept_lines, skipped_count = prefilter_lines(numbered_lines)

    if not kept_lines:
        return FileClassification(
            file_path=str(file_path),
            language=language.value if language else None,
            classified_lines=(),
            lines_submitted=0,
            lines_skipped=skipped_count,
        )

    original_lines = {num: text for num, text in kept_lines}
    all_classified: list[ClassifiedLine] = []

    for chunk in chunk_lines(kept_lines):
        user_prompt = build_user_prompt(chunk)
        result = model.classify(SYSTEM_PROMPT, user_prompt)
        chunk_originals = {num: text for num, text in chunk}
        parsed = parse_classification_response(result.text, chunk_originals)
        all_classified.extend(parsed)

    return FileClassification(
        file_path=str(file_path),
        language=language.value if language else None,
        classified_lines=tuple(sorted(all_classified, key=lambda c: c.line_number)),
        lines_submitted=len(kept_lines),
        lines_skipped=skipped_count,
    )


def classify_repository(
    repo_path: str | Path,
    model: LLMModel,
    languages: list[Language] = [],
) -> RepositoryClassification:
    """Classify all source files in a repository.

    Args:
        repo_path: Path to the repository.
        model: An LLMModel implementation.
        languages: List of languages to filter by. Empty means all.

    Returns:
        RepositoryClassification with results for all scanned files.
    """
    repo_path = Path(repo_path)
    classifications: list[FileClassification] = []
    files_scanned = 0

    for file_path in _get_source_files(repo_path, languages):
        files_scanned += 1
        result = classify_file(file_path, model)
        if result is not None:
            classifications.append(result)

    return RepositoryClassification(
        file_classifications=tuple(classifications),
        files_scanned=files_scanned,
        model_id=model.model_id,
    )
