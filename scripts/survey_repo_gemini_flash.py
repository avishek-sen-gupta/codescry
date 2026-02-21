"""Run the full survey pipeline using Gemini Flash for concretisation.

Identical to survey_repo_ollama.py up to the concretisation step, but uses the
Gemini Flash API instead of a local Ollama model for classification.

Usage:
    poetry run python scripts/survey_repo_gemini_flash.py /path/to/repo
    poetry run python scripts/survey_repo_gemini_flash.py /path/to/repo --languages Java
    poetry run python scripts/survey_repo_gemini_flash.py /path/to/repo \\
        --model gemini-2.5-flash
"""

import argparse
import json
import logging
import os
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from repo_surveyor import survey
from repo_surveyor.integration_concretiser.gemini_concretiser import (
    _DEFAULT_MODEL,
    concretise_with_gemini,
)
from repo_surveyor.core.pipeline_timer import PipelineTimingObserver
from repo_surveyor.training.signal_classifier import NullSignalClassifier
from repo_surveyor.training.types import TrainingLabel

logger = logging.getLogger(__name__)

_ENV_VAR_API_KEY = "GEMINI_001_EMBEDDING_API_KEY"


def _signal_to_dict(s, label_map: dict) -> dict:
    """Serialise a ConcretisedSignal to a JSON-friendly dict."""
    gemini_meta = label_map.get(
        (s.original_signal.match.file_path, s.original_signal.match.line_number), {}
    )
    return {
        "label": s.label.value,
        "gemini_confidence": gemini_meta.get("confidence"),
        "gemini_reason": gemini_meta.get("reason"),
        "integration_type": s.original_signal.integration_type.value,
        "confidence": s.original_signal.confidence.value,
        "line_content": s.original_signal.match.line_content.strip(),
        "file_path": s.original_signal.match.file_path,
        "line_number": s.original_signal.match.line_number,
        "matched_pattern": s.original_signal.matched_pattern,
        "source": s.original_signal.source,
        "ast_node_type": s.ast_context.node_type,
        "ast_start_line": s.ast_context.start_line,
        "ast_end_line": s.ast_context.end_line,
        "ast_node_text": s.ast_context.node_text,
    }


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("repo_path", help="Path to the repository to analyse")
    parser.add_argument(
        "--languages",
        nargs="+",
        default=[],
        metavar="LANG",
        help="Languages to filter (e.g. Java Python). Defaults to all.",
    )
    parser.add_argument(
        "--model",
        default=_DEFAULT_MODEL,
        metavar="MODEL",
        help=f"Gemini model name (default: {_DEFAULT_MODEL}).",
    )
    parser.add_argument(
        "--output-dir",
        default="data/survey_output_gemini",
        metavar="DIR",
        help="Directory to write per-label JSONL files (default: data/survey_output_gemini).",
    )
    return parser.parse_args()


def main() -> None:
    args = _parse_args()

    logging.basicConfig(
        level=logging.DEBUG,
        format="%(asctime)s %(levelname)-8s %(name)s: %(message)s",
        stream=sys.stdout,
    )

    api_key = os.environ.get(_ENV_VAR_API_KEY, "")
    if not api_key:
        logger.error(
            "Set the %s environment variable to your Google AI API key.",
            _ENV_VAR_API_KEY,
        )
        sys.exit(1)

    logger.info("=== Gemini Flash Survey Pipeline ===")
    logger.info("Repo:       %s", args.repo_path)
    logger.info("Languages:  %s", args.languages or "all")
    logger.info("Model:      %s", args.model)
    logger.info("Output dir: %s", args.output_dir)

    # --- Phase 1: run the pre-concretisation pipeline ---
    logger.info("--- Phase 1: running pre-concretisation pipeline ---")
    timer = PipelineTimingObserver()
    _, _, integration, _, _ = survey(
        args.repo_path,
        languages=args.languages,
        classifier=NullSignalClassifier(),
        timer=timer,
    )

    logger.info(
        "Integration detection complete: %d signals in %d files",
        len(integration.integration_points),
        integration.files_scanned,
    )

    # --- Phase 2: Gemini-based concretisation ---
    logger.info("--- Phase 2: Gemini Flash concretisation ---")
    concretisation, gemini_metadata = concretise_with_gemini(
        integration,
        api_key=api_key,
        model=args.model,
    )

    print(f"\nFiles scanned:  {integration.files_scanned}")
    print(f"Signals found:  {len(integration.integration_points)}")
    print(f"\nSubmitted:      {concretisation.signals_submitted}")
    print(f"Definite:       {concretisation.signals_definite}")
    print(f"Discarded:      {concretisation.signals_discarded}")

    print("\n=== Timings ===")
    for r in timer.completed:
        print(f"  {r.stage:<50s} {r.duration_seconds:>8.2f} s")

    # --- Phase 3: write output files ---
    out = Path(args.output_dir)
    out.mkdir(parents=True, exist_ok=True)

    print()
    for label in TrainingLabel:
        signals = [s for s in concretisation.concretised if s.label == label]
        path = out / f"{label.value.lower()}.jsonl"
        path.write_text(
            "\n".join(json.dumps(_signal_to_dict(s, gemini_metadata)) for s in signals)
            + "\n",
            encoding="utf-8",
        )
        print(f"  {label.value}: {len(signals):4d} signals → {path}")


if __name__ == "__main__":
    main()
