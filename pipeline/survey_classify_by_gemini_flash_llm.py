"""Run the full survey pipeline using Gemini Flash for concretisation.

Identical to survey_classify_by_ollama_llm.py up to the concretisation step, but uses the
Gemini Flash API instead of a local Ollama model for classification.

Usage:
    poetry run python pipeline/survey_classify_by_gemini_flash_llm.py /path/to/repo
    poetry run python pipeline/survey_classify_by_gemini_flash_llm.py /path/to/repo --languages Java
    poetry run python pipeline/survey_classify_by_gemini_flash_llm.py /path/to/repo \\
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
from repo_surveyor.integration_concretiser.types import SignalValidity
from repo_surveyor.integration_patterns import SignalDirection
from repo_surveyor.training.signal_classifier import NullSignalClassifier

logger = logging.getLogger(__name__)

_STAGE_GEMINI_FLASH_CONCRETISATION = "gemini_flash_concretisation"
_ENV_VAR_API_KEY = "GEMINI_001_EMBEDDING_API_KEY"


def _signal_to_dict(s, label_map: dict) -> dict:
    """Serialise a ConcretisedSignal to a JSON-friendly dict."""
    gemini_meta = label_map.get(
        (s.original_signal.match.file_path, s.original_signal.match.line_number), {}
    )
    base = s.original_signal.to_dict()
    return {
        "validity": s.validity.value,
        "direction": s.direction.value,
        "gemini_confidence": gemini_meta.get("confidence"),
        "gemini_reason": gemini_meta.get("reason"),
        **base,
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
    timer.stage_started(_STAGE_GEMINI_FLASH_CONCRETISATION)
    concretisation, gemini_metadata = concretise_with_gemini(
        integration,
        api_key=api_key,
        model=args.model,
    )
    timer.stage_completed(_STAGE_GEMINI_FLASH_CONCRETISATION)

    print(f"\nFiles scanned:  {integration.files_scanned}")
    print(f"Signals found:  {len(integration.integration_points)}")
    print(f"\nSubmitted:      {concretisation.signals_submitted}")
    print(f"Classified:     {concretisation.signals_classified}")
    print(f"Unclassified:   {concretisation.signals_unclassified}")

    print("\n=== Timings ===")
    for r in timer.completed:
        print(f"  {r.stage:<50s} {r.duration_seconds:>8.2f} s")

    # --- Phase 3: write output files ---
    out = Path(args.output_dir)
    out.mkdir(parents=True, exist_ok=True)

    timings_path = out / "timings.json"
    timings_path.write_text(timer.to_json(), encoding="utf-8")
    logger.info("Timings written to %s", timings_path)

    _OUTPUT_GROUPS = [
        ("inward", SignalValidity.SIGNAL, SignalDirection.INWARD),
        ("outward", SignalValidity.SIGNAL, SignalDirection.OUTWARD),
        ("ambiguous", SignalValidity.SIGNAL, SignalDirection.AMBIGUOUS),
        ("not_integration", SignalValidity.NOISE, SignalDirection.NOT_INTEGRATION),
        ("noise", SignalValidity.NOISE, None),
    ]

    print()
    for filename, validity, direction in _OUTPUT_GROUPS:
        signals = [
            s
            for s in concretisation.concretised
            if s.validity == validity
            and (direction is None or s.direction == direction)
        ]
        path = out / f"{filename}.jsonl"
        path.write_text(
            "\n".join(json.dumps(_signal_to_dict(s, gemini_metadata)) for s in signals)
            + "\n",
            encoding="utf-8",
        )
        print(f"  {filename}: {len(signals):4d} signals → {path}")


if __name__ == "__main__":
    main()
