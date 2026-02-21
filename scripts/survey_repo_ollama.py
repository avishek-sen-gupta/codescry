"""Run the full survey pipeline using a local Ollama LLM for concretisation.

Identical to survey_repo.py up to the concretisation step, but instead of the
TF-IDF + Logistic Regression classifier, passes each unique file with detected
signals to a local Ollama model and asks it to classify every signal as
DEFINITE_INWARD, DEFINITE_OUTWARD, or NOT_DEFINITE.

The ML-based pipeline is preserved unchanged — this is a parallel approach.

Usage:
    poetry run python scripts/survey_repo_ollama.py /path/to/repo
    poetry run python scripts/survey_repo_ollama.py /path/to/repo --languages Java
    poetry run python scripts/survey_repo_ollama.py /path/to/repo \\
        --model qwen2.5-coder:7b-instruct --ollama-url http://localhost:11434
"""

import argparse
import json
import logging
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from repo_surveyor import survey
from repo_surveyor.integration_concretiser.ollama_concretiser import (
    _DEFAULT_MODEL,
    _DEFAULT_OLLAMA_BASE_URL,
    concretise_with_ollama,
)
from repo_surveyor.pipeline_timer import PipelineTimingObserver
from repo_surveyor.integration_concretiser.types import SignalValidity
from repo_surveyor.integration_patterns import SignalDirection
from repo_surveyor.training.signal_classifier import NullSignalClassifier

logger = logging.getLogger(__name__)


def _signal_to_dict(s, label_map: dict) -> dict:
    """Serialise a ConcretisedSignal to a JSON-friendly dict."""
    ollama_meta = label_map.get(
        (s.original_signal.match.file_path, s.original_signal.match.line_number), {}
    )
    base = s.original_signal.to_dict()
    return {
        "validity": s.validity.value,
        "direction": s.direction.value,
        "ollama_confidence": ollama_meta.get("confidence"),
        "ollama_reason": ollama_meta.get("reason"),
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
        help=f"Ollama model name (default: {_DEFAULT_MODEL}).",
    )
    parser.add_argument(
        "--ollama-url",
        default=_DEFAULT_OLLAMA_BASE_URL,
        metavar="URL",
        help=f"Ollama API base URL (default: {_DEFAULT_OLLAMA_BASE_URL}).",
    )
    parser.add_argument(
        "--output-dir",
        default="data/survey_output_ollama",
        metavar="DIR",
        help="Directory to write per-label JSONL files (default: data/survey_output_ollama).",
    )
    return parser.parse_args()


def main() -> None:
    args = _parse_args()

    logging.basicConfig(
        level=logging.DEBUG,
        format="%(asctime)s %(levelname)-8s %(name)s: %(message)s",
        stream=sys.stdout,
    )

    logger.info("=== Ollama Survey Pipeline ===")
    logger.info("Repo:       %s", args.repo_path)
    logger.info("Languages:  %s", args.languages or "all")
    logger.info("Model:      %s", args.model)
    logger.info("Ollama URL: %s", args.ollama_url)
    logger.info("Output dir: %s", args.output_dir)

    # --- Phase 1: run the pre-concretisation pipeline (same as ML pipeline) ---
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

    # --- Phase 2: Ollama-based concretisation ---
    logger.info("--- Phase 2: Ollama-based concretisation ---")
    concretisation, ollama_metadata = concretise_with_ollama(
        integration,
        model=args.model,
        base_url=args.ollama_url,
    )

    # Print summary
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
            "\n".join(json.dumps(_signal_to_dict(s, ollama_metadata)) for s in signals)
            + "\n",
            encoding="utf-8",
        )
        print(f"  {filename}: {len(signals):4d} signals → {path}")


if __name__ == "__main__":
    main()
