"""Run the hybrid survey pipeline: embedding gate + Gemini Flash direction.

The embedding concretiser is fast and good at SIGNAL/NOISE gating (cosine
similarity threshold), but poor at direction resolution (INWARD vs OUTWARD).
The Gemini Flash concretiser is excellent at direction but slower and more
expensive (LLM API calls).

This hybrid pipeline:
  1. Runs the pre-concretisation pipeline (stages 1-5)
  2. Runs EmbeddingConcretiser for SIGNAL/NOISE gating
  3. Sends only the SIGNALs to Gemini Flash for direction classification
  4. Merges: embedding's validity is authoritative, Gemini's direction is
     authoritative for SIGNALs

Supports two embedding backends via --backend:
  - huggingface (default): nomic-embed-code via HuggingFace Inference Endpoint
    Requires HUGGING_FACE_URL and HUGGING_FACE_API_TOKEN env vars.
  - gemini: gemini-embedding-001 via Google Gemini API
    Requires GEMINI_001_EMBEDDING_API_KEY env var.

Requires GEMINI_API_KEY env var for the Gemini Flash direction classifier.

Usage:
    poetry run python scripts/survey_repo_hybrid.py /path/to/repo
    poetry run python scripts/survey_repo_hybrid.py /path/to/repo --backend gemini
    poetry run python scripts/survey_repo_hybrid.py /path/to/repo --languages Java
"""

import argparse
import json
import logging
import os
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from repo_surveyor import survey
from repo_surveyor.core.pipeline_timer import PipelineTimingObserver
from repo_surveyor.detection.integration_detector import IntegrationDetectorResult
from repo_surveyor.integration_concretiser.embedding_concretiser import (
    EmbeddingClient,
    EmbeddingConcretiser,
    GeminiEmbeddingClient,
)
from repo_surveyor.integration_concretiser.gemini_concretiser import (
    concretise_with_gemini,
)
from repo_surveyor.integration_concretiser.types import (
    ConcretisationResult,
    ConcretisedSignal,
    SignalValidity,
)
from repo_surveyor.integration_patterns import SignalDirection
from repo_surveyor.training.signal_classifier import NullSignalClassifier

logger = logging.getLogger(__name__)

_ENV_VAR_GEMINI_API_KEY = "GEMINI_API_KEY"
_ENV_VAR_GEMINI_EMBEDDING_API_KEY = "GEMINI_001_EMBEDDING_API_KEY"


def _create_embedding_client(
    backend: str,
) -> EmbeddingClient | GeminiEmbeddingClient:
    """Create the appropriate embedding client based on the backend choice."""
    if backend == "gemini":
        api_key = os.environ[_ENV_VAR_GEMINI_EMBEDDING_API_KEY]
        return GeminiEmbeddingClient(api_key=api_key)
    endpoint_url = os.environ["HUGGING_FACE_URL"]
    api_token = os.environ["HUGGING_FACE_API_TOKEN"]
    return EmbeddingClient(endpoint_url=endpoint_url, token=api_token)


def merge_results(
    embedding_result: ConcretisationResult,
    gemini_result: ConcretisationResult,
    embedding_metadata: dict[tuple[str, int], dict],
    gemini_metadata: dict[tuple[str, int], dict],
) -> tuple[ConcretisationResult, dict[tuple[str, int], dict]]:
    """Merge embedding validity with Gemini direction classification.

    Embedding's validity is authoritative (SIGNAL vs NOISE).
    Gemini's direction is authoritative for SIGNALs.
    If Gemini returned NOT_DEFINITE for a SIGNAL, keep validity=SIGNAL
    but set direction=AMBIGUOUS.

    Args:
        embedding_result: Full concretisation result from embedding gate.
        gemini_result: Concretisation result from Gemini (SIGNALs only).
        embedding_metadata: Metadata from the embedding concretiser.
        gemini_metadata: Metadata from the Gemini concretiser.

    Returns:
        Tuple of (merged ConcretisationResult, merged metadata).
    """
    gemini_lookup: dict[tuple[str, int], ConcretisedSignal] = {
        (cs.original_signal.match.file_path, cs.original_signal.match.line_number): cs
        for cs in gemini_result.concretised
    }

    merged: list[ConcretisedSignal] = [
        _merge_single(cs, gemini_lookup) for cs in embedding_result.concretised
    ]

    classified = sum(1 for s in merged if s.is_integration)
    merged_metadata = {**embedding_metadata, **gemini_metadata}

    return (
        ConcretisationResult(
            concretised=tuple(merged),
            signals_submitted=len(merged),
            signals_classified=classified,
            signals_unclassified=len(merged) - classified,
        ),
        merged_metadata,
    )


def _merge_single(
    embedding_signal: ConcretisedSignal,
    gemini_lookup: dict[tuple[str, int], ConcretisedSignal],
) -> ConcretisedSignal:
    """Merge a single embedding signal with Gemini's direction."""
    if embedding_signal.validity == SignalValidity.NOISE:
        return embedding_signal

    key = (
        embedding_signal.original_signal.match.file_path,
        embedding_signal.original_signal.match.line_number,
    )
    gemini_signal = gemini_lookup.get(key)

    if gemini_signal is None:
        direction = SignalDirection.AMBIGUOUS
    elif gemini_signal.validity == SignalValidity.NOISE:
        direction = SignalDirection.AMBIGUOUS
    else:
        direction = gemini_signal.direction

    return ConcretisedSignal(
        original_signal=embedding_signal.original_signal,
        ast_context=embedding_signal.ast_context,
        validity=SignalValidity.SIGNAL,
        direction=direction,
    )


def _signal_to_dict(
    s: ConcretisedSignal,
    metadata: dict[tuple[str, int], dict],
) -> dict:
    """Serialise a ConcretisedSignal to a JSON-friendly dict."""
    key = (s.original_signal.match.file_path, s.original_signal.match.line_number)
    meta = metadata.get(key, {})
    base = s.original_signal.to_dict()
    return {
        "validity": s.validity.value,
        "direction": s.direction.value,
        "embedding_best_type": meta.get("best_type"),
        "embedding_best_direction": meta.get("best_direction"),
        "embedding_score": meta.get("score"),
        "gemini_confidence": meta.get("confidence"),
        "gemini_reason": meta.get("reason"),
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
        "--backend",
        choices=["huggingface", "gemini"],
        default="huggingface",
        help="Embedding backend: huggingface (nomic-embed-code) or gemini "
        "(gemini-embedding-001). Default: huggingface.",
    )
    parser.add_argument(
        "--output-dir",
        default="data/survey_output_hybrid",
        metavar="DIR",
        help="Directory to write per-label JSONL files "
        "(default: data/survey_output_hybrid).",
    )
    return parser.parse_args()


_OUTPUT_GROUPS = [
    ("inward", SignalValidity.SIGNAL, SignalDirection.INWARD),
    ("outward", SignalValidity.SIGNAL, SignalDirection.OUTWARD),
    ("ambiguous", SignalValidity.SIGNAL, SignalDirection.AMBIGUOUS),
    ("noise", SignalValidity.NOISE, None),
]


def main() -> None:
    args = _parse_args()

    logging.basicConfig(
        level=logging.DEBUG,
        format="%(asctime)s %(levelname)-8s %(name)s: %(message)s",
        stream=sys.stdout,
    )

    gemini_api_key = os.environ.get(_ENV_VAR_GEMINI_API_KEY, "")
    if not gemini_api_key:
        logger.error(
            "Set the %s environment variable to your Google AI API key.",
            _ENV_VAR_GEMINI_API_KEY,
        )
        sys.exit(1)

    logger.info("=== Hybrid Survey Pipeline (Embedding Gate + Gemini Direction) ===")
    logger.info("Repo:       %s", args.repo_path)
    logger.info("Languages:  %s", args.languages or "all")
    logger.info("Backend:    %s", args.backend)
    logger.info("Output dir: %s", args.output_dir)

    # --- Phase 1: pre-concretisation pipeline ---
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

    # --- Phase 2a: embedding gate ---
    logger.info("--- Phase 2a: Embedding-based SIGNAL/NOISE gate ---")
    embedding_client = _create_embedding_client(args.backend)
    concretiser = EmbeddingConcretiser(embedding_client)
    embedding_result, embedding_metadata = concretiser.concretise(integration)

    signal_count = sum(
        1 for cs in embedding_result.concretised if cs.validity == SignalValidity.SIGNAL
    )
    noise_count = len(embedding_result.concretised) - signal_count
    logger.info(
        "Embedding gate: %d SIGNALs, %d NOISE (total %d)",
        signal_count,
        noise_count,
        len(embedding_result.concretised),
    )

    # --- Phase 2b: Gemini direction on SIGNALs only ---
    logger.info("--- Phase 2b: Gemini Flash direction classification ---")
    signal_originals = [
        cs.original_signal
        for cs in embedding_result.concretised
        if cs.validity == SignalValidity.SIGNAL
    ]

    filtered_detector = IntegrationDetectorResult(
        integration_points=signal_originals,
        files_scanned=integration.files_scanned,
    )
    gemini_result, gemini_metadata = concretise_with_gemini(
        filtered_detector,
        api_key=gemini_api_key,
    )

    # --- Phase 2c: merge results ---
    logger.info("--- Phase 2c: Merging embedding validity + Gemini direction ---")
    final_result, final_metadata = merge_results(
        embedding_result, gemini_result, embedding_metadata, gemini_metadata
    )

    # Print summary
    print(f"\nFiles scanned:  {integration.files_scanned}")
    print(f"Signals found:  {len(integration.integration_points)}")
    print(f"\nSubmitted:      {final_result.signals_submitted}")
    print(f"Classified:     {final_result.signals_classified}")
    print(f"Unclassified:   {final_result.signals_unclassified}")

    print("\n=== Timings ===")
    for r in timer.completed:
        print(f"  {r.stage:<50s} {r.duration_seconds:>8.2f} s")

    # --- Phase 3: write output files ---
    out = Path(args.output_dir)
    out.mkdir(parents=True, exist_ok=True)

    print()
    for filename, validity, direction in _OUTPUT_GROUPS:
        signals = [
            s
            for s in final_result.concretised
            if s.validity == validity
            and (direction is None or s.direction == direction)
        ]
        path = out / f"{filename}.jsonl"
        path.write_text(
            "\n".join(json.dumps(_signal_to_dict(s, final_metadata)) for s in signals)
            + "\n",
            encoding="utf-8",
        )
        print(f"  {filename}: {len(signals):4d} signals -> {path}")


if __name__ == "__main__":
    main()
