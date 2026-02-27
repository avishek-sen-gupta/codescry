"""Run the hybrid survey pipeline: framework-specific pattern embedding gate + Gemini Flash direction.

The framework-specific pattern embedding concretiser uses ~3,430 per-pattern
descriptions with distance-weighted KNN for rich semantic SIGNAL/NOISE gating.
Gemini Flash is excellent at direction classification (INWARD vs OUTWARD) but
slower and more expensive (LLM API calls).

This hybrid pipeline:
  1. Runs the pre-concretisation pipeline (stages 1-5)
  2. Runs FrameworkSpecificIntegrationDescriptionEmbeddingConcretiser for
     SIGNAL/NOISE gating (per-pattern descriptions, KNN, cosine threshold)
  3. Sends only the SIGNALs to Gemini Flash for direction classification
  4. Merges: embedding's validity is authoritative, Gemini's direction is
     authoritative for SIGNALs

Supports six embedding backends via --backend:
  - huggingface (default): nomic-embed-code via HuggingFace Inference Endpoint
    Requires HUGGING_FACE_URL and HUGGING_FACE_API_TOKEN env vars.
  - gemini: gemini-embedding-001 via Google Gemini API
    Requires GEMINI_001_EMBEDDING_API_KEY env var.
  - ollama: jina/jina-embeddings-v2-base-code via local Ollama server
    No API key required.  Use --model and --ollama-url to customise.
  - hf-local: Salesforce/codet5p-110m-embedding via local HuggingFace transformers
    No API server needed.  Use --model to customise.
  - coderank: nomic-ai/CodeRankEmbed via local sentence-transformers
    No API server needed.  Use --model to customise.
  - bge: BAAI/bge-base-en-v1.5 via local sentence-transformers
    No API server needed.  Use --model to customise.

Requires GEMINI_001_EMBEDDING_API_KEY env var for Gemini Flash direction classification.

Usage:
    poetry run python pipeline/survey_classify_by_framework_specific_pattern_embedding_gate_gemini_flash.py /path/to/repo
    poetry run python pipeline/survey_classify_by_framework_specific_pattern_embedding_gate_gemini_flash.py /path/to/repo --backend bge
    poetry run python pipeline/survey_classify_by_framework_specific_pattern_embedding_gate_gemini_flash.py /path/to/repo --backend bge --languages Java
"""

import argparse
import json
import logging
import os
import sys
from datetime import datetime, timezone
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from repo_surveyor import survey
from repo_surveyor.core.pipeline_timer import PipelineTimingObserver
from repo_surveyor.detection.integration_detector import IntegrationDetectorResult
from repo_surveyor.integration_concretiser.embedding_concretiser import (
    EmbeddingClient,
    GeminiEmbeddingClient,
    HuggingFaceLocalEmbeddingClient,
    OllamaEmbeddingClient,
    SentenceTransformerEmbeddingClient,
    create_bge_embedding_client,
    create_coderank_embedding_client,
    _BGE_DEFAULT_MODEL,
    _CODERANK_DEFAULT_MODEL,
    _CODERANK_QUERY_PREFIX,
)
from repo_surveyor.integration_concretiser.gemini_concretiser import (
    concretise_with_gemini,
)
from repo_surveyor.integration_concretiser.pattern_embedding_concretiser import (
    FrameworkSpecificIntegrationDescriptionEmbeddingConcretiser,
    _default_cache_path,
)
from repo_surveyor.integration_concretiser.types import (
    ConcretisationResult,
    ConcretisedSignal,
    SignalValidity,
)
from repo_surveyor.integration_patterns import SignalDirection
from repo_surveyor.training.signal_classifier import NullSignalClassifier

logger = logging.getLogger(__name__)

_ENV_VAR_GEMINI_API_KEY = "GEMINI_001_EMBEDDING_API_KEY"

_BACKEND_DEFAULT_MODELS: dict[str, str] = {
    "ollama": "unclemusclez/jina-embeddings-v2-base-code",
    "hf-local": "Salesforce/codet5p-110m-embedding",
    "coderank": _CODERANK_DEFAULT_MODEL,
    "bge": _BGE_DEFAULT_MODEL,
}


def _resolve_model(args: argparse.Namespace) -> str:
    """Return the effective model name, applying per-backend defaults."""
    if args.model:
        return args.model
    return _BACKEND_DEFAULT_MODELS.get(args.backend, "")


def _create_embedding_client(
    args: argparse.Namespace,
) -> (
    EmbeddingClient
    | GeminiEmbeddingClient
    | OllamaEmbeddingClient
    | HuggingFaceLocalEmbeddingClient
    | SentenceTransformerEmbeddingClient
):
    """Create the appropriate embedding client based on the backend choice."""
    model = _resolve_model(args)
    if args.backend == "bge":
        return create_bge_embedding_client(model_name=model)
    if args.backend == "coderank":
        return create_coderank_embedding_client(
            model_name=model, query_prefix=_CODERANK_QUERY_PREFIX
        )
    if args.backend == "hf-local":
        return HuggingFaceLocalEmbeddingClient(model_name=model)
    if args.backend == "ollama":
        return OllamaEmbeddingClient(model=model, base_url=args.ollama_url)
    if args.backend == "gemini":
        api_key = os.environ[_ENV_VAR_GEMINI_API_KEY]
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
    If Gemini returned NOT_INTEGRATION, keep validity=SIGNAL but set
    direction=NOT_INTEGRATION.

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
    merged_metadata = {
        key: {**embedding_metadata.get(key, {}), **gemini_metadata.get(key, {})}
        for key in set(embedding_metadata) | set(gemini_metadata)
    }

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
    elif gemini_signal.direction == SignalDirection.NOT_INTEGRATION:
        direction = SignalDirection.NOT_INTEGRATION
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
        "nearest_description": meta.get("nearest_description"),
        "nearest_type": meta.get("nearest_type"),
        "nearest_direction": meta.get("nearest_direction"),
        "nearest_source": meta.get("nearest_source"),
        "nearest_score": meta.get("score"),
        "nearest_k": meta.get("k"),
        "nearest_k_avg_score": meta.get("avg_score"),
        "gemini_confidence": meta.get("confidence"),
        "gemini_reason": meta.get("reason"),
        **base,
        "ast_node_type": s.ast_context.node_type,
        "ast_start_line": s.ast_context.start_line,
        "ast_end_line": s.ast_context.end_line,
        "ast_node_text": s.ast_context.node_text,
    }


_OUTPUT_GROUPS = [
    ("inward", SignalValidity.SIGNAL, SignalDirection.INWARD),
    ("outward", SignalValidity.SIGNAL, SignalDirection.OUTWARD),
    ("ambiguous", SignalValidity.SIGNAL, SignalDirection.AMBIGUOUS),
    ("not_integration", SignalValidity.SIGNAL, SignalDirection.NOT_INTEGRATION),
    ("noise", SignalValidity.NOISE, None),
]


def _build_output_dir(args: argparse.Namespace) -> str:
    """Build a timestamped output directory name encoding the technique used."""
    if args.output_dir:
        return args.output_dir
    timestamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%S")
    model = _resolve_model(args).replace("/", "--") if _resolve_model(args) else ""
    parts = ["fw_pattern_gate_gemini_flash", args.backend]
    if model:
        parts.append(model)
    parts.append(f"k{args.k}")
    parts.append(f"t{args.threshold}")
    parts.append(timestamp)
    return str(Path("data") / "survey_output" / "_".join(parts))


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
        choices=["huggingface", "gemini", "ollama", "hf-local", "coderank", "bge"],
        default="huggingface",
        help=(
            "Embedding backend: huggingface (nomic-embed-code), "
            "gemini (gemini-embedding-001), "
            "ollama (local, default model jina/jina-embeddings-v2-base-code), "
            "hf-local (local HuggingFace transformers), "
            "coderank (local sentence-transformers, nomic-ai/CodeRankEmbed), or "
            "bge (local sentence-transformers, BAAI/bge-base-en-v1.5). "
            "Default: huggingface."
        ),
    )
    parser.add_argument(
        "--model",
        default="",
        help=(
            "Model name (used with --backend ollama, hf-local, coderank, or bge). "
            "Each backend has a sensible default."
        ),
    )
    parser.add_argument(
        "--ollama-url",
        default="http://localhost:11434",
        help=(
            "Ollama server base URL (only used with --backend ollama). "
            "Default: http://localhost:11434."
        ),
    )
    parser.add_argument(
        "--threshold",
        type=float,
        default=0.68,
        metavar="T",
        help="Cosine similarity threshold for SIGNAL/NOISE gate (default: 0.68).",
    )
    parser.add_argument(
        "--k",
        type=int,
        default=5,
        help="Number of nearest neighbours for KNN classification (default: 5).",
    )
    parser.add_argument(
        "--output-dir",
        default="",
        metavar="DIR",
        help=(
            "Directory to write per-label JSONL files. "
            "Default: auto-generated with timestamp and technique."
        ),
    )
    return parser.parse_args()


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

    output_dir = _build_output_dir(args)

    logger.info(
        "=== Hybrid Pipeline (FW-Specific Pattern Embedding Gate + Gemini Flash) ==="
    )
    logger.info("Repo:       %s", args.repo_path)
    logger.info("Languages:  %s", args.languages or "all")
    logger.info("Backend:    %s", args.backend)
    logger.info("Threshold:  %.3f", args.threshold)
    logger.info("K (neighbours): %d", args.k)
    logger.info("Output dir: %s", output_dir)

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

    # --- Phase 2a: framework-specific pattern embedding gate ---
    logger.info(
        "--- Phase 2a: Framework-specific pattern embedding SIGNAL/NOISE gate ---"
    )
    embedding_client = _create_embedding_client(args)
    model = _resolve_model(args)
    cache_path = _default_cache_path(args.backend, model=model)
    logger.info("Embedding cache path: %s", cache_path)
    concretiser = FrameworkSpecificIntegrationDescriptionEmbeddingConcretiser(
        embedding_client, threshold=args.threshold, cache_path=cache_path, k=args.k
    )
    embedding_result, embedding_metadata = concretiser.concretise(integration)

    signal_count = sum(
        1 for cs in embedding_result.concretised if cs.validity == SignalValidity.SIGNAL
    )
    noise_count = len(embedding_result.concretised) - signal_count
    logger.info(
        "Pattern embedding gate: %d SIGNALs, %d NOISE (total %d)",
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
    logger.info("--- Phase 3: writing output files ---")
    out = Path(output_dir)
    out.mkdir(parents=True, exist_ok=True)

    timings_path = out / "timings.json"
    timings_path.write_text(timer.to_json(), encoding="utf-8")
    logger.info("Timings written to %s", timings_path)

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
        logger.info("  %s: %4d signals -> %s", filename, len(signals), path)

    logger.info("Output written to %s", out.resolve())


if __name__ == "__main__":
    main()
