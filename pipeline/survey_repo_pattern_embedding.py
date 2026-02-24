"""Run the full survey pipeline using pattern-description embedding concretisation.

Instead of 26 generic directional descriptions, this pipeline uses the
per-pattern descriptions attached to every regex pattern.  A nearest-neighbor
lookup against these pre-embedded descriptions classifies each signal,
capturing framework-specific semantics.

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
    No API server needed.  768-dim, 8192-token context.  Use --model to customise.
  - bge: BAAI/bge-base-en-v1.5 via local sentence-transformers
    No API server needed.  768-dim, general-purpose semantic embedding.
    Use --model to customise.

Usage:
    poetry run python pipeline/survey_repo_pattern_embedding.py /path/to/repo
    poetry run python pipeline/survey_repo_pattern_embedding.py /path/to/repo --backend gemini
    poetry run python pipeline/survey_repo_pattern_embedding.py /path/to/repo --backend ollama
    poetry run python pipeline/survey_repo_pattern_embedding.py /path/to/repo --backend hf-local
    poetry run python pipeline/survey_repo_pattern_embedding.py /path/to/repo --backend coderank
    poetry run python pipeline/survey_repo_pattern_embedding.py /path/to/repo --backend bge
    poetry run python pipeline/survey_repo_pattern_embedding.py /path/to/repo --languages Java
"""

import argparse
import json
import logging
import os
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from repo_surveyor import survey
from repo_surveyor.integration_concretiser.embedding_concretiser import (
    BGEEmbeddingClient,
    CodeRankEmbeddingClient,
    EmbeddingClient,
    GeminiEmbeddingClient,
    HuggingFaceLocalEmbeddingClient,
    OllamaEmbeddingClient,
    _BGE_DEFAULT_MODEL,
    _CODERANK_DEFAULT_MODEL,
)
from repo_surveyor.integration_concretiser.pattern_embedding_concretiser import (
    PatternEmbeddingConcretiser,
    _default_cache_path,
)
from repo_surveyor.core.pipeline_timer import PipelineTimingObserver
from repo_surveyor.integration_concretiser.types import SignalValidity
from repo_surveyor.integration_patterns import SignalDirection
from repo_surveyor.training.signal_classifier import NullSignalClassifier

logger = logging.getLogger(__name__)

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


def _create_client(
    args: argparse.Namespace,
) -> (
    EmbeddingClient
    | GeminiEmbeddingClient
    | OllamaEmbeddingClient
    | HuggingFaceLocalEmbeddingClient
):
    """Create the appropriate embedding client based on the backend choice."""
    model = _resolve_model(args)
    if args.backend == "bge":
        return BGEEmbeddingClient(model_name=model)
    if args.backend == "coderank":
        return CodeRankEmbeddingClient(model_name=model)
    if args.backend == "hf-local":
        return HuggingFaceLocalEmbeddingClient(model_name=model)
    if args.backend == "ollama":
        return OllamaEmbeddingClient(model=model, base_url=args.ollama_url)
    if args.backend == "gemini":
        api_key = os.environ["GEMINI_001_EMBEDDING_API_KEY"]
        return GeminiEmbeddingClient(api_key=api_key)
    endpoint_url = os.environ["HUGGING_FACE_URL"]
    api_token = os.environ["HUGGING_FACE_API_TOKEN"]
    return EmbeddingClient(endpoint_url=endpoint_url, token=api_token)


def _signal_to_dict(s, label_map: dict) -> dict:
    """Serialise a ConcretisedSignal to a JSON-friendly dict."""
    emb_meta = label_map.get(
        (s.original_signal.match.file_path, s.original_signal.match.line_number),
        {},
    )
    base = s.original_signal.to_dict()
    return {
        "validity": s.validity.value,
        "direction": s.direction.value,
        "nearest_description": emb_meta.get("nearest_description"),
        "nearest_type": emb_meta.get("nearest_type"),
        "nearest_direction": emb_meta.get("nearest_direction"),
        "nearest_source": emb_meta.get("nearest_source"),
        "nearest_score": emb_meta.get("score"),
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
        choices=["huggingface", "gemini", "ollama", "hf-local", "coderank", "bge"],
        default="huggingface",
        help=(
            "Embedding backend: huggingface (nomic-embed-code), "
            "gemini (gemini-embedding-001), "
            "ollama (local, default model jina/jina-embeddings-v2-base-code), "
            "hf-local (local HuggingFace transformers, default model "
            "Salesforce/codet5p-110m-embedding), "
            "coderank (local sentence-transformers, default model "
            "nomic-ai/CodeRankEmbed), or "
            "bge (local sentence-transformers, default model "
            "BAAI/bge-base-en-v1.5). "
            "Default: huggingface."
        ),
    )
    parser.add_argument(
        "--model",
        default="",
        help=(
            "Model name (used with --backend ollama, hf-local, coderank, or bge). "
            "Defaults: unclemusclez/jina-embeddings-v2-base-code for ollama, "
            "Salesforce/codet5p-110m-embedding for hf-local, "
            "nomic-ai/CodeRankEmbed for coderank, "
            "BAAI/bge-base-en-v1.5 for bge."
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
        default=0.62,
        help="Cosine similarity threshold for signal vs noise (default: 0.62).",
    )
    parser.add_argument(
        "--output-dir",
        default="data/survey_output_pattern_embedding",
        metavar="DIR",
        help=(
            "Directory to write per-label JSONL files "
            "(default: data/survey_output_pattern_embedding)."
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

    logger.info("=== Pattern-Embedding Survey Pipeline ===")
    logger.info("Repo:       %s", args.repo_path)
    logger.info("Languages:  %s", args.languages or "all")
    logger.info("Backend:    %s", args.backend)
    logger.info("Threshold:  %.3f", args.threshold)
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

    # --- Phase 2: Pattern-embedding concretisation ---
    logger.info("--- Phase 2: Pattern-embedding concretisation ---")
    client = _create_client(args)
    model = _resolve_model(args)
    cache_path = _default_cache_path(args.backend, model=model)
    logger.info("Embedding cache path: %s", cache_path)
    concretiser = PatternEmbeddingConcretiser(
        client, threshold=args.threshold, cache_path=cache_path
    )
    concretisation, embedding_metadata = concretiser.concretise(integration)

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
            "\n".join(
                json.dumps(_signal_to_dict(s, embedding_metadata)) for s in signals
            )
            + "\n",
            encoding="utf-8",
        )
        print(f"  {filename}: {len(signals):4d} signals -> {path}")


if __name__ == "__main__":
    main()
