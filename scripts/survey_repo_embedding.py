"""Run the full survey pipeline using embedding-based concretisation.

Identical to survey_repo.py up to the concretisation step, but instead of the
TF-IDF + Logistic Regression classifier, uses code embeddings and cosine
similarity against 26 directional descriptions to classify each signal as
DEFINITE_INWARD, DEFINITE_OUTWARD, or NOT_DEFINITE.

Supports two embedding backends via --backend:
  - huggingface (default): nomic-embed-code via HuggingFace Inference Endpoint
    Requires HUGGING_FACE_URL and HUGGING_FACE_API_TOKEN env vars.
  - gemini: gemini-embedding-001 via Google Gemini API
    Requires GEMINI_API_KEY env var.

Usage:
    poetry run python scripts/survey_repo_embedding.py /path/to/repo
    poetry run python scripts/survey_repo_embedding.py /path/to/repo --backend gemini
    poetry run python scripts/survey_repo_embedding.py /path/to/repo --languages Java
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
    EmbeddingClient,
    EmbeddingConcretiser,
    GeminiEmbeddingClient,
)
from repo_surveyor.core.pipeline_timer import PipelineTimingObserver
from repo_surveyor.training.signal_classifier import NullSignalClassifier
from repo_surveyor.training.types import TrainingLabel

logger = logging.getLogger(__name__)


def _create_client(backend: str) -> EmbeddingClient | GeminiEmbeddingClient:
    """Create the appropriate embedding client based on the backend choice."""
    if backend == "gemini":
        api_key = os.environ["GEMINI_001_EMBEDDING_API_KEY"]
        return GeminiEmbeddingClient(api_key=api_key)
    endpoint_url = os.environ["HUGGING_FACE_URL"]
    api_token = os.environ["HUGGING_FACE_API_TOKEN"]
    return EmbeddingClient(endpoint_url=endpoint_url, token=api_token)


def _signal_to_dict(s, label_map: dict) -> dict:
    """Serialise a ConcretisedSignal to a JSON-friendly dict."""
    emb_meta = label_map.get(
        (s.original_signal.match.file_path, s.original_signal.match.line_number), {}
    )
    base = s.original_signal.to_dict()
    return {
        "label": s.label.value,
        "embedding_best_type": emb_meta.get("best_type"),
        "embedding_best_direction": emb_meta.get("best_direction"),
        "embedding_score": emb_meta.get("score"),
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
        help="Embedding backend: huggingface (nomic-embed-code) or gemini (gemini-embedding-001). Default: huggingface.",
    )
    parser.add_argument(
        "--output-dir",
        default="data/survey_output_embedding",
        metavar="DIR",
        help="Directory to write per-label JSONL files (default: data/survey_output_embedding).",
    )
    return parser.parse_args()


def main() -> None:
    args = _parse_args()

    logging.basicConfig(
        level=logging.DEBUG,
        format="%(asctime)s %(levelname)-8s %(name)s: %(message)s",
        stream=sys.stdout,
    )

    logger.info("=== Embedding Survey Pipeline ===")
    logger.info("Repo:       %s", args.repo_path)
    logger.info("Languages:  %s", args.languages or "all")
    logger.info("Backend:    %s", args.backend)
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

    # --- Phase 2: Embedding-based concretisation ---
    logger.info("--- Phase 2: Embedding-based concretisation ---")
    client = _create_client(args.backend)
    concretiser = EmbeddingConcretiser(client)
    concretisation, embedding_metadata = concretiser.concretise(integration)

    # Print summary
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
            "\n".join(
                json.dumps(_signal_to_dict(s, embedding_metadata)) for s in signals
            )
            + "\n",
            encoding="utf-8",
        )
        print(f"  {label.value}: {len(signals):4d} signals -> {path}")


if __name__ == "__main__":
    main()
