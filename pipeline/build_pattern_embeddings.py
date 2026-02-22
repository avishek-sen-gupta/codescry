"""Pre-build the pattern description embedding cache.

Embeds all ~3,430 pattern description strings and saves the result to a
JSON cache file.  Subsequent runs of the pattern-embedding survey pipeline
will load these cached embeddings instead of re-calling the embedding API.

Usage:
    poetry run python pipeline/build_pattern_embeddings.py --backend gemini
    poetry run python pipeline/build_pattern_embeddings.py --backend huggingface
"""

import argparse
import logging
import os
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from repo_surveyor.integration_concretiser.embedding_concretiser import (
    EmbeddingClient,
    GeminiEmbeddingClient,
)
from repo_surveyor.integration_concretiser.pattern_embedding_concretiser import (
    _compute_content_hash,
    _default_cache_path,
    _save_cache,
)
from repo_surveyor.integration_patterns import get_all_pattern_descriptions

logger = logging.getLogger(__name__)


def _create_client(backend: str) -> EmbeddingClient | GeminiEmbeddingClient:
    """Create the appropriate embedding client based on the backend choice."""
    if backend == "gemini":
        api_key = os.environ["GEMINI_001_EMBEDDING_API_KEY"]
        return GeminiEmbeddingClient(api_key=api_key)
    endpoint_url = os.environ["HUGGING_FACE_URL"]
    api_token = os.environ["HUGGING_FACE_API_TOKEN"]
    return EmbeddingClient(endpoint_url=endpoint_url, token=api_token)


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--backend",
        choices=["huggingface", "gemini"],
        default="huggingface",
        help=(
            "Embedding backend: huggingface (nomic-embed-code) or "
            "gemini (gemini-embedding-001). Default: huggingface."
        ),
    )
    parser.add_argument(
        "--output",
        default="",
        metavar="PATH",
        help="Override output path (default: data/embeddings/pattern_description_embeddings_{backend}.json).",
    )
    return parser.parse_args()


def main() -> None:
    args = _parse_args()

    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s %(levelname)-8s %(name)s: %(message)s",
        stream=sys.stdout,
    )

    cache_path = Path(args.output) if args.output else _default_cache_path(args.backend)

    descriptions = get_all_pattern_descriptions()
    logger.info("Collected %d pattern descriptions", len(descriptions))
    logger.info("Content hash: %s", _compute_content_hash(descriptions))

    client = _create_client(args.backend)
    desc_texts = [d.text for d in descriptions]

    logger.info("Embedding %d descriptions via %s...", len(desc_texts), args.backend)
    embeddings = client.embed_batch(desc_texts) if desc_texts else []
    logger.info(
        "Embedded %d vectors (dim=%d)",
        len(embeddings),
        len(embeddings[0]) if embeddings else 0,
    )

    _save_cache(cache_path, descriptions, embeddings, backend=args.backend)
    logger.info("Cache saved to %s", cache_path)


if __name__ == "__main__":
    main()
