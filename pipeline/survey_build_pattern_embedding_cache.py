"""Pre-build the pattern description embedding cache.

Embeds all ~3,430 pattern description strings and saves the result to a
JSON cache file.  Subsequent runs of the pattern-embedding survey pipeline
will load these cached embeddings instead of re-calling the embedding API.

Usage:
    poetry run python pipeline/survey_build_pattern_embedding_cache.py --backend gemini
    poetry run python pipeline/survey_build_pattern_embedding_cache.py --backend huggingface
    poetry run python pipeline/survey_build_pattern_embedding_cache.py --backend ollama
    poetry run python pipeline/survey_build_pattern_embedding_cache.py --backend hf-local
    poetry run python pipeline/survey_build_pattern_embedding_cache.py --backend coderank
    poetry run python pipeline/survey_build_pattern_embedding_cache.py --backend bge
"""

import argparse
import logging
import os
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from repo_surveyor.integration_concretiser.embedding_concretiser import (
    BGEEmbeddingClient,
    CodeRankEmbeddingClient,
    EmbeddingClient,
    GeminiEmbeddingClient,
    HuggingFaceLocalEmbeddingClient,
    OllamaEmbeddingClient,
    _BGE_DEFAULT_MODEL,
    _CODERANK_DEFAULT_MODEL,
    _CODERANK_QUERY_PREFIX,
)
from repo_surveyor.integration_concretiser.pattern_embedding_concretiser import (
    _compute_content_hash,
    _default_cache_path,
    _save_cache,
)
from repo_surveyor.integration_patterns import get_all_pattern_descriptions

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
        return CodeRankEmbeddingClient(
            model_name=model, query_prefix=_CODERANK_QUERY_PREFIX
        )
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


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--backend",
        choices=["huggingface", "gemini", "ollama", "hf-local", "coderank", "bge"],
        default="huggingface",
        help=(
            "Embedding backend: huggingface (nomic-embed-code), "
            "gemini (gemini-embedding-001), "
            "ollama (local, default model unclemusclez/jina-embeddings-v2-base-code), "
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

    model = _resolve_model(args)
    cache_path = (
        Path(args.output)
        if args.output
        else _default_cache_path(args.backend, model=model)
    )

    descriptions = get_all_pattern_descriptions()
    logger.info("Collected %d pattern descriptions", len(descriptions))
    logger.info("Content hash: %s", _compute_content_hash(descriptions))

    client = _create_client(args)
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
