"""Embedding-based integration signal concretisation.

Uses code embeddings to classify each FILE_CONTENT integration signal as
DEFINITE_INWARD, DEFINITE_OUTWARD, or NOT_DEFINITE by computing cosine
similarity against 26 directional description embeddings (13 integration
types x 2 directions).

Supports six embedding backends:
  - HuggingFace Inference Endpoint (nomic-embed-code) via ``EmbeddingClient``
  - Google Gemini (gemini-embedding-001) via ``GeminiEmbeddingClient``
  - Local Ollama (unclemusclez/jina-embeddings-v2-base-code) via ``OllamaEmbeddingClient``
  - Local HuggingFace transformers (Salesforce/codet5p-110m-embedding) via ``HuggingFaceLocalEmbeddingClient``
  - Local sentence-transformers (nomic-ai/CodeRankEmbed) via ``create_coderank_embedding_client``
  - Local sentence-transformers (BAAI/bge-base-en-v1.5) via ``create_bge_embedding_client``

All implement the same ``embed_batch`` interface so ``GenericIntegrationDescriptionEmbeddingConcretiser``
is backend-agnostic.

Usage:
    from repo_surveyor.integration_concretiser.embedding_concretiser import (
        EmbeddingClient,
        GeminiEmbeddingClient,
        OllamaEmbeddingClient,
        HuggingFaceLocalEmbeddingClient,
        create_coderank_embedding_client,
        create_bge_embedding_client,
        GenericIntegrationDescriptionEmbeddingConcretiser,
    )
    # HuggingFace Inference Endpoint backend
    client = EmbeddingClient(endpoint_url=..., token=...)
    # -- or Gemini backend --
    client = GeminiEmbeddingClient(api_key=...)
    # -- or Ollama backend (local, no API key needed) --
    client = OllamaEmbeddingClient()
    # -- or HuggingFace local backend (no API server needed) --
    client = HuggingFaceLocalEmbeddingClient()
    # -- or CodeRankEmbed backend (local, asymmetric embedding) --
    client = create_coderank_embedding_client()
    # -- or BGE backend (local, general-purpose semantic embedding) --
    client = create_bge_embedding_client()
    concretiser = GenericIntegrationDescriptionEmbeddingConcretiser(client)
    result, metadata = concretiser.concretise(detector_result, file_reader)
"""

import logging
import math
import time
from collections.abc import Callable
from itertools import batched
from typing import Protocol, runtime_checkable

from repo_surveyor.detection.integration_detector import (
    EntityType,
    IntegrationDetectorResult,
)
from repo_surveyor.integration_concretiser.ast_walker import (
    FALLBACK_AST_CONTEXT,
    batch_extract_invocation_contexts,
)
from repo_surveyor.integration_concretiser.types import (
    ASTContext,
    ConcretisationResult,
    ConcretisedSignal,
    SignalLike,
    SignalValidity,
)
from repo_surveyor.integration_patterns import Language, SignalDirection

logger = logging.getLogger(__name__)

_CONFIDENCE_THRESHOLD = 0.56

_BATCH_SIZE = 100

_MAX_RETRIES = 5
_INITIAL_BACKOFF_SECONDS = 2.0
_INTER_BATCH_DELAY_SECONDS = 1.0

_DIRECTIONAL_DESCRIPTIONS: dict[tuple[str, str], str] = {
    ("http_rest", "inward"): (
        "Exposes an HTTP or REST endpoint that listens for and routes"
        " incoming GET, POST, or PUT requests from clients."
    ),
    ("http_rest", "outward"): (
        "Makes an outgoing HTTP or REST request to a remote URL,"
        " such as GET, POST, or PUT to an external web API."
    ),
    ("soap", "inward"): (
        "Implements a SOAP web service endpoint annotated with @WebService"
        " or @WebMethod that receives and processes incoming SOAP XML requests."
    ),
    ("soap", "outward"): (
        "Calls a remote SOAP web service using a WSDL-generated client"
        " proxy with XML envelope serialization."
    ),
    ("messaging", "inward"): (
        "Consumes or listens for incoming messages from a queue or topic"
        " via a message broker subscription."
    ),
    ("messaging", "outward"): (
        "Produces and sends an outgoing message to a queue or topic"
        " via a message broker."
    ),
    ("socket", "inward"): (
        "Binds to a network port and listens for incoming TCP, UDP,"
        " or WebSocket connections from clients."
    ),
    ("socket", "outward"): (
        "Opens an outgoing TCP, UDP, or WebSocket connection by connecting"
        " to a remote host and port."
    ),
    ("database", "inward"): (
        "Defines a database entity, repository, or ORM model class"
        " that maps to and exposes a database table or collection."
    ),
    ("database", "outward"): (
        "Executes a SQL query, ORM find, or database call to read"
        " or write data in a relational database or document store."
    ),
    ("file_io", "inward"): (
        "Accepts an incoming file upload from a client via a multipart"
        " form or upload endpoint."
    ),
    ("file_io", "outward"): (
        "Opens, reads, or writes a file on the local filesystem using"
        " file handles, streams, or path-based I/O."
    ),
    ("grpc", "inward"): (
        "Implements a gRPC service method that handles incoming remote"
        " procedure calls from clients."
    ),
    ("grpc", "outward"): (
        "Creates a gRPC client channel and stub to make an outgoing"
        " remote procedure call to a gRPC service."
    ),
    ("graphql", "inward"): (
        "Defines a GraphQL schema resolver or query field that handles"
        " incoming queries or mutations from clients."
    ),
    ("graphql", "outward"): (
        "Sends a GraphQL query or mutation as a client to a remote"
        " GraphQL API endpoint."
    ),
    ("email", "inward"): (
        "Receives or retrieves incoming email messages from a mail"
        " server using IMAP or POP3 protocol."
    ),
    ("email", "outward"): (
        "Sends an outgoing email message to a recipient via an SMTP server."
    ),
    ("caching", "inward"): (
        "Annotates a method or class as cacheable so its results are"
        " stored and served from cache on subsequent calls."
    ),
    ("caching", "outward"): (
        "Reads from or writes data to an external cache store like"
        " Redis or Memcached using get/set operations."
    ),
    ("sse_streaming", "inward"): (
        "Exposes a server-sent events endpoint that pushes event streams"
        " to connected clients with text/event-stream content type."
    ),
    ("sse_streaming", "outward"): (
        "Connects to a server-sent events endpoint as a client using"
        " EventSource to receive streaming event data."
    ),
    ("scheduling", "inward"): (
        "Annotates a method with @Scheduled or cron decorator so it is"
        " invoked automatically at configured times or intervals."
    ),
    ("scheduling", "outward"): (
        "Programmatically creates and submits a new scheduled job or timer"
        " to a task scheduler or cron system."
    ),
    ("ftp_sftp", "inward"): (
        "Configures an FTP or SFTP server endpoint that accepts incoming"
        " file transfer connections from clients."
    ),
    ("ftp_sftp", "outward"): (
        "Connects to a remote FTP or SFTP server as a client and uploads"
        " or downloads files."
    ),
}

_DIRECTION_STR_TO_ENUM: dict[str, SignalDirection] = {
    "inward": SignalDirection.INWARD,
    "outward": SignalDirection.OUTWARD,
}

_AMBIGUITY_RATIO = 1.2


def cosine(a: list[float], b: list[float]) -> float:
    """Compute cosine similarity between two vectors."""
    dot = sum(x * y for x, y in zip(a, b))
    mag_a = math.sqrt(sum(x * x for x in a))
    mag_b = math.sqrt(sum(y * y for y in b))
    if mag_a == 0 or mag_b == 0:
        return 0.0
    return dot / (mag_a * mag_b)


def _read_file_bytes(file_path: str) -> bytes:
    with open(file_path, "rb") as f:
        return f.read()


@runtime_checkable
class EmbeddingClientProtocol(Protocol):
    """Structural interface for embedding clients."""

    def embed_batch(self, texts: list[str]) -> list[list[float]]: ...


class EmbeddingClient:
    """Thin wrapper over a HuggingFace Inference Endpoint for embeddings."""

    def __init__(self, endpoint_url: str, token: str) -> None:
        self._endpoint_url = endpoint_url
        self._token = token

    def embed_batch(self, texts: list[str]) -> list[list[float]]:
        """Embed a list of texts, batching internally to avoid overflows."""
        import json
        import time
        from urllib.request import Request, urlopen

        total_texts = len(texts)
        total_batches = math.ceil(total_texts / _BATCH_SIZE)
        logger.info(
            "[Nomic] Embedding %d texts in %d batches (batch_size=%d)",
            total_texts,
            total_batches,
            _BATCH_SIZE,
        )

        all_embeddings: list[list[float]] = []
        for batch_idx, chunk in enumerate(batched(texts, _BATCH_SIZE), start=1):
            chunk_list = list(chunk)
            logger.info(
                "[Nomic] Batch %d/%d: sending %d texts to endpoint",
                batch_idx,
                total_batches,
                len(chunk_list),
            )
            result = self._request_with_retry(
                chunk_list, batch_idx, total_batches, json, Request, urlopen
            )
            all_embeddings.extend(result)
            if batch_idx < total_batches:
                time.sleep(_INTER_BATCH_DELAY_SECONDS)

        logger.info(
            "[Nomic] Embedding complete: %d total embeddings", len(all_embeddings)
        )
        return all_embeddings

    def _request_with_retry(
        self,
        texts: list[str],
        batch_idx: int,
        total_batches: int,
        json_mod: object,
        request_cls: type,
        urlopen_fn: object,
    ) -> list[list[float]]:
        """Send an embedding request with exponential backoff on rate-limit errors."""
        import time
        from urllib.error import HTTPError

        backoff = _INITIAL_BACKOFF_SECONDS
        for attempt in range(_MAX_RETRIES):
            payload = json_mod.dumps({"inputs": texts}).encode("utf-8")
            req = request_cls(
                self._endpoint_url,
                data=payload,
                headers={
                    "Content-Type": "application/json",
                    "Authorization": f"Bearer {self._token}",
                },
            )
            try:
                t0 = time.monotonic()
                with urlopen_fn(req, timeout=120) as resp:
                    result = json_mod.loads(resp.read())
                elapsed = time.monotonic() - t0
                dim = len(result[0]) if result else 0
                logger.info(
                    "[Nomic] Batch %d/%d: received %d embeddings (dim=%d) in %.2fs",
                    batch_idx,
                    total_batches,
                    len(result),
                    dim,
                    elapsed,
                )
                return result
            except HTTPError as exc:
                if exc.code != 429:
                    raise
                logger.warning(
                    "[Nomic] Batch %d/%d: rate-limited (attempt %d/%d), "
                    "retrying in %.1fs...",
                    batch_idx,
                    total_batches,
                    attempt + 1,
                    _MAX_RETRIES,
                    backoff,
                )
                time.sleep(backoff)
                backoff *= 2

        raise RuntimeError(
            f"[Nomic] Batch {batch_idx}/{total_batches}: "
            f"failed after {_MAX_RETRIES} retries due to rate limiting"
        )


class GeminiEmbeddingClient:
    """Embedding client using Google's gemini-embedding-001 model."""

    _MODEL = "gemini-embedding-001"

    def __init__(self, api_key: str) -> None:
        from google import genai

        self._client = genai.Client(api_key=api_key)

    def embed_batch(self, texts: list[str]) -> list[list[float]]:
        """Embed a list of texts via the Gemini embedding API."""
        import time

        from google.genai import types

        total_texts = len(texts)
        total_batches = math.ceil(total_texts / _BATCH_SIZE)
        logger.info(
            "[Gemini] Embedding %d texts in %d batches (batch_size=%d, model=%s)",
            total_texts,
            total_batches,
            _BATCH_SIZE,
            self._MODEL,
        )

        all_embeddings: list[list[float]] = []
        for batch_idx, chunk in enumerate(batched(texts, _BATCH_SIZE), start=1):
            chunk_list = list(chunk)
            logger.info(
                "[Gemini] Batch %d/%d: sending %d texts",
                batch_idx,
                total_batches,
                len(chunk_list),
            )
            batch_embeddings = self._embed_with_retry(
                chunk_list, batch_idx, total_batches, types
            )
            all_embeddings.extend(batch_embeddings)
            if batch_idx < total_batches:
                time.sleep(_INTER_BATCH_DELAY_SECONDS)

        logger.info(
            "[Gemini] Embedding complete: %d total embeddings", len(all_embeddings)
        )
        return all_embeddings

    def _embed_with_retry(
        self,
        texts: list[str],
        batch_idx: int,
        total_batches: int,
        types: object,
    ) -> list[list[float]]:
        """Call embed_content with exponential backoff on rate-limit errors."""
        import time

        backoff = _INITIAL_BACKOFF_SECONDS
        for attempt in range(_MAX_RETRIES):
            try:
                t0 = time.monotonic()
                result = self._client.models.embed_content(
                    model=self._MODEL,
                    contents=texts,
                    config=types.EmbedContentConfig(output_dimensionality=768),
                )
                elapsed = time.monotonic() - t0
                batch_embeddings = [emb.values for emb in result.embeddings]
                dim = len(batch_embeddings[0]) if batch_embeddings else 0
                logger.info(
                    "[Gemini] Batch %d/%d: received %d embeddings (dim=%d) in %.2fs",
                    batch_idx,
                    total_batches,
                    len(batch_embeddings),
                    dim,
                    elapsed,
                )
                return batch_embeddings
            except Exception as exc:
                if "429" not in str(exc) and "RESOURCE_EXHAUSTED" not in str(exc):
                    raise
                logger.warning(
                    "[Gemini] Batch %d/%d: rate-limited (attempt %d/%d), "
                    "retrying in %.1fs... error: %s",
                    batch_idx,
                    total_batches,
                    attempt + 1,
                    _MAX_RETRIES,
                    backoff,
                    exc,
                )
                time.sleep(backoff)
                backoff *= 2

        raise RuntimeError(
            f"[Gemini] Batch {batch_idx}/{total_batches}: "
            f"failed after {_MAX_RETRIES} retries due to rate limiting"
        )


_HF_LOCAL_DEFAULT_MODEL = "Salesforce/codet5p-110m-embedding"


class HuggingFaceLocalEmbeddingClient:
    """Embedding client using a local HuggingFace transformers model.

    Loads the model and tokenizer once at construction time and runs
    inference locally — no API server or network calls required.
    Default model: ``Salesforce/codet5p-110m-embedding`` (256-dim, L2-normalised).
    """

    def __init__(
        self,
        model_name: str = _HF_LOCAL_DEFAULT_MODEL,
        device: str = "cpu",
        model: object = None,
        tokenizer: object = None,
    ) -> None:
        self._model_name = model_name
        self._device = device

        if model is not None and tokenizer is not None:
            self._model = model
            self._tokenizer = tokenizer
        else:
            from transformers import AutoModel, AutoTokenizer

            logger.info(
                "[HF-Local] Loading tokenizer and model: %s (device=%s)",
                model_name,
                device,
            )
            self._tokenizer = AutoTokenizer.from_pretrained(
                model_name, trust_remote_code=True
            )
            self._model = AutoModel.from_pretrained(model_name, trust_remote_code=True)
            import torch  # noqa: F811

            if device != "cpu" and torch.cuda.is_available():
                self._model = self._model.to(device)
            logger.info("[HF-Local] Model loaded successfully")

    def embed_batch(self, texts: list[str]) -> list[list[float]]:
        """Embed a list of texts using the local HuggingFace model."""
        import time

        total_texts = len(texts)
        total_batches = math.ceil(total_texts / _BATCH_SIZE) if total_texts else 0
        logger.info(
            "[HF-Local] Embedding %d texts in %d batches (batch_size=%d, model=%s)",
            total_texts,
            total_batches,
            _BATCH_SIZE,
            self._model_name,
        )

        all_embeddings: list[list[float]] = []
        for batch_idx, chunk in enumerate(batched(texts, _BATCH_SIZE), start=1):
            chunk_list = list(chunk)
            logger.info(
                "[HF-Local] Batch %d/%d: embedding %d texts",
                batch_idx,
                total_batches,
                len(chunk_list),
            )
            t0 = time.monotonic()
            batch_embeddings = [self._embed_single(text) for text in chunk_list]
            elapsed = time.monotonic() - t0
            dim = len(batch_embeddings[0]) if batch_embeddings else 0
            logger.info(
                "[HF-Local] Batch %d/%d: produced %d embeddings (dim=%d) in %.2fs",
                batch_idx,
                total_batches,
                len(batch_embeddings),
                dim,
                elapsed,
            )
            all_embeddings.extend(batch_embeddings)

        logger.info(
            "[HF-Local] Embedding complete: %d total embeddings", len(all_embeddings)
        )
        return all_embeddings

    def _embed_single(self, text: str) -> list[float]:
        """Tokenize and embed a single text string, truncating to model max length."""
        inputs = self._tokenizer.encode(
            text, return_tensors="pt", truncation=True, max_length=512
        )
        if hasattr(inputs, "to"):
            inputs = inputs.to(self._device)
        output = self._model(inputs)[0]
        return output.detach().cpu().tolist()


_CODERANK_DEFAULT_MODEL = "nomic-ai/CodeRankEmbed"
_CODERANK_QUERY_PREFIX = "Represent this query for searching relevant code: "
_BGE_DEFAULT_MODEL = "BAAI/bge-base-en-v1.5"


class SentenceTransformerEmbeddingClient:
    """Embedding client using sentence-transformers.

    Uses ``sentence_transformers.SentenceTransformer`` to load any compatible
    embedding model locally.  Works with both code-specific models like
    ``nomic-ai/CodeRankEmbed`` (768-dim, 8192-token, asymmetric) and
    general-purpose models like ``BAAI/bge-base-en-v1.5`` (768-dim).

    The ``query_prefix`` parameter supports asymmetric embedding models:
    natural-language queries can be prefixed (e.g. CodeRankEmbed requires
    ``"Represent this query for searching relevant code: "``), while code
    documents are embedded as-is (empty prefix).

    Set ``trust_remote_code=True`` for models that ship custom code
    (e.g. CodeRankEmbed).  Standard HuggingFace models (e.g. BGE) do not
    need this.
    """

    def __init__(
        self,
        model_name: str = _CODERANK_DEFAULT_MODEL,
        device: str = "cpu",
        query_prefix: str = "",
        trust_remote_code: bool = False,
        model: object = None,
    ) -> None:
        self._model_name = model_name
        self._device = device
        self._query_prefix = query_prefix

        if model is not None:
            self._model = model
        else:
            from sentence_transformers import SentenceTransformer

            logger.info(
                "[SentenceTransformer] Loading model: %s (device=%s, trust_remote_code=%s)",
                model_name,
                device,
                trust_remote_code,
            )
            self._model = SentenceTransformer(
                model_name, trust_remote_code=trust_remote_code, device=device
            )
            logger.info("[SentenceTransformer] Model loaded successfully")

    def embed_batch(self, texts: list[str]) -> list[list[float]]:
        """Embed a list of texts using the sentence-transformers model.

        Passes all texts in a single ``encode()`` call and lets
        sentence-transformers handle internal batching (default 32).
        """
        import time

        if not texts:
            return []

        prefixed_texts = (
            [f"{self._query_prefix}{t}" for t in texts] if self._query_prefix else texts
        )

        logger.info(
            "[ST] Embedding %d texts (model=%s)",
            len(prefixed_texts),
            self._model_name,
        )

        t0 = time.monotonic()
        result = self._model.encode(
            prefixed_texts, normalize_embeddings=True, show_progress_bar=True
        )
        embeddings = [vec.tolist() for vec in result]
        elapsed = time.monotonic() - t0
        dim = len(embeddings[0]) if embeddings else 0

        logger.info(
            "[ST] Embedding complete: %d embeddings (dim=%d) in %.2fs",
            len(embeddings),
            dim,
            elapsed,
        )
        return embeddings


def create_coderank_embedding_client(
    model_name: str = _CODERANK_DEFAULT_MODEL,
    device: str = "cpu",
    query_prefix: str = "",
    model: object = None,
) -> SentenceTransformerEmbeddingClient:
    """Create a SentenceTransformerEmbeddingClient configured for CodeRankEmbed."""
    return SentenceTransformerEmbeddingClient(
        model_name=model_name,
        device=device,
        query_prefix=query_prefix,
        trust_remote_code=True,
        model=model,
    )


def create_bge_embedding_client(
    model_name: str = _BGE_DEFAULT_MODEL,
    device: str = "cpu",
    model: object = None,
) -> SentenceTransformerEmbeddingClient:
    """Create a SentenceTransformerEmbeddingClient configured for BGE models."""
    return SentenceTransformerEmbeddingClient(
        model_name=model_name,
        device=device,
        query_prefix="",
        trust_remote_code=False,
        model=model,
    )


_OLLAMA_DEFAULT_MODEL = "unclemusclez/jina-embeddings-v2-base-code"
_OLLAMA_DEFAULT_BASE_URL = "http://localhost:11434"


class OllamaEmbeddingClient:
    """Embedding client using a local Ollama server.

    Calls the ``/api/embed`` endpoint which accepts multiple texts in a single
    request and returns their embeddings.  Runs entirely locally so there are
    no API quotas or rate limits to worry about.
    """

    def __init__(
        self,
        model: str = _OLLAMA_DEFAULT_MODEL,
        base_url: str = _OLLAMA_DEFAULT_BASE_URL,
        request_fn: object = None,
    ) -> None:
        self._model = model
        self._base_url = base_url.rstrip("/")
        self._request_fn = request_fn

    def embed_batch(self, texts: list[str]) -> list[list[float]]:
        """Embed a list of texts via the local Ollama /api/embed endpoint."""
        import json
        import time
        from urllib.request import Request, urlopen

        request_fn = self._request_fn if self._request_fn is not None else urlopen

        total_texts = len(texts)
        total_batches = math.ceil(total_texts / _BATCH_SIZE)
        logger.info(
            "[Ollama] Embedding %d texts in %d batches (batch_size=%d, model=%s)",
            total_texts,
            total_batches,
            _BATCH_SIZE,
            self._model,
        )

        all_embeddings: list[list[float]] = []
        for batch_idx, chunk in enumerate(batched(texts, _BATCH_SIZE), start=1):
            chunk_list = list(chunk)
            logger.info(
                "[Ollama] Batch %d/%d: sending %d texts",
                batch_idx,
                total_batches,
                len(chunk_list),
            )
            batch_embeddings = self._embed_with_retry(
                chunk_list, batch_idx, total_batches, json, Request, request_fn
            )
            all_embeddings.extend(batch_embeddings)

        logger.info(
            "[Ollama] Embedding complete: %d total embeddings", len(all_embeddings)
        )
        return all_embeddings

    def _embed_with_retry(
        self,
        texts: list[str],
        batch_idx: int,
        total_batches: int,
        json_mod: object,
        request_cls: type,
        urlopen_fn: object,
    ) -> list[list[float]]:
        """Call Ollama /api/embed with exponential backoff on connection errors."""
        import time
        from urllib.error import URLError

        url = f"{self._base_url}/api/embed"
        backoff = _INITIAL_BACKOFF_SECONDS
        for attempt in range(_MAX_RETRIES):
            payload = json_mod.dumps({"model": self._model, "input": texts}).encode(
                "utf-8"
            )
            req = request_cls(
                url, data=payload, headers={"Content-Type": "application/json"}
            )
            try:
                t0 = time.monotonic()
                with urlopen_fn(req, timeout=300) as resp:
                    result = json_mod.loads(resp.read())
                elapsed = time.monotonic() - t0
                embeddings = result["embeddings"]
                dim = len(embeddings[0]) if embeddings else 0
                logger.info(
                    "[Ollama] Batch %d/%d: received %d embeddings (dim=%d) in %.2fs",
                    batch_idx,
                    total_batches,
                    len(embeddings),
                    dim,
                    elapsed,
                )
                return embeddings
            except URLError as exc:
                logger.warning(
                    "[Ollama] Batch %d/%d: connection error (attempt %d/%d), "
                    "retrying in %.1fs... error: %s",
                    batch_idx,
                    total_batches,
                    attempt + 1,
                    _MAX_RETRIES,
                    backoff,
                    exc,
                )
                time.sleep(backoff)
                backoff *= 2

        raise RuntimeError(
            f"[Ollama] Batch {batch_idx}/{total_batches}: "
            f"failed after {_MAX_RETRIES} retries due to connection errors"
        )


class GenericIntegrationDescriptionEmbeddingConcretiser:
    """Classify integration signals using embedding similarity."""

    def __init__(
        self, client: EmbeddingClientProtocol, threshold: float = _CONFIDENCE_THRESHOLD
    ) -> None:
        self._client = client
        self._threshold = threshold
        self._desc_keys = list(_DIRECTIONAL_DESCRIPTIONS.keys())
        desc_texts = [_DIRECTIONAL_DESCRIPTIONS[k] for k in self._desc_keys]
        self._desc_embeddings = client.embed_batch(desc_texts)
        logger.info(
            "Pre-embedded %d directional descriptions", len(self._desc_embeddings)
        )

    def concretise(
        self,
        detector_result: IntegrationDetectorResult,
        file_reader: Callable[[str], bytes] = _read_file_bytes,
    ) -> tuple[ConcretisationResult, dict[tuple[str, int], dict]]:
        """Concretise FILE_CONTENT signals using embedding similarity.

        Args:
            detector_result: Output from the integration detector.
            file_reader: Callable that reads a file path to bytes.

        Returns:
            Tuple of (ConcretisationResult, metadata) where metadata maps
            (file_path, line_number) to best_type, best_direction, and score.
        """
        t0_all = time.monotonic()
        file_content_signals: list[SignalLike] = [
            s
            for s in detector_result.integration_points
            if s.entity_type == EntityType.FILE_CONTENT
        ]

        logger.info(
            "Embedding concretiser: %d FILE_CONTENT signals",
            len(file_content_signals),
        )

        signal_contexts = self._extract_contexts(file_content_signals, file_reader)
        line_texts = [sig.match.line_content.strip() for sig in file_content_signals]

        logger.info("Embedding %d signal line texts...", len(line_texts))
        signal_embeddings = self._client.embed_batch(line_texts) if line_texts else []

        concretised, metadata = self._classify_signals(
            file_content_signals, signal_contexts, signal_embeddings
        )

        classified = sum(1 for s in concretised if s.is_integration)
        result = ConcretisationResult(
            concretised=tuple(concretised),
            signals_submitted=len(concretised),
            signals_classified=classified,
            signals_unclassified=len(concretised) - classified,
        )
        elapsed_total = time.monotonic() - t0_all
        logger.info(
            "Embedding concretisation complete: submitted=%d classified=%d "
            "unclassified=%d elapsed=%.2fs",
            result.signals_submitted,
            result.signals_classified,
            result.signals_unclassified,
            elapsed_total,
        )
        return result, metadata

    def _extract_contexts(
        self,
        signals: list[SignalLike],
        file_reader: Callable[[str], bytes],
    ) -> list[ASTContext]:
        """Extract invocation-level AST context for each signal.

        Groups signals by file and language, then calls
        batch_extract_invocation_contexts once per (file, language) pair
        to avoid redundant parses and AST walks.
        """
        logger.info("Extracting AST contexts for %d signals...", len(signals))
        file_cache: dict[str, bytes] = {}
        groups: dict[tuple[str, Language], list[int]] = {}
        fallback_indices: list[int] = []

        for idx, sig in enumerate(signals):
            fp = sig.match.file_path
            if fp not in file_cache:
                try:
                    file_cache[fp] = file_reader(fp)
                    logger.debug("Cached file: %s", fp)
                except OSError as exc:
                    logger.error("Cannot read %s: %s", fp, exc)
                    file_cache[fp] = b""

            language = sig.match.language
            if language is None:
                fallback_indices.append(idx)
                continue

            key = (fp, language)
            groups.setdefault(key, []).append(idx)

        context_by_index: dict[int, ASTContext] = {
            idx: FALLBACK_AST_CONTEXT for idx in fallback_indices
        }
        for (fp, language), indices in groups.items():
            line_numbers = frozenset(signals[idx].match.line_number for idx in indices)
            contexts_by_line = batch_extract_invocation_contexts(
                file_cache[fp], language, line_numbers
            )
            for idx in indices:
                context_by_index[idx] = contexts_by_line[signals[idx].match.line_number]

        contexts = [context_by_index[i] for i in range(len(signals))]
        fallback_count = len(fallback_indices)
        logger.info(
            "AST context extraction complete: %d contexts from %d unique files (%d fallbacks)",
            len(contexts),
            len(file_cache),
            fallback_count,
        )
        return contexts

    def _classify_signals(
        self,
        signals: list[SignalLike],
        contexts: list[ASTContext],
        embeddings: list[list[float]],
    ) -> tuple[list[ConcretisedSignal], dict[tuple[str, int], dict]]:
        """Classify each signal using two-stage gate + direction logic."""
        logger.info(
            "Classifying %d signals against %d directional descriptions (threshold=%.3f)",
            len(signals),
            len(self._desc_keys),
            self._threshold,
        )
        concretised: list[ConcretisedSignal] = []
        metadata: dict[tuple[str, int], dict] = {}
        label_counts: dict[str, int] = {}
        total = len(signals)
        t0_classify = time.monotonic()
        progress_interval = max(1, total // 10)

        for idx, (sig, ctx, emb) in enumerate(zip(signals, contexts, embeddings)):
            scores = [cosine(emb, desc_emb) for desc_emb in self._desc_embeddings]
            best_idx = max(range(len(scores)), key=lambda i: scores[i])
            best_score = scores[best_idx]
            best_type, best_direction = self._desc_keys[best_idx]

            # Stage 1: gate — below threshold means NOISE
            if best_score < self._threshold:
                validity = SignalValidity.NOISE
                direction = SignalDirection.AMBIGUOUS
            else:
                # Stage 2: direction by score ratio
                validity = SignalValidity.SIGNAL
                direction = self._resolve_direction(scores)

            tag = f"{validity.value}:{direction.value}"
            label_counts[tag] = label_counts.get(tag, 0) + 1

            key = (sig.match.file_path, sig.match.line_number)
            metadata[key] = {
                "best_type": best_type,
                "best_direction": best_direction,
                "score": best_score,
            }

            logger.debug(
                "  [%d/%d] %s:%d  score=%.3f  type=%s  dir=%s  validity=%s  direction=%s",
                idx + 1,
                total,
                sig.match.file_path,
                sig.match.line_number,
                best_score,
                best_type,
                best_direction,
                validity.value,
                direction.value,
            )

            if (idx + 1) % progress_interval == 0 or idx + 1 == total:
                logger.info(
                    "Classification progress: %d/%d signals (%.2fs elapsed)",
                    idx + 1,
                    total,
                    time.monotonic() - t0_classify,
                )

            concretised.append(
                ConcretisedSignal(
                    original_signal=sig,
                    ast_context=ctx,
                    validity=validity,
                    direction=direction,
                )
            )

        logger.info(
            "Classification complete: %s",
            ", ".join(f"{k}={v}" for k, v in sorted(label_counts.items())),
        )
        return concretised, metadata

    def _resolve_direction(self, scores: list[float]) -> SignalDirection:
        """Determine direction from per-description scores using ratio threshold."""
        best_inward = max(
            (scores[i] for i, k in enumerate(self._desc_keys) if k[1] == "inward"),
            default=0.0,
        )
        best_outward = max(
            (scores[i] for i, k in enumerate(self._desc_keys) if k[1] == "outward"),
            default=0.0,
        )
        if best_inward == 0.0 and best_outward == 0.0:
            return SignalDirection.AMBIGUOUS
        if best_inward > best_outward * _AMBIGUITY_RATIO:
            return SignalDirection.INWARD
        if best_outward > best_inward * _AMBIGUITY_RATIO:
            return SignalDirection.OUTWARD
        return SignalDirection.AMBIGUOUS
