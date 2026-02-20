"""Embedding-based integration signal concretisation.

Uses nomic-embed-code embeddings to classify each FILE_CONTENT integration
signal as DEFINITE_INWARD, DEFINITE_OUTWARD, or NOT_DEFINITE by computing
cosine similarity against 26 directional description embeddings (13 integration
types x 2 directions).

This is a third concretisation path alongside the ML classifier and Ollama LLM.

Usage:
    from repo_surveyor.integration_concretiser.embedding_concretiser import (
        EmbeddingClient,
        EmbeddingConcretiser,
    )
    client = EmbeddingClient(endpoint_url=..., token=...)
    concretiser = EmbeddingConcretiser(client)
    result, metadata = concretiser.concretise(detector_result, file_reader)
"""

import logging
import math
from collections.abc import Callable
from itertools import batched

from repo_surveyor.detection.integration_detector import (
    EntityType,
    IntegrationDetectorResult,
    IntegrationSignal,
)
from repo_surveyor.integration_concretiser.ast_walker import (
    FALLBACK_AST_CONTEXT,
    extract_statement_context,
)
from repo_surveyor.integration_concretiser.types import (
    ASTContext,
    ConcretisationResult,
    ConcretisedSignal,
)
from repo_surveyor.training.types import TrainingLabel

logger = logging.getLogger(__name__)

_CONFIDENCE_THRESHOLD = 0.40

_BATCH_SIZE = 32

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

_DIRECTION_TO_LABEL: dict[str, TrainingLabel] = {
    "inward": TrainingLabel.DEFINITE_INWARD,
    "outward": TrainingLabel.DEFINITE_OUTWARD,
}


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


class EmbeddingClient:
    """Thin wrapper over a HuggingFace Inference Endpoint for embeddings."""

    def __init__(self, endpoint_url: str, token: str) -> None:
        self._endpoint_url = endpoint_url
        self._token = token

    def embed_batch(self, texts: list[str]) -> list[list[float]]:
        """Embed a list of texts, batching internally to avoid overflows."""
        import json
        from urllib.request import Request, urlopen

        all_embeddings: list[list[float]] = []
        for chunk in batched(texts, _BATCH_SIZE):
            chunk_list = list(chunk)
            payload = json.dumps({"inputs": chunk_list}).encode("utf-8")
            req = Request(
                self._endpoint_url,
                data=payload,
                headers={
                    "Content-Type": "application/json",
                    "Authorization": f"Bearer {self._token}",
                },
            )
            with urlopen(req, timeout=120) as resp:
                result = json.loads(resp.read())
            all_embeddings.extend(result)
        return all_embeddings


class EmbeddingConcretiser:
    """Classify integration signals using embedding similarity."""

    def __init__(
        self, client: EmbeddingClient, threshold: float = _CONFIDENCE_THRESHOLD
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
        file_content_signals = [
            s
            for s in detector_result.integration_points
            if s.entity_type == EntityType.FILE_CONTENT
        ]

        logger.info(
            "Embedding concretiser: %d FILE_CONTENT signals",
            len(file_content_signals),
        )

        signal_contexts = self._extract_contexts(file_content_signals, file_reader)
        statement_texts = [ctx.node_text for ctx in signal_contexts]

        logger.info("Embedding %d statement texts...", len(statement_texts))
        statement_embeddings = (
            self._client.embed_batch(statement_texts) if statement_texts else []
        )

        concretised, metadata = self._classify_signals(
            file_content_signals, signal_contexts, statement_embeddings
        )

        definite = sum(1 for s in concretised if s.is_definite)
        result = ConcretisationResult(
            concretised=tuple(concretised),
            signals_submitted=len(concretised),
            signals_definite=definite,
            signals_discarded=len(concretised) - definite,
        )
        logger.info(
            "Embedding concretisation complete: submitted=%d definite=%d discarded=%d",
            result.signals_submitted,
            result.signals_definite,
            result.signals_discarded,
        )
        return result, metadata

    def _extract_contexts(
        self,
        signals: list[IntegrationSignal],
        file_reader: Callable[[str], bytes],
    ) -> list[ASTContext]:
        """Extract statement-level AST context for each signal."""
        file_cache: dict[str, bytes] = {}
        contexts: list[ASTContext] = []
        for sig in signals:
            fp = sig.match.file_path
            if fp not in file_cache:
                try:
                    file_cache[fp] = file_reader(fp)
                except OSError as exc:
                    logger.error("Cannot read %s: %s", fp, exc)
                    file_cache[fp] = b""

            language = sig.match.language
            if language is None:
                contexts.append(FALLBACK_AST_CONTEXT)
                continue

            ctx = extract_statement_context(
                file_cache[fp], language, sig.match.line_number
            )
            contexts.append(ctx)
        return contexts

    def _classify_signals(
        self,
        signals: list[IntegrationSignal],
        contexts: list[ASTContext],
        embeddings: list[list[float]],
    ) -> tuple[list[ConcretisedSignal], dict[tuple[str, int], dict]]:
        """Classify each signal by argmax cosine similarity over descriptions."""
        concretised: list[ConcretisedSignal] = []
        metadata: dict[tuple[str, int], dict] = {}

        for sig, ctx, emb in zip(signals, contexts, embeddings):
            scores = [cosine(emb, desc_emb) for desc_emb in self._desc_embeddings]
            best_idx = max(range(len(scores)), key=lambda i: scores[i])
            best_score = scores[best_idx]
            best_type, best_direction = self._desc_keys[best_idx]

            if best_score < self._threshold:
                label = TrainingLabel.NOT_DEFINITE
            else:
                label = _DIRECTION_TO_LABEL[best_direction]

            key = (sig.match.file_path, sig.match.line_number)
            metadata[key] = {
                "best_type": best_type,
                "best_direction": best_direction,
                "score": best_score,
            }

            logger.debug(
                "  %s:%d  score=%.3f  type=%s  dir=%s  label=%s",
                sig.match.file_path,
                sig.match.line_number,
                best_score,
                best_type,
                best_direction,
                label.value,
            )

            concretised.append(
                ConcretisedSignal(
                    original_signal=sig,
                    ast_context=ctx,
                    label=label,
                )
            )

        return concretised, metadata
