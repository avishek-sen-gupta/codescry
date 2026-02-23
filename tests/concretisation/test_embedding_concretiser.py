"""Tests for embedding-based integration signal concretisation."""

import io
import json
import math
from urllib.error import URLError

import pytest

from repo_surveyor.integration_concretiser.ast_walker import (
    FALLBACK_AST_CONTEXT,
    extract_invocation_context,
)
from repo_surveyor.integration_concretiser.embedding_concretiser import (
    EmbeddingClient,
    EmbeddingConcretiser,
    OllamaEmbeddingClient,
    _BATCH_SIZE,
    _DIRECTIONAL_DESCRIPTIONS,
    _MAX_RETRIES,
    cosine,
)
from repo_surveyor.integration_concretiser.types import ASTContext, SignalValidity
from repo_surveyor.detection.integration_detector import (
    EntityType,
    FileMatch,
    IntegrationDetectorResult,
    IntegrationSignal,
)
from repo_surveyor.integration_patterns import (
    Confidence,
    IntegrationType,
    Language,
    SignalDirection,
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

_PYTHON_SOURCE = b"""\
import requests

class MyService:
    def fetch_data(self, url):
        response = requests.get(url)
        return response.json()

    def handle_request(self):
        pass

def standalone_function():
    db.execute("SELECT * FROM users")
"""

_JAVA_SOURCE = b"""\
package com.example;

import org.springframework.web.bind.annotation.*;

@RestController
public class UserController {

    @GetMapping("/users")
    public List<User> getUsers() {
        return userService.findAll();
    }
}
"""

_FAKE_FILES: dict[str, bytes] = {
    "client.py": _PYTHON_SOURCE,
    "UserController.java": _JAVA_SOURCE,
}


def _fake_reader(path: str) -> bytes:
    return _FAKE_FILES[path]


def _make_signal(
    file_path: str,
    line_number: int,
    line_content: str,
    language: Language,
    integration_type: IntegrationType = IntegrationType.HTTP_REST,
    entity_type: EntityType = EntityType.FILE_CONTENT,
) -> IntegrationSignal:
    return IntegrationSignal(
        match=FileMatch(
            file_path=file_path,
            line_number=line_number,
            line_content=line_content,
            language=language,
        ),
        integration_type=integration_type,
        confidence=Confidence.HIGH,
        matched_pattern="test_pattern",
        entity_type=entity_type,
        source="test",
    )


def _unit_vector(dim: int, index: int) -> list[float]:
    """Create a unit vector with 1.0 at the given index, 0.0 elsewhere."""
    vec = [0.0] * dim
    vec[index] = 1.0
    return vec


def _scaled_vector(base: list[float], scale: float) -> list[float]:
    """Return base vector scaled by a factor."""
    return [x * scale for x in base]


class _MockEmbeddingClient(EmbeddingClient):
    """Mock client that returns pre-configured embeddings."""

    def __init__(self, embeddings_by_call: list[list[list[float]]]) -> None:
        self._call_queue = list(embeddings_by_call)
        self.call_log: list[list[str]] = []

    def embed_batch(self, texts: list[str]) -> list[list[float]]:
        self.call_log.append(texts)
        if not self._call_queue:
            return [[0.0] * 10] * len(texts)
        return self._call_queue.pop(0)


# ---------------------------------------------------------------------------
# Tests: extract_invocation_context (nearest invocation walk-up)
# ---------------------------------------------------------------------------


class TestExtractInvocationContext:
    """Verify extract_invocation_context returns the nearest invocation."""

    def test_python_method_call_returns_call(self):
        ctx = extract_invocation_context(_PYTHON_SOURCE, Language.PYTHON, 5)
        assert ctx.node_type == "call"
        assert "requests.get" in ctx.node_text
        assert ctx.start_line <= 5
        assert ctx.end_line >= 5

    def test_python_standalone_call_returns_call(self):
        ctx = extract_invocation_context(_PYTHON_SOURCE, Language.PYTHON, 12)
        assert ctx.node_type == "call"
        assert "db.execute" in ctx.node_text

    def test_unsupported_language_returns_fallback(self):
        ctx = extract_invocation_context(b"some content", Language.PLI, 1)
        assert ctx == FALLBACK_AST_CONTEXT

    def test_empty_file_returns_fallback(self):
        ctx = extract_invocation_context(b"", Language.PYTHON, 1)
        assert ctx == FALLBACK_AST_CONTEXT

    def test_import_line_falls_back_to_statement(self):
        ctx = extract_invocation_context(_PYTHON_SOURCE, Language.PYTHON, 1)
        assert "import" in ctx.node_type
        assert "requests" in ctx.node_text

    def test_java_method_call_returns_invocation(self):
        ctx = extract_invocation_context(_JAVA_SOURCE, Language.JAVA, 10)
        assert ctx.node_type == "method_invocation"
        assert "findAll" in ctx.node_text
        assert ctx.start_line <= 10
        assert ctx.end_line >= 10

    def test_java_chained_builder_returns_innermost_invocation(self):
        javalin_source = b"""\
public class App {
    void run() {
        Javalin.create(config -> {
            config.jsonMapper(mapper);
        }).get("/api/users", ctx -> {
            ctx.json(users);
        }).start(8080);
    }
}
"""
        ctx = extract_invocation_context(javalin_source, Language.JAVA, 4)
        assert ctx.node_type == "method_invocation"
        assert "jsonMapper" in ctx.node_text


# ---------------------------------------------------------------------------
# Tests: cosine function
# ---------------------------------------------------------------------------


class TestCosine:
    def test_identical_vectors(self):
        assert cosine([1.0, 0.0, 0.0], [1.0, 0.0, 0.0]) == pytest.approx(1.0)

    def test_orthogonal_vectors(self):
        assert cosine([1.0, 0.0], [0.0, 1.0]) == pytest.approx(0.0)

    def test_opposite_vectors(self):
        assert cosine([1.0, 0.0], [-1.0, 0.0]) == pytest.approx(-1.0)

    def test_zero_vector_returns_zero(self):
        assert cosine([0.0, 0.0], [1.0, 1.0]) == pytest.approx(0.0)


# ---------------------------------------------------------------------------
# Tests: EmbeddingConcretiser threshold logic
# ---------------------------------------------------------------------------


class TestThresholdLogic:
    """Signals below threshold become NOISE."""

    def test_below_threshold_is_noise(self):
        dim = 26
        desc_embeddings = [_unit_vector(dim, i) for i in range(26)]
        low_score_vec = [0.01] * dim
        signal_embeddings = [low_score_vec]

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, signal_embeddings]
        )
        concretiser = EmbeddingConcretiser(mock_client, threshold=0.40)

        signal = _make_signal("client.py", 5, "requests.get(url)", Language.PYTHON)
        detector_result = IntegrationDetectorResult(
            integration_points=[signal], files_scanned=1
        )

        result, metadata = concretiser.concretise(detector_result, _fake_reader)

        assert result.concretised[0].validity == SignalValidity.NOISE
        assert result.concretised[0].direction == SignalDirection.AMBIGUOUS
        key = ("client.py", 5)
        assert metadata[key]["score"] < 0.40

    def test_above_threshold_is_signal(self):
        dim = 26
        desc_embeddings = [_unit_vector(dim, i) for i in range(26)]
        high_score_vec = _unit_vector(dim, 0)
        signal_embeddings = [high_score_vec]

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, signal_embeddings]
        )
        concretiser = EmbeddingConcretiser(mock_client, threshold=0.40)

        signal = _make_signal("client.py", 5, "requests.get(url)", Language.PYTHON)
        detector_result = IntegrationDetectorResult(
            integration_points=[signal], files_scanned=1
        )

        result, _ = concretiser.concretise(detector_result, _fake_reader)

        assert result.concretised[0].validity == SignalValidity.SIGNAL


# ---------------------------------------------------------------------------
# Tests: Direction classification
# ---------------------------------------------------------------------------


class TestDirectionClassification:
    """Verify correct INWARD/OUTWARD direction from score ratio."""

    def test_inward_when_best_match_is_inward(self):
        desc_keys = list(_DIRECTIONAL_DESCRIPTIONS.keys())
        inward_idx = next(i for i, k in enumerate(desc_keys) if k[1] == "inward")
        dim = 26
        desc_embeddings = [_unit_vector(dim, i) for i in range(26)]
        signal_vec = _unit_vector(dim, inward_idx)

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, [signal_vec]]
        )
        concretiser = EmbeddingConcretiser(mock_client, threshold=0.40)

        signal = _make_signal("client.py", 5, "requests.get(url)", Language.PYTHON)
        detector_result = IntegrationDetectorResult(
            integration_points=[signal], files_scanned=1
        )

        result, metadata = concretiser.concretise(detector_result, _fake_reader)

        assert result.concretised[0].validity == SignalValidity.SIGNAL
        assert result.concretised[0].direction == SignalDirection.INWARD
        assert metadata[("client.py", 5)]["best_direction"] == "inward"

    def test_outward_when_best_match_is_outward(self):
        desc_keys = list(_DIRECTIONAL_DESCRIPTIONS.keys())
        outward_idx = next(i for i, k in enumerate(desc_keys) if k[1] == "outward")
        dim = 26
        desc_embeddings = [_unit_vector(dim, i) for i in range(26)]
        signal_vec = _unit_vector(dim, outward_idx)

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, [signal_vec]]
        )
        concretiser = EmbeddingConcretiser(mock_client, threshold=0.40)

        signal = _make_signal("client.py", 5, "requests.get(url)", Language.PYTHON)
        detector_result = IntegrationDetectorResult(
            integration_points=[signal], files_scanned=1
        )

        result, metadata = concretiser.concretise(detector_result, _fake_reader)

        assert result.concretised[0].validity == SignalValidity.SIGNAL
        assert result.concretised[0].direction == SignalDirection.OUTWARD
        assert metadata[("client.py", 5)]["best_direction"] == "outward"


# ---------------------------------------------------------------------------
# Tests: Batching
# ---------------------------------------------------------------------------


class TestBatching:
    """Verify signals are sent as a single batch to embed_batch."""

    def test_multiple_signals_batched_together(self):
        dim = 26
        desc_embeddings = [_unit_vector(dim, i) for i in range(26)]
        signal_vecs = [_unit_vector(dim, 0), _unit_vector(dim, 1)]

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, signal_vecs]
        )
        concretiser = EmbeddingConcretiser(mock_client, threshold=0.40)

        signals = [
            _make_signal("client.py", 5, "requests.get(url)", Language.PYTHON),
            _make_signal("client.py", 12, 'db.execute("SELECT")', Language.PYTHON),
        ]
        detector_result = IntegrationDetectorResult(
            integration_points=signals, files_scanned=1
        )

        result, _ = concretiser.concretise(detector_result, _fake_reader)

        assert result.signals_submitted == 2
        assert len(mock_client.call_log) == 2
        assert len(mock_client.call_log[1]) == 2


# ---------------------------------------------------------------------------
# Tests: Metadata
# ---------------------------------------------------------------------------


class TestMetadata:
    """Verify metadata maps (file_path, line_number) correctly."""

    def test_metadata_keys_match_signals(self):
        dim = 26
        desc_embeddings = [_unit_vector(dim, i) for i in range(26)]
        signal_vecs = [_unit_vector(dim, 0)]

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, signal_vecs]
        )
        concretiser = EmbeddingConcretiser(mock_client, threshold=0.40)

        signal = _make_signal("client.py", 5, "requests.get(url)", Language.PYTHON)
        detector_result = IntegrationDetectorResult(
            integration_points=[signal], files_scanned=1
        )

        _, metadata = concretiser.concretise(detector_result, _fake_reader)

        assert ("client.py", 5) in metadata
        meta = metadata[("client.py", 5)]
        assert "best_type" in meta
        assert "best_direction" in meta
        assert "score" in meta
        assert isinstance(meta["score"], float)


# ---------------------------------------------------------------------------
# Tests: Fallback for unsupported language
# ---------------------------------------------------------------------------


class TestFallbackLanguage:
    """Unsupported languages still produce ConcretisedSignal with fallback context."""

    def test_pli_produces_fallback_ast_context(self):
        dim = 26
        desc_embeddings = [_unit_vector(dim, i) for i in range(26)]
        low_vec = [0.01] * dim

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, [low_vec]]
        )
        concretiser = EmbeddingConcretiser(mock_client, threshold=0.40)

        signal = _make_signal("program.pli", 1, "CALL PLITDLI", Language.PLI)
        _FAKE_FILES["program.pli"] = b"CALL PLITDLI;\n"

        detector_result = IntegrationDetectorResult(
            integration_points=[signal], files_scanned=1
        )

        result, _ = concretiser.concretise(detector_result, _fake_reader)

        assert len(result.concretised) == 1
        assert result.concretised[0].ast_context == FALLBACK_AST_CONTEXT

        del _FAKE_FILES["program.pli"]


# ---------------------------------------------------------------------------
# Tests: Filters out DIRECTORY signals
# ---------------------------------------------------------------------------


class TestFilterDirectorySignals:
    """Directory signals are excluded from concretisation."""

    def test_directory_signals_excluded(self):
        dim = 26
        desc_embeddings = [_unit_vector(dim, i) for i in range(26)]
        signal_vecs = [_unit_vector(dim, 0)]

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, signal_vecs]
        )
        concretiser = EmbeddingConcretiser(mock_client, threshold=0.40)

        file_signal = _make_signal("client.py", 5, "requests.get(url)", Language.PYTHON)
        dir_signal = _make_signal(
            "client.py",
            5,
            "requests.get(url)",
            Language.PYTHON,
            entity_type=EntityType.DIRECTORY,
        )
        detector_result = IntegrationDetectorResult(
            integration_points=[file_signal, dir_signal], files_scanned=1
        )

        result, _ = concretiser.concretise(detector_result, _fake_reader)

        assert result.signals_submitted == 1


# ---------------------------------------------------------------------------
# Tests: End-to-end with mock client
# ---------------------------------------------------------------------------


class TestEndToEnd:
    """Full pipeline with mock client and known vectors."""

    def test_full_pipeline_produces_expected_classifications(self):
        desc_keys = list(_DIRECTIONAL_DESCRIPTIONS.keys())
        dim = len(desc_keys)
        desc_embeddings = [_unit_vector(dim, i) for i in range(dim)]

        outward_http_idx = next(
            i for i, k in enumerate(desc_keys) if k == ("http_rest", "outward")
        )
        inward_http_idx = next(
            i for i, k in enumerate(desc_keys) if k == ("http_rest", "inward")
        )

        signal_vecs = [
            _unit_vector(dim, outward_http_idx),
            _unit_vector(dim, inward_http_idx),
            [0.01] * dim,
        ]

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, signal_vecs]
        )
        concretiser = EmbeddingConcretiser(mock_client, threshold=0.40)

        signals = [
            _make_signal("client.py", 5, "requests.get(url)", Language.PYTHON),
            _make_signal(
                "UserController.java", 10, "userService.findAll()", Language.JAVA
            ),
            _make_signal("client.py", 12, 'db.execute("SELECT")', Language.PYTHON),
        ]
        detector_result = IntegrationDetectorResult(
            integration_points=signals, files_scanned=2
        )

        result, metadata = concretiser.concretise(detector_result, _fake_reader)

        assert result.signals_submitted == 3
        assert result.signals_classified == 2
        assert result.signals_unclassified == 1

        validities = [s.validity for s in result.concretised]
        directions = [s.direction for s in result.concretised]
        assert validities[0] == SignalValidity.SIGNAL
        assert directions[0] == SignalDirection.OUTWARD
        assert validities[1] == SignalValidity.SIGNAL
        assert directions[1] == SignalDirection.INWARD
        assert validities[2] == SignalValidity.NOISE
        assert directions[2] == SignalDirection.AMBIGUOUS

        assert metadata[("client.py", 5)]["best_type"] == "http_rest"
        assert metadata[("client.py", 5)]["best_direction"] == "outward"
        assert metadata[("UserController.java", 10)]["best_type"] == "http_rest"
        assert metadata[("UserController.java", 10)]["best_direction"] == "inward"


# ---------------------------------------------------------------------------
# Helpers for OllamaEmbeddingClient tests
# ---------------------------------------------------------------------------


class _FakeOllamaResponse:
    """Fake HTTP response that acts as a context manager and returns JSON."""

    def __init__(self, embeddings: list[list[float]]) -> None:
        self._data = json.dumps({"embeddings": embeddings}).encode("utf-8")

    def read(self) -> bytes:
        return self._data

    def __enter__(self):
        return self

    def __exit__(self, *args):
        pass


class _FakeOllamaServer:
    """Injectable fake that records calls and returns pre-configured embeddings."""

    def __init__(self, embeddings_per_call: list[list[list[float]]]) -> None:
        self._queue = list(embeddings_per_call)
        self.requests: list[dict] = []

    def __call__(self, req, timeout=300):
        body = json.loads(req.data.decode("utf-8"))
        self.requests.append(body)
        if not self._queue:
            return _FakeOllamaResponse([[0.0] * 10] * len(body["input"]))
        return _FakeOllamaResponse(self._queue.pop(0))


class _FailThenSucceedServer:
    """Raises URLError for the first N calls, then succeeds."""

    def __init__(self, fail_count: int, embeddings: list[list[float]]) -> None:
        self._fail_count = fail_count
        self._embeddings = embeddings
        self.call_count = 0

    def __call__(self, req, timeout=300):
        self.call_count += 1
        if self.call_count <= self._fail_count:
            raise URLError("Connection refused")
        return _FakeOllamaResponse(self._embeddings)


# ---------------------------------------------------------------------------
# Tests: OllamaEmbeddingClient
# ---------------------------------------------------------------------------


class TestOllamaEmbedBatch:
    """Verify OllamaEmbeddingClient.embed_batch returns correct embeddings."""

    def test_single_batch_returns_embeddings(self):
        expected = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]
        server = _FakeOllamaServer(embeddings_per_call=[expected])
        client = OllamaEmbeddingClient(request_fn=server)

        result = client.embed_batch(["hello", "world"])

        assert result == expected
        assert len(server.requests) == 1
        assert server.requests[0]["model"] == "jina/jina-embeddings-v2-base-code"
        assert server.requests[0]["input"] == ["hello", "world"]

    def test_custom_model_and_url(self):
        server = _FakeOllamaServer(embeddings_per_call=[[[1.0]]])
        client = OllamaEmbeddingClient(
            model="custom-model", base_url="http://myhost:9999", request_fn=server
        )

        client.embed_batch(["test"])

        assert server.requests[0]["model"] == "custom-model"

    def test_empty_input_returns_empty(self):
        server = _FakeOllamaServer(embeddings_per_call=[])
        client = OllamaEmbeddingClient(request_fn=server)

        result = client.embed_batch([])

        assert result == []
        assert len(server.requests) == 0


class TestOllamaBatching:
    """Verify batching splits texts into chunks of _BATCH_SIZE."""

    def test_texts_split_into_correct_batches(self):
        num_texts = _BATCH_SIZE + 10
        batch1_embeddings = [[float(i)] for i in range(_BATCH_SIZE)]
        batch2_embeddings = [[float(i)] for i in range(10)]
        server = _FakeOllamaServer(
            embeddings_per_call=[batch1_embeddings, batch2_embeddings]
        )
        client = OllamaEmbeddingClient(request_fn=server)

        texts = [f"text_{i}" for i in range(num_texts)]
        result = client.embed_batch(texts)

        assert len(result) == num_texts
        assert len(server.requests) == 2
        assert len(server.requests[0]["input"]) == _BATCH_SIZE
        assert len(server.requests[1]["input"]) == 10


class TestOllamaRetry:
    """Verify retry with backoff on connection errors."""

    def test_retries_on_connection_error_then_succeeds(self):
        expected = [[1.0, 2.0]]
        server = _FailThenSucceedServer(fail_count=2, embeddings=expected)
        client = OllamaEmbeddingClient(request_fn=server)

        result = client.embed_batch(["test"])

        assert result == expected
        assert server.call_count == 3

    def test_raises_after_max_retries(self):
        server = _FailThenSucceedServer(fail_count=_MAX_RETRIES + 1, embeddings=[[1.0]])
        client = OllamaEmbeddingClient(request_fn=server)

        with pytest.raises(RuntimeError, match="failed after .* retries"):
            client.embed_batch(["test"])
