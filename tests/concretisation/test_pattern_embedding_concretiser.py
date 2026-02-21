"""Tests for pattern-description embedding concretisation."""

import pytest

from repo_surveyor.integration_concretiser.embedding_concretiser import cosine
from repo_surveyor.integration_concretiser.pattern_embedding_concretiser import (
    PatternEmbeddingConcretiser,
)
from repo_surveyor.integration_concretiser.types import SignalValidity
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
    PatternDescription,
    SignalDirection,
    get_all_pattern_descriptions,
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
"""

_FAKE_FILES: dict[str, bytes] = {
    "client.py": _PYTHON_SOURCE,
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


class _MockEmbeddingClient:
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
# Tests: PatternDescription registry
# ---------------------------------------------------------------------------


class TestPatternDescriptionRegistry:
    """Verify get_all_pattern_descriptions() returns valid entries."""

    def test_returns_non_empty(self):
        descriptions = get_all_pattern_descriptions()
        assert len(descriptions) > 0

    def test_entries_have_valid_metadata(self):
        descriptions = get_all_pattern_descriptions()
        for desc in descriptions:
            assert isinstance(desc, PatternDescription)
            assert isinstance(desc.text, str)
            assert len(desc.text) > 0
            assert isinstance(desc.integration_type, IntegrationType)
            assert isinstance(desc.direction, SignalDirection)
            assert isinstance(desc.source, str)
            assert len(desc.source) > 0

    def test_contains_common_descriptions(self):
        descriptions = get_all_pattern_descriptions()
        sources = {d.source for d in descriptions}
        assert "common" in sources

    def test_contains_framework_descriptions(self):
        descriptions = get_all_pattern_descriptions()
        sources = {d.source for d in descriptions}
        # At least some framework sources should be present
        assert len(sources) > 1

    def test_descriptions_cover_multiple_integration_types(self):
        descriptions = get_all_pattern_descriptions()
        types = {d.integration_type for d in descriptions}
        assert len(types) >= 5

    def test_descriptions_cover_multiple_directions(self):
        descriptions = get_all_pattern_descriptions()
        directions = {d.direction for d in descriptions}
        assert SignalDirection.INWARD in directions
        assert SignalDirection.OUTWARD in directions


# ---------------------------------------------------------------------------
# Tests: Nearest-neighbor classification
# ---------------------------------------------------------------------------


class TestNearestNeighborClassification:
    """Verify nearest-neighbor classification logic."""

    def test_below_threshold_is_noise(self):
        descriptions = get_all_pattern_descriptions()
        num_descs = len(descriptions)
        dim = max(num_descs, 10)

        desc_embeddings = [_unit_vector(dim, i % dim) for i in range(num_descs)]
        low_score_vec = [0.01] * dim
        signal_embeddings = [low_score_vec]

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, signal_embeddings]
        )
        concretiser = PatternEmbeddingConcretiser(mock_client, threshold=0.40)

        signal = _make_signal("client.py", 5, "requests.get(url)", Language.PYTHON)
        detector_result = IntegrationDetectorResult(
            integration_points=[signal], files_scanned=1
        )

        result, metadata = concretiser.concretise(detector_result, _fake_reader)

        assert result.concretised[0].validity == SignalValidity.NOISE
        assert result.concretised[0].direction == SignalDirection.AMBIGUOUS
        assert metadata[("client.py", 5)]["score"] < 0.40

    def test_above_threshold_returns_nearest_metadata(self):
        descriptions = get_all_pattern_descriptions()
        num_descs = len(descriptions)
        dim = max(num_descs, 10)

        desc_embeddings = [_unit_vector(dim, i % dim) for i in range(num_descs)]
        # Match exactly the first description
        signal_embeddings = [_unit_vector(dim, 0)]

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, signal_embeddings]
        )
        concretiser = PatternEmbeddingConcretiser(mock_client, threshold=0.40)

        signal = _make_signal("client.py", 5, "requests.get(url)", Language.PYTHON)
        detector_result = IntegrationDetectorResult(
            integration_points=[signal], files_scanned=1
        )

        result, metadata = concretiser.concretise(detector_result, _fake_reader)

        assert result.concretised[0].validity == SignalValidity.SIGNAL
        meta = metadata[("client.py", 5)]
        assert "nearest_description" in meta
        assert "nearest_type" in meta
        assert "nearest_direction" in meta
        assert "nearest_source" in meta
        assert "score" in meta
        assert meta["score"] >= 0.40

    def test_direction_comes_from_nearest_description(self):
        descriptions = get_all_pattern_descriptions()
        num_descs = len(descriptions)
        dim = max(num_descs, 10)

        # Find an OUTWARD description
        outward_idx = next(
            i
            for i, d in enumerate(descriptions)
            if d.direction == SignalDirection.OUTWARD
        )

        desc_embeddings = [_unit_vector(dim, i % dim) for i in range(num_descs)]
        signal_embeddings = [_unit_vector(dim, outward_idx % dim)]

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, signal_embeddings]
        )
        concretiser = PatternEmbeddingConcretiser(mock_client, threshold=0.40)

        signal = _make_signal("client.py", 5, "requests.get(url)", Language.PYTHON)
        detector_result = IntegrationDetectorResult(
            integration_points=[signal], files_scanned=1
        )

        result, metadata = concretiser.concretise(detector_result, _fake_reader)

        assert result.concretised[0].validity == SignalValidity.SIGNAL
        assert result.concretised[0].direction == SignalDirection.OUTWARD
        assert metadata[("client.py", 5)]["nearest_direction"] == "outward"


# ---------------------------------------------------------------------------
# Tests: End-to-end with mock client
# ---------------------------------------------------------------------------


class TestEndToEnd:
    """Full pipeline with mock client and multiple signals."""

    def test_multiple_signals_classified_correctly(self):
        descriptions = get_all_pattern_descriptions()
        num_descs = len(descriptions)
        dim = max(num_descs, 10)

        desc_embeddings = [_unit_vector(dim, i % dim) for i in range(num_descs)]

        # Find inward and outward description indices
        inward_idx = next(
            i
            for i, d in enumerate(descriptions)
            if d.direction == SignalDirection.INWARD
        )
        outward_idx = next(
            i
            for i, d in enumerate(descriptions)
            if d.direction == SignalDirection.OUTWARD
        )

        signal_vecs = [
            _unit_vector(dim, outward_idx % dim),  # should be SIGNAL+OUTWARD
            _unit_vector(dim, inward_idx % dim),  # should be SIGNAL+INWARD
            [0.01] * dim,  # should be NOISE
        ]

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, signal_vecs]
        )
        concretiser = PatternEmbeddingConcretiser(mock_client, threshold=0.40)

        signals = [
            _make_signal("client.py", 5, "requests.get(url)", Language.PYTHON),
            _make_signal("client.py", 1, "import requests", Language.PYTHON),
            _make_signal("client.py", 3, "class MyService:", Language.PYTHON),
        ]
        detector_result = IntegrationDetectorResult(
            integration_points=signals, files_scanned=1
        )

        result, metadata = concretiser.concretise(detector_result, _fake_reader)

        assert result.signals_submitted == 3
        assert result.signals_classified == 2
        assert result.signals_unclassified == 1

        validities = [s.validity for s in result.concretised]
        assert validities[0] == SignalValidity.SIGNAL
        assert validities[1] == SignalValidity.SIGNAL
        assert validities[2] == SignalValidity.NOISE

    def test_directory_signals_excluded(self):
        descriptions = get_all_pattern_descriptions()
        num_descs = len(descriptions)
        dim = max(num_descs, 10)

        desc_embeddings = [_unit_vector(dim, i % dim) for i in range(num_descs)]
        signal_vecs = [_unit_vector(dim, 0)]

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, signal_vecs]
        )
        concretiser = PatternEmbeddingConcretiser(mock_client, threshold=0.40)

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

    def test_metadata_contains_expected_fields(self):
        descriptions = get_all_pattern_descriptions()
        num_descs = len(descriptions)
        dim = max(num_descs, 10)

        desc_embeddings = [_unit_vector(dim, i % dim) for i in range(num_descs)]
        signal_vecs = [_unit_vector(dim, 0)]

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, signal_vecs]
        )
        concretiser = PatternEmbeddingConcretiser(mock_client, threshold=0.40)

        signal = _make_signal("client.py", 5, "requests.get(url)", Language.PYTHON)
        detector_result = IntegrationDetectorResult(
            integration_points=[signal], files_scanned=1
        )

        _, metadata = concretiser.concretise(detector_result, _fake_reader)

        meta = metadata[("client.py", 5)]
        assert "nearest_description" in meta
        assert "nearest_type" in meta
        assert "nearest_direction" in meta
        assert "nearest_source" in meta
        assert "score" in meta
        assert isinstance(meta["nearest_description"], str)
        assert isinstance(meta["score"], float)
