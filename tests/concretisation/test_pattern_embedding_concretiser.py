"""Tests for pattern-description embedding concretisation."""

import json
from pathlib import Path

import pytest

from repo_surveyor.integration_concretiser.embedding_concretiser import cosine
from repo_surveyor.integration_concretiser.pattern_embedding_concretiser import (
    PatternEmbeddingConcretiser,
    _compute_content_hash,
    _load_cached_embeddings,
    _resolve_direction_by_vote,
    _save_cache,
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
# Module-level cached descriptions — loaded once across all tests.
# ---------------------------------------------------------------------------

_CACHED_DESCRIPTIONS = get_all_pattern_descriptions()

# Embedding dimension for mock vectors.  Kept small (50) to avoid the
# O(num_descs × dim) pure-Python cosine cost that made tests ~45 s each
# when dim == num_descs (~3 430).  Unit-vector indices wrap around via
# modulo, which is fine: tests only need the *nearest* match to work, not
# all 3 430 vectors to be mutually orthogonal.
_MOCK_DIM = 50

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
        descriptions = _CACHED_DESCRIPTIONS
        assert len(descriptions) > 0

    def test_entries_have_valid_metadata(self):
        descriptions = _CACHED_DESCRIPTIONS
        for desc in descriptions:
            assert isinstance(desc, PatternDescription)
            assert isinstance(desc.text, str)
            assert len(desc.text) > 0
            assert isinstance(desc.integration_type, IntegrationType)
            assert isinstance(desc.direction, SignalDirection)
            assert isinstance(desc.source, str)
            assert len(desc.source) > 0

    def test_contains_common_descriptions(self):
        descriptions = _CACHED_DESCRIPTIONS
        sources = {d.source for d in descriptions}
        assert "common" in sources

    def test_contains_framework_descriptions(self):
        descriptions = _CACHED_DESCRIPTIONS
        sources = {d.source for d in descriptions}
        # At least some framework sources should be present
        assert len(sources) > 1

    def test_descriptions_cover_multiple_integration_types(self):
        descriptions = _CACHED_DESCRIPTIONS
        types = {d.integration_type for d in descriptions}
        assert len(types) >= 5

    def test_descriptions_cover_multiple_directions(self):
        descriptions = _CACHED_DESCRIPTIONS
        directions = {d.direction for d in descriptions}
        assert SignalDirection.INWARD in directions
        assert SignalDirection.OUTWARD in directions


# ---------------------------------------------------------------------------
# Tests: Nearest-neighbor classification
# ---------------------------------------------------------------------------


class TestNearestNeighborClassification:
    """Verify nearest-neighbor classification logic."""

    def test_below_threshold_is_noise(self):
        descriptions = _CACHED_DESCRIPTIONS
        num_descs = len(descriptions)
        dim = _MOCK_DIM

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
        descriptions = _CACHED_DESCRIPTIONS
        num_descs = len(descriptions)
        dim = _MOCK_DIM

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
        descriptions = _CACHED_DESCRIPTIONS
        num_descs = len(descriptions)
        dim = _MOCK_DIM

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
        # With KNN, direction comes from weighted vote across K neighbours,
        # so the voted direction may differ from the top-1 nearest.
        # Verify the top-1 metadata still reflects the OUTWARD description.
        assert metadata[("client.py", 5)]["nearest_direction"] == "outward"


# ---------------------------------------------------------------------------
# Tests: End-to-end with mock client
# ---------------------------------------------------------------------------


class TestEndToEnd:
    """Full pipeline with mock client and multiple signals."""

    def test_multiple_signals_classified_correctly(self):
        descriptions = _CACHED_DESCRIPTIONS
        num_descs = len(descriptions)
        dim = _MOCK_DIM

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
        descriptions = _CACHED_DESCRIPTIONS
        num_descs = len(descriptions)
        dim = _MOCK_DIM

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
        descriptions = _CACHED_DESCRIPTIONS
        num_descs = len(descriptions)
        dim = _MOCK_DIM

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


# ---------------------------------------------------------------------------
# Tests: Embedding cache
# ---------------------------------------------------------------------------


class TestEmbeddingCache:
    """Verify cache save/load round-trip and invalidation."""

    def test_cache_round_trip(self, tmp_path: Path):
        descriptions = _CACHED_DESCRIPTIONS
        num_descs = len(descriptions)
        dim = 10
        embeddings = [[float(i + j) for j in range(dim)] for i in range(num_descs)]

        cache_file = tmp_path / "cache.json"
        _save_cache(cache_file, descriptions, embeddings, backend="test")

        loaded = _load_cached_embeddings(cache_file, descriptions)
        assert len(loaded) == num_descs
        assert loaded[0] == embeddings[0]
        assert loaded[-1] == embeddings[-1]

    def test_cache_invalidation_on_hash_mismatch(self, tmp_path: Path):
        descriptions = _CACHED_DESCRIPTIONS
        num_descs = len(descriptions)
        dim = 10
        embeddings = [[float(i)] * dim for i in range(num_descs)]

        cache_file = tmp_path / "cache.json"
        _save_cache(cache_file, descriptions, embeddings, backend="test")

        # Tamper with the content hash
        data = json.loads(cache_file.read_text(encoding="utf-8"))
        data["content_hash"] = "bogus_hash"
        cache_file.write_text(json.dumps(data), encoding="utf-8")

        loaded = _load_cached_embeddings(cache_file, descriptions)
        assert loaded == []

    def test_cache_miss_falls_through_to_api(self, tmp_path: Path):
        descriptions = _CACHED_DESCRIPTIONS
        num_descs = len(descriptions)
        dim = 10

        desc_embeddings = [[float(i)] * dim for i in range(num_descs)]
        signal_embeddings = [[0.5] * dim]

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, signal_embeddings]
        )

        cache_file = tmp_path / "nonexistent_cache.json"
        concretiser = PatternEmbeddingConcretiser(
            mock_client, threshold=0.40, cache_path=cache_file
        )

        # Client should have been called to embed descriptions
        assert len(mock_client.call_log) == 1
        assert len(mock_client.call_log[0]) == num_descs

        # Cache file should now exist
        assert cache_file.exists()

    def test_cache_hit_skips_api_call(self, tmp_path: Path):
        descriptions = _CACHED_DESCRIPTIONS
        num_descs = len(descriptions)
        dim = 10
        embeddings = [[float(i)] * dim for i in range(num_descs)]

        cache_file = tmp_path / "cache.json"
        _save_cache(cache_file, descriptions, embeddings, backend="test")

        mock_client = _MockEmbeddingClient(embeddings_by_call=[])
        concretiser = PatternEmbeddingConcretiser(
            mock_client, threshold=0.40, cache_path=cache_file
        )

        # Client should NOT have been called for descriptions
        assert len(mock_client.call_log) == 0

    def test_content_hash_deterministic(self):
        descriptions = _CACHED_DESCRIPTIONS
        hash1 = _compute_content_hash(descriptions)
        hash2 = _compute_content_hash(descriptions)
        assert hash1 == hash2
        assert len(hash1) == 64  # SHA-256 hex digest

    def test_cache_file_missing_returns_empty(self, tmp_path: Path):
        descriptions = _CACHED_DESCRIPTIONS
        missing = tmp_path / "does_not_exist.json"
        loaded = _load_cached_embeddings(missing, descriptions)
        assert loaded == []


# ---------------------------------------------------------------------------
# Tests: KNN classification
# ---------------------------------------------------------------------------


def _make_desc(
    text: str,
    direction: SignalDirection,
    integration_type: IntegrationType = IntegrationType.HTTP_REST,
    source: str = "test",
) -> PatternDescription:
    return PatternDescription(
        text=text,
        integration_type=integration_type,
        direction=direction,
        source=source,
    )


class TestResolveDirectionByVote:
    """Verify the pure _resolve_direction_by_vote function."""

    def test_weighted_vote_picks_higher_total(self):
        neighbours = [
            (_make_desc("inward1", SignalDirection.INWARD), 0.90),
            (_make_desc("inward2", SignalDirection.INWARD), 0.85),
            (_make_desc("inward3", SignalDirection.INWARD), 0.80),
            (_make_desc("outward1", SignalDirection.OUTWARD), 0.60),
            (_make_desc("outward2", SignalDirection.OUTWARD), 0.55),
        ]
        # INWARD sum = 2.55, OUTWARD sum = 1.15
        assert _resolve_direction_by_vote(neighbours) == SignalDirection.INWARD

    def test_weighted_vote_outward_wins(self):
        neighbours = [
            (_make_desc("outward1", SignalDirection.OUTWARD), 0.95),
            (_make_desc("outward2", SignalDirection.OUTWARD), 0.90),
            (_make_desc("outward3", SignalDirection.OUTWARD), 0.85),
            (_make_desc("inward1", SignalDirection.INWARD), 0.40),
            (_make_desc("inward2", SignalDirection.INWARD), 0.35),
        ]
        assert _resolve_direction_by_vote(neighbours) == SignalDirection.OUTWARD

    def test_tie_breaks_to_highest_score_neighbour(self):
        # Equal weighted sums: INWARD=0.80, OUTWARD=0.80
        neighbours = [
            (_make_desc("inward1", SignalDirection.INWARD), 0.80),
            (_make_desc("outward1", SignalDirection.OUTWARD), 0.80),
        ]
        # Highest score is shared — first in sorted order (stable) should
        # be the first neighbour, which is INWARD (both have 0.80, but
        # the first one in the list is checked first)
        result = _resolve_direction_by_vote(neighbours)
        # With a true tie, the highest-scoring neighbour (index 0) wins
        assert result == SignalDirection.INWARD

    def test_tie_breaks_to_highest_single_score(self):
        # INWARD sum = 0.70, OUTWARD sum = 0.70
        # But the single highest score belongs to OUTWARD
        neighbours = [
            (_make_desc("outward1", SignalDirection.OUTWARD), 0.70),
            (_make_desc("inward1", SignalDirection.INWARD), 0.35),
            (_make_desc("inward2", SignalDirection.INWARD), 0.35),
        ]
        assert _resolve_direction_by_vote(neighbours) == SignalDirection.OUTWARD


class TestKNNClassification:
    """Verify KNN classification end-to-end through the concretiser."""

    def test_knn_direction_weighted_vote(self):
        """5 neighbours: 3 INWARD (high) + 2 OUTWARD (low) → INWARD wins."""
        descriptions = _CACHED_DESCRIPTIONS
        num_descs = len(descriptions)
        dim = _MOCK_DIM

        # Find inward descriptions
        inward_idx = next(
            i
            for i, d in enumerate(descriptions)
            if d.direction == SignalDirection.INWARD
        )

        desc_embeddings = [_unit_vector(dim, i % dim) for i in range(num_descs)]
        # Signal embedding matches the inward description
        signal_embeddings = [_unit_vector(dim, inward_idx % dim)]

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, signal_embeddings]
        )
        concretiser = PatternEmbeddingConcretiser(mock_client, threshold=0.40, k=5)

        signal = _make_signal("client.py", 5, "requests.get(url)", Language.PYTHON)
        detector_result = IntegrationDetectorResult(
            integration_points=[signal], files_scanned=1
        )
        result, metadata = concretiser.concretise(detector_result, _fake_reader)

        assert result.concretised[0].validity == SignalValidity.SIGNAL
        meta = metadata[("client.py", 5)]
        assert "neighbours" in meta
        assert "avg_score" in meta
        assert "direction_votes" in meta
        assert len(meta["neighbours"]) <= 5

    def test_knn_validity_uses_average_score(self):
        """Average of K scores below threshold → NOISE, even if top-1 is above."""
        descriptions = _CACHED_DESCRIPTIONS
        num_descs = len(descriptions)
        dim = _MOCK_DIM

        desc_embeddings = [_unit_vector(dim, i % dim) for i in range(num_descs)]
        # Create a signal that barely matches anything — low average across K
        low_score_vec = [0.01] * dim
        signal_embeddings = [low_score_vec]

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, signal_embeddings]
        )
        concretiser = PatternEmbeddingConcretiser(mock_client, threshold=0.40, k=5)

        signal = _make_signal("client.py", 5, "requests.get(url)", Language.PYTHON)
        detector_result = IntegrationDetectorResult(
            integration_points=[signal], files_scanned=1
        )
        result, metadata = concretiser.concretise(detector_result, _fake_reader)

        assert result.concretised[0].validity == SignalValidity.NOISE
        meta = metadata[("client.py", 5)]
        assert meta["avg_score"] < 0.40

    def test_knn_validity_above_threshold(self):
        """Average of K scores above threshold → SIGNAL."""
        descriptions = _CACHED_DESCRIPTIONS
        num_descs = len(descriptions)
        dim = _MOCK_DIM

        desc_embeddings = [_unit_vector(dim, i % dim) for i in range(num_descs)]
        signal_embeddings = [_unit_vector(dim, 0)]

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, signal_embeddings]
        )
        concretiser = PatternEmbeddingConcretiser(mock_client, threshold=0.40, k=5)

        signal = _make_signal("client.py", 5, "requests.get(url)", Language.PYTHON)
        detector_result = IntegrationDetectorResult(
            integration_points=[signal], files_scanned=1
        )
        result, metadata = concretiser.concretise(detector_result, _fake_reader)

        assert result.concretised[0].validity == SignalValidity.SIGNAL
        meta = metadata[("client.py", 5)]
        assert meta["avg_score"] >= 0.40

    def test_knn_metadata_contains_k_neighbours(self):
        """Metadata should include all K neighbours with descriptions and scores."""
        descriptions = _CACHED_DESCRIPTIONS
        num_descs = len(descriptions)
        dim = _MOCK_DIM

        desc_embeddings = [_unit_vector(dim, i % dim) for i in range(num_descs)]
        signal_embeddings = [_unit_vector(dim, 0)]

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, signal_embeddings]
        )
        concretiser = PatternEmbeddingConcretiser(mock_client, threshold=0.40, k=5)

        signal = _make_signal("client.py", 5, "requests.get(url)", Language.PYTHON)
        detector_result = IntegrationDetectorResult(
            integration_points=[signal], files_scanned=1
        )
        _, metadata = concretiser.concretise(detector_result, _fake_reader)

        meta = metadata[("client.py", 5)]
        assert "neighbours" in meta
        assert len(meta["neighbours"]) == 5
        for neighbour in meta["neighbours"]:
            assert "description" in neighbour
            assert "type" in neighbour
            assert "direction" in neighbour
            assert "source" in neighbour
            assert "score" in neighbour

        # Backward compat: top-1 aliases still present
        assert "nearest_description" in meta
        assert "nearest_type" in meta
        assert "nearest_direction" in meta
        assert "nearest_source" in meta
        assert "score" in meta
        assert meta["k"] == 5

    def test_knn_k_exceeds_descriptions(self):
        """When K > number of descriptions, gracefully use all available."""
        # Use a tiny custom description set via a small mock
        dim = 10
        # We'll create a concretiser with k=100 but only ~num_descs descriptions
        descriptions = _CACHED_DESCRIPTIONS
        num_descs = len(descriptions)
        k = num_descs + 100  # way more than available

        desc_embeddings = [_unit_vector(dim, i % dim) for i in range(num_descs)]
        signal_embeddings = [_unit_vector(dim, 0)]

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, signal_embeddings]
        )
        concretiser = PatternEmbeddingConcretiser(mock_client, threshold=0.01, k=k)

        signal = _make_signal("client.py", 5, "requests.get(url)", Language.PYTHON)
        detector_result = IntegrationDetectorResult(
            integration_points=[signal], files_scanned=1
        )
        _, metadata = concretiser.concretise(detector_result, _fake_reader)

        meta = metadata[("client.py", 5)]
        # Should use all available descriptions, not crash
        assert len(meta["neighbours"]) == num_descs

    def test_knn_defaults_to_k5(self):
        """Default constructor uses K=5."""
        descriptions = _CACHED_DESCRIPTIONS
        num_descs = len(descriptions)
        dim = _MOCK_DIM

        desc_embeddings = [_unit_vector(dim, i % dim) for i in range(num_descs)]
        signal_embeddings = [_unit_vector(dim, 0)]

        mock_client = _MockEmbeddingClient(
            embeddings_by_call=[desc_embeddings, signal_embeddings]
        )
        concretiser = PatternEmbeddingConcretiser(mock_client, threshold=0.40)

        signal = _make_signal("client.py", 5, "requests.get(url)", Language.PYTHON)
        detector_result = IntegrationDetectorResult(
            integration_points=[signal], files_scanned=1
        )
        _, metadata = concretiser.concretise(detector_result, _fake_reader)

        meta = metadata[("client.py", 5)]
        assert meta["k"] == 5
        assert len(meta["neighbours"]) == 5
