"""Tests for the hybrid pipeline merge logic (embedding gate + Gemini direction)."""

import pytest

from repo_surveyor.detection.integration_detector import (
    EntityType,
    FileMatch,
    IntegrationDetectorResult,
    IntegrationSignal,
)
from repo_surveyor.integration_concretiser.types import (
    ASTContext,
    ConcretisationResult,
    ConcretisedSignal,
    SignalValidity,
)
from repo_surveyor.integration_patterns import (
    Confidence,
    IntegrationType,
    Language,
    SignalDirection,
)

# Import is done from the script via sys.path manipulation in the script,
# but for tests we import the merge function directly.
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent.parent / "scripts"))
from survey_repo_hybrid import merge_results

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

_DEFAULT_AST = ASTContext(
    node_type="call",
    node_text="some.call()",
    start_line=1,
    end_line=1,
)


def _make_signal(
    file_path: str = "Foo.java",
    line_number: int = 10,
    line_content: str = "httpClient.get(url)",
) -> IntegrationSignal:
    return IntegrationSignal(
        match=FileMatch(
            file_path=file_path,
            line_number=line_number,
            line_content=line_content,
            language=Language.JAVA,
        ),
        integration_type=IntegrationType.HTTP_REST,
        confidence=Confidence.HIGH,
        matched_pattern="test_pattern",
        entity_type=EntityType.FILE_CONTENT,
        source="test",
    )


def _make_concretised(
    signal: IntegrationSignal,
    validity: SignalValidity,
    direction: SignalDirection,
) -> ConcretisedSignal:
    return ConcretisedSignal(
        original_signal=signal,
        ast_context=_DEFAULT_AST,
        validity=validity,
        direction=direction,
    )


def _make_result(
    concretised: list[ConcretisedSignal],
) -> ConcretisationResult:
    classified = sum(1 for s in concretised if s.is_integration)
    return ConcretisationResult(
        concretised=tuple(concretised),
        signals_submitted=len(concretised),
        signals_classified=classified,
        signals_unclassified=len(concretised) - classified,
    )


# ---------------------------------------------------------------------------
# Tests: NOISE signals pass through unchanged
# ---------------------------------------------------------------------------


class TestNoisePassthrough:
    """NOISE signals from embedding should be unchanged in merged output."""

    def test_noise_signal_preserved(self):
        sig = _make_signal("Noise.java", 5, "// comment")
        emb_cs = _make_concretised(sig, SignalValidity.NOISE, SignalDirection.AMBIGUOUS)
        embedding_result = _make_result([emb_cs])
        gemini_result = _make_result([])

        merged, _ = merge_results(embedding_result, gemini_result, {}, {})

        assert len(merged.concretised) == 1
        assert merged.concretised[0].validity == SignalValidity.NOISE
        assert merged.concretised[0].direction == SignalDirection.AMBIGUOUS

    def test_noise_not_sent_to_gemini_lookup(self):
        sig = _make_signal("Noise.java", 5)
        emb_cs = _make_concretised(sig, SignalValidity.NOISE, SignalDirection.AMBIGUOUS)
        # Even if Gemini somehow has a result for this key, NOISE is preserved
        gemini_cs = _make_concretised(
            sig, SignalValidity.SIGNAL, SignalDirection.INWARD
        )
        embedding_result = _make_result([emb_cs])
        gemini_result = _make_result([gemini_cs])

        merged, _ = merge_results(embedding_result, gemini_result, {}, {})

        assert merged.concretised[0].validity == SignalValidity.NOISE
        assert merged.concretised[0].direction == SignalDirection.AMBIGUOUS


# ---------------------------------------------------------------------------
# Tests: SIGNAL signals get Gemini's direction
# ---------------------------------------------------------------------------


class TestSignalGetsGeminiDirection:
    """SIGNAL signals should adopt Gemini's direction classification."""

    def test_signal_gets_inward_from_gemini(self):
        sig = _make_signal("Api.java", 20)
        emb_cs = _make_concretised(
            sig, SignalValidity.SIGNAL, SignalDirection.AMBIGUOUS
        )
        gemini_cs = _make_concretised(
            sig, SignalValidity.SIGNAL, SignalDirection.INWARD
        )

        merged, _ = merge_results(
            _make_result([emb_cs]), _make_result([gemini_cs]), {}, {}
        )

        assert merged.concretised[0].validity == SignalValidity.SIGNAL
        assert merged.concretised[0].direction == SignalDirection.INWARD

    def test_signal_gets_outward_from_gemini(self):
        sig = _make_signal("Client.java", 30)
        emb_cs = _make_concretised(
            sig, SignalValidity.SIGNAL, SignalDirection.AMBIGUOUS
        )
        gemini_cs = _make_concretised(
            sig, SignalValidity.SIGNAL, SignalDirection.OUTWARD
        )

        merged, _ = merge_results(
            _make_result([emb_cs]), _make_result([gemini_cs]), {}, {}
        )

        assert merged.concretised[0].validity == SignalValidity.SIGNAL
        assert merged.concretised[0].direction == SignalDirection.OUTWARD


# ---------------------------------------------------------------------------
# Tests: SIGNAL missing from Gemini gets AMBIGUOUS direction
# ---------------------------------------------------------------------------


class TestSignalMissingFromGemini:
    """SIGNALs not present in Gemini response should get AMBIGUOUS direction."""

    def test_missing_signal_gets_ambiguous(self):
        sig = _make_signal("Missing.java", 15)
        emb_cs = _make_concretised(
            sig, SignalValidity.SIGNAL, SignalDirection.AMBIGUOUS
        )

        merged, _ = merge_results(_make_result([emb_cs]), _make_result([]), {}, {})

        assert merged.concretised[0].validity == SignalValidity.SIGNAL
        assert merged.concretised[0].direction == SignalDirection.AMBIGUOUS


# ---------------------------------------------------------------------------
# Tests: SIGNAL where Gemini says NOT_DEFINITE (validity=NOISE)
# ---------------------------------------------------------------------------


class TestGeminiNotDefinite:
    """When Gemini marks a signal as NOT_DEFINITE (NOISE), keep SIGNAL validity
    but set direction to AMBIGUOUS."""

    def test_gemini_noise_keeps_signal_validity(self):
        sig = _make_signal("Ambig.java", 42)
        emb_cs = _make_concretised(
            sig, SignalValidity.SIGNAL, SignalDirection.AMBIGUOUS
        )
        gemini_cs = _make_concretised(
            sig, SignalValidity.NOISE, SignalDirection.AMBIGUOUS
        )

        merged, _ = merge_results(
            _make_result([emb_cs]), _make_result([gemini_cs]), {}, {}
        )

        assert merged.concretised[0].validity == SignalValidity.SIGNAL
        assert merged.concretised[0].direction == SignalDirection.AMBIGUOUS


# ---------------------------------------------------------------------------
# Tests: Metadata merging
# ---------------------------------------------------------------------------


class TestMetadataMerging:
    """Merged metadata should contain entries from both stages."""

    def test_both_metadata_sources_present(self):
        sig1 = _make_signal("A.java", 10)
        sig2 = _make_signal("B.java", 20)

        emb_cs1 = _make_concretised(
            sig1, SignalValidity.SIGNAL, SignalDirection.AMBIGUOUS
        )
        emb_cs2 = _make_concretised(
            sig2, SignalValidity.NOISE, SignalDirection.AMBIGUOUS
        )
        gemini_cs1 = _make_concretised(
            sig1, SignalValidity.SIGNAL, SignalDirection.INWARD
        )

        embedding_metadata = {
            ("A.java", 10): {"best_type": "http_rest", "score": 0.72},
            ("B.java", 20): {"best_type": "database", "score": 0.35},
        }
        gemini_metadata = {
            ("A.java", 10): {"confidence": 0.95, "reason": "REST endpoint"},
        }

        _, merged_meta = merge_results(
            _make_result([emb_cs1, emb_cs2]),
            _make_result([gemini_cs1]),
            embedding_metadata,
            gemini_metadata,
        )

        # Gemini metadata overwrites embedding for shared keys
        assert merged_meta[("A.java", 10)]["confidence"] == 0.95
        assert merged_meta[("A.java", 10)]["reason"] == "REST endpoint"
        # Embedding-only key preserved
        assert merged_meta[("B.java", 20)]["best_type"] == "database"

    def test_gemini_metadata_overwrites_embedding_for_same_key(self):
        sig = _make_signal("X.java", 5)
        emb_cs = _make_concretised(
            sig, SignalValidity.SIGNAL, SignalDirection.AMBIGUOUS
        )
        gemini_cs = _make_concretised(
            sig, SignalValidity.SIGNAL, SignalDirection.OUTWARD
        )

        embedding_metadata = {("X.java", 5): {"score": 0.8}}
        gemini_metadata = {("X.java", 5): {"confidence": 0.9, "reason": "outbound"}}

        _, merged_meta = merge_results(
            _make_result([emb_cs]),
            _make_result([gemini_cs]),
            embedding_metadata,
            gemini_metadata,
        )

        # Gemini overwrites
        assert merged_meta[("X.java", 5)] == {
            "confidence": 0.9,
            "reason": "outbound",
        }


# ---------------------------------------------------------------------------
# Tests: Counts in merged result
# ---------------------------------------------------------------------------


class TestMergedCounts:
    """Verify submitted/classified/unclassified counts in merged result."""

    def test_counts_reflect_embedding_validity(self):
        sig1 = _make_signal("A.java", 10)
        sig2 = _make_signal("B.java", 20)
        sig3 = _make_signal("C.java", 30)

        emb_signals = [
            _make_concretised(sig1, SignalValidity.SIGNAL, SignalDirection.AMBIGUOUS),
            _make_concretised(sig2, SignalValidity.NOISE, SignalDirection.AMBIGUOUS),
            _make_concretised(sig3, SignalValidity.SIGNAL, SignalDirection.AMBIGUOUS),
        ]
        gemini_signals = [
            _make_concretised(sig1, SignalValidity.SIGNAL, SignalDirection.INWARD),
            _make_concretised(sig3, SignalValidity.SIGNAL, SignalDirection.OUTWARD),
        ]

        merged, _ = merge_results(
            _make_result(emb_signals), _make_result(gemini_signals), {}, {}
        )

        assert merged.signals_submitted == 3
        assert merged.signals_classified == 2
        assert merged.signals_unclassified == 1


# ---------------------------------------------------------------------------
# Tests: End-to-end with mixed signals
# ---------------------------------------------------------------------------


class TestEndToEndMixed:
    """Full merge with a mix of NOISE, SIGNAL+direction, SIGNAL+missing."""

    def test_mixed_scenario(self):
        noise_sig = _make_signal("Noise.java", 1)
        inward_sig = _make_signal("Inward.java", 2)
        outward_sig = _make_signal("Outward.java", 3)
        missing_sig = _make_signal("Missing.java", 4)
        not_definite_sig = _make_signal("NotDef.java", 5)

        emb_signals = [
            _make_concretised(
                noise_sig, SignalValidity.NOISE, SignalDirection.AMBIGUOUS
            ),
            _make_concretised(
                inward_sig, SignalValidity.SIGNAL, SignalDirection.AMBIGUOUS
            ),
            _make_concretised(
                outward_sig, SignalValidity.SIGNAL, SignalDirection.AMBIGUOUS
            ),
            _make_concretised(
                missing_sig, SignalValidity.SIGNAL, SignalDirection.AMBIGUOUS
            ),
            _make_concretised(
                not_definite_sig, SignalValidity.SIGNAL, SignalDirection.AMBIGUOUS
            ),
        ]
        gemini_signals = [
            _make_concretised(
                inward_sig, SignalValidity.SIGNAL, SignalDirection.INWARD
            ),
            _make_concretised(
                outward_sig, SignalValidity.SIGNAL, SignalDirection.OUTWARD
            ),
            # missing_sig is not in Gemini results
            _make_concretised(
                not_definite_sig, SignalValidity.NOISE, SignalDirection.AMBIGUOUS
            ),
        ]

        merged, _ = merge_results(
            _make_result(emb_signals), _make_result(gemini_signals), {}, {}
        )

        results = {
            cs.original_signal.match.file_path: (cs.validity, cs.direction)
            for cs in merged.concretised
        }

        assert results["Noise.java"] == (
            SignalValidity.NOISE,
            SignalDirection.AMBIGUOUS,
        )
        assert results["Inward.java"] == (
            SignalValidity.SIGNAL,
            SignalDirection.INWARD,
        )
        assert results["Outward.java"] == (
            SignalValidity.SIGNAL,
            SignalDirection.OUTWARD,
        )
        assert results["Missing.java"] == (
            SignalValidity.SIGNAL,
            SignalDirection.AMBIGUOUS,
        )
        assert results["NotDef.java"] == (
            SignalValidity.SIGNAL,
            SignalDirection.AMBIGUOUS,
        )

        assert merged.signals_submitted == 5
        assert merged.signals_classified == 4
        assert merged.signals_unclassified == 1
