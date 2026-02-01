"""Tests for integration point detection."""

import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from repo_surveyor import CTagsEntry, CTagsResult, detect_integrations
from repo_surveyor.integration_detector import (
    IntegrationDetectorResult,
    IntegrationPoint,
    classify_entry,
    determine_confidence,
    first_matching_pattern,
)


def make_entry(
    name: str = "test",
    path: str = "Test.java",
    kind: str = "method",
    line: int = 1,
    scope: str | None = None,
    scope_kind: str | None = None,
    signature: str | None = None,
    language: str = "Java",
) -> CTagsEntry:
    """Helper to create a CTagsEntry for testing."""
    return CTagsEntry(
        name=name,
        path=path,
        kind=kind,
        line=line,
        scope=scope,
        scope_kind=scope_kind,
        signature=signature,
        language=language,
    )


def make_ctags_result(entries: list[CTagsEntry]) -> CTagsResult:
    """Helper to create a CTagsResult for testing."""
    return CTagsResult(
        entries=entries,
        raw_output="",
        return_code=0,
        error_output="",
    )


class TestFirstMatchingPattern:
    """Tests for the first_matching_pattern function."""

    def test_returns_matched_pattern(self) -> None:
        """Should return the matched pattern."""
        patterns = (r"(?i)http", r"(?i)rest")
        result = first_matching_pattern("HttpClient", patterns)
        assert result == r"(?i)http"

    def test_case_insensitive(self) -> None:
        """Should match case-insensitively when pattern specifies."""
        patterns = (r"(?i)controller",)
        assert first_matching_pattern("UserController", patterns) is not None
        assert first_matching_pattern("CONTROLLER", patterns) is not None
        assert first_matching_pattern("controller", patterns) is not None

    def test_returns_none_for_no_match(self) -> None:
        """Should return None when no pattern matches."""
        patterns = (r"(?i)http", r"(?i)rest")
        result = first_matching_pattern("DatabaseHandler", patterns)
        assert result is None

    def test_handles_none_input(self) -> None:
        """Should return None for None input."""
        patterns = (r"(?i)http",)
        result = first_matching_pattern(None, patterns)
        assert result is None

    def test_empty_patterns_tuple(self) -> None:
        """Should return None for empty patterns tuple."""
        result = first_matching_pattern("HttpClient", ())
        assert result is None


class TestDetermineConfidence:
    """Tests for confidence level determination."""

    def test_high_confidence_for_annotation_patterns(self) -> None:
        """Should return high confidence for annotation patterns."""
        assert (
            determine_confidence("http_rest", "@RequestMapping", False, True, False)
            == "high"
        )
        assert (
            determine_confidence("database", "@Repository", False, True, False)
            == "high"
        )

    def test_high_confidence_for_signature_match(self) -> None:
        """Should return high confidence for signature matches."""
        assert (
            determine_confidence("http_rest", "SomePattern", False, True, False)
            == "high"
        )

    def test_medium_confidence_for_scope_match(self) -> None:
        """Should return medium confidence for scope matches."""
        assert (
            determine_confidence("http_rest", "(?i)controller", False, False, True)
            == "medium"
        )

    def test_medium_confidence_for_strong_name_keywords(self) -> None:
        """Should return medium confidence for strong keyword names."""
        assert (
            determine_confidence("http_rest", "(?i)controller", True, False, False)
            == "medium"
        )
        assert (
            determine_confidence("database", "(?i)repository", True, False, False)
            == "medium"
        )

    def test_low_confidence_for_generic_name_match(self) -> None:
        """Should return low confidence for generic name matches."""
        assert (
            determine_confidence("http_rest", "(?i)http", True, False, False) == "low"
        )


class TestClassifyEntry:
    """Tests for entry classification."""

    def test_classify_http_rest_by_name(self) -> None:
        """Should classify HTTP/REST entries by name."""
        entry = make_entry(name="HttpClient")
        matches = classify_entry(entry)

        assert len(matches) >= 1
        types = [m[0] for m in matches]
        assert "http_rest" in types

    def test_classify_http_rest_by_signature(self) -> None:
        """Should classify HTTP/REST entries by signature."""
        entry = make_entry(name="getUsers", signature='@GetMapping("/users")')
        matches = classify_entry(entry)

        assert len(matches) >= 1
        http_matches = [m for m in matches if m[0] == "http_rest"]
        assert len(http_matches) >= 1
        assert http_matches[0][1] == "high"  # High confidence for annotation

    def test_classify_http_rest_by_scope(self) -> None:
        """Should classify HTTP/REST entries by scope."""
        entry = make_entry(name="processRequest", scope="UserController")
        matches = classify_entry(entry)

        assert len(matches) >= 1
        http_matches = [m for m in matches if m[0] == "http_rest"]
        assert len(http_matches) >= 1

    def test_classify_database_by_name(self) -> None:
        """Should classify database entries by name."""
        entry = make_entry(name="UserRepository")
        matches = classify_entry(entry)

        assert len(matches) >= 1
        types = [m[0] for m in matches]
        assert "database" in types

    def test_classify_database_by_signature(self) -> None:
        """Should classify database entries by signature."""
        entry = make_entry(name="findAll", signature='@Query("SELECT * FROM users")')
        matches = classify_entry(entry)

        assert len(matches) >= 1
        db_matches = [m for m in matches if m[0] == "database"]
        assert len(db_matches) >= 1
        assert db_matches[0][1] == "high"

    def test_classify_messaging_by_name(self) -> None:
        """Should classify messaging entries by name."""
        entry = make_entry(name="KafkaProducer")
        matches = classify_entry(entry)

        assert len(matches) >= 1
        types = [m[0] for m in matches]
        assert "messaging" in types

    def test_classify_messaging_by_signature(self) -> None:
        """Should classify messaging entries by signature."""
        entry = make_entry(
            name="handleMessage", signature='@KafkaListener(topics = "orders")'
        )
        matches = classify_entry(entry)

        assert len(matches) >= 1
        msg_matches = [m for m in matches if m[0] == "messaging"]
        assert len(msg_matches) >= 1
        # Should have a high confidence match from the signature
        high_confidence_matches = [m for m in msg_matches if m[1] == "high"]
        assert len(high_confidence_matches) >= 1

    def test_classify_socket_by_name(self) -> None:
        """Should classify socket entries by name."""
        entry = make_entry(name="WebSocketHandler")
        matches = classify_entry(entry)

        assert len(matches) >= 1
        types = [m[0] for m in matches]
        assert "socket" in types

    def test_classify_socket_by_signature(self) -> None:
        """Should classify socket entries by signature."""
        entry = make_entry(name="onMessage", signature="@OnMessage")
        matches = classify_entry(entry)

        assert len(matches) >= 1
        socket_matches = [m for m in matches if m[0] == "socket"]
        assert len(socket_matches) >= 1
        assert socket_matches[0][1] == "high"

    def test_classify_soap_by_name(self) -> None:
        """Should classify SOAP entries by name."""
        entry = make_entry(name="SoapServiceImpl")
        matches = classify_entry(entry)

        assert len(matches) >= 1
        types = [m[0] for m in matches]
        assert "soap" in types

    def test_classify_soap_by_signature(self) -> None:
        """Should classify SOAP entries by signature."""
        entry = make_entry(name="getCustomer", signature="@WebMethod")
        matches = classify_entry(entry)

        assert len(matches) >= 1
        soap_matches = [m for m in matches if m[0] == "soap"]
        assert len(soap_matches) >= 1
        assert soap_matches[0][1] == "high"

    def test_classify_multiple_types(self) -> None:
        """Should detect multiple integration types for ambiguous names."""
        # "MessageHandler" could match messaging
        entry = make_entry(name="MessageHandler")
        matches = classify_entry(entry)

        assert len(matches) >= 1
        types = [m[0] for m in matches]
        assert "messaging" in types

    def test_classify_no_match(self) -> None:
        """Should return empty list for non-integration entries."""
        entry = make_entry(name="StringUtils")
        matches = classify_entry(entry)

        assert len(matches) == 0

    def test_matched_pattern_includes_match_type(self) -> None:
        """Should include match type prefix in matched_pattern."""
        entry = make_entry(name="HttpClient")
        matches = classify_entry(entry)

        assert len(matches) >= 1
        assert matches[0][2].startswith("name:")

        entry = make_entry(name="foo", signature="@GetMapping")
        matches = classify_entry(entry)
        http_matches = [m for m in matches if m[0] == "http_rest"]
        assert len(http_matches) >= 1
        assert http_matches[0][2].startswith("signature:")


class TestDetectIntegrations:
    """Tests for the main detect_integrations function."""

    def test_detect_integrations_returns_result(self) -> None:
        """Should return IntegrationDetectorResult."""
        entries = [make_entry(name="HttpClient")]
        result = make_ctags_result(entries)

        detector_result = detect_integrations(result)

        assert isinstance(detector_result, IntegrationDetectorResult)
        assert detector_result.entries_scanned == 1

    def test_detect_integrations_finds_http_rest(self) -> None:
        """Should detect HTTP/REST integration points."""
        entries = [
            make_entry(name="UserController", kind="class"),
            make_entry(
                name="getUsers",
                signature='@GetMapping("/users")',
                scope="UserController",
            ),
        ]
        result = make_ctags_result(entries)

        detector_result = detect_integrations(result)

        http_points = [
            p
            for p in detector_result.integration_points
            if p.integration_type == "http_rest"
        ]
        assert len(http_points) >= 1

    def test_detect_integrations_finds_database(self) -> None:
        """Should detect database integration points."""
        entries = [
            make_entry(name="UserRepository", kind="interface"),
            make_entry(name="findById", signature="@Query", scope="UserRepository"),
        ]
        result = make_ctags_result(entries)

        detector_result = detect_integrations(result)

        db_points = [
            p
            for p in detector_result.integration_points
            if p.integration_type == "database"
        ]
        assert len(db_points) >= 1

    def test_detect_integrations_finds_messaging(self) -> None:
        """Should detect messaging integration points."""
        entries = [
            make_entry(name="OrderEventListener", kind="class"),
            make_entry(name="handleOrder", signature='@KafkaListener(topics="orders")'),
        ]
        result = make_ctags_result(entries)

        detector_result = detect_integrations(result)

        msg_points = [
            p
            for p in detector_result.integration_points
            if p.integration_type == "messaging"
        ]
        assert len(msg_points) >= 1

    def test_detect_integrations_finds_socket(self) -> None:
        """Should detect socket integration points."""
        entries = [
            make_entry(name="ChatWebSocket", kind="class"),
            make_entry(name="onOpen", signature="@OnOpen"),
        ]
        result = make_ctags_result(entries)

        detector_result = detect_integrations(result)

        socket_points = [
            p
            for p in detector_result.integration_points
            if p.integration_type == "socket"
        ]
        assert len(socket_points) >= 1

    def test_detect_integrations_finds_soap(self) -> None:
        """Should detect SOAP integration points."""
        entries = [
            make_entry(name="CustomerWebService", kind="class"),
            make_entry(name="getCustomer", signature="@WebService"),
        ]
        result = make_ctags_result(entries)

        detector_result = detect_integrations(result)

        soap_points = [
            p
            for p in detector_result.integration_points
            if p.integration_type == "soap"
        ]
        assert len(soap_points) >= 1

    def test_detect_integrations_empty_result(self) -> None:
        """Should handle empty CTags result."""
        result = make_ctags_result([])

        detector_result = detect_integrations(result)

        assert detector_result.entries_scanned == 0
        assert len(detector_result.integration_points) == 0

    def test_detect_integrations_no_matches(self) -> None:
        """Should handle entries with no integration patterns."""
        entries = [
            make_entry(name="StringUtils", kind="class"),
            make_entry(name="isEmpty", kind="method"),
        ]
        result = make_ctags_result(entries)

        detector_result = detect_integrations(result)

        assert detector_result.entries_scanned == 2
        assert len(detector_result.integration_points) == 0

    def test_integration_point_has_entry(self) -> None:
        """Should include the original entry in IntegrationPoint."""
        entry = make_entry(name="HttpClient")
        result = make_ctags_result([entry])

        detector_result = detect_integrations(result)

        assert len(detector_result.integration_points) >= 1
        assert detector_result.integration_points[0].entry is entry

    def test_integration_point_has_confidence(self) -> None:
        """Should include confidence level in IntegrationPoint."""
        entry = make_entry(name="foo", signature="@Repository")
        result = make_ctags_result([entry])

        detector_result = detect_integrations(result)

        db_points = [
            p
            for p in detector_result.integration_points
            if p.integration_type == "database"
        ]
        assert len(db_points) >= 1
        assert db_points[0].confidence == "high"


class TestPureFunctionBehavior:
    """Tests to verify pure function behavior (no side effects)."""

    def test_detect_integrations_does_not_modify_input(self) -> None:
        """Should not modify the input CTagsResult."""
        entries = [make_entry(name="HttpClient")]
        result = make_ctags_result(entries)
        original_len = len(result.entries)

        detect_integrations(result)

        assert len(result.entries) == original_len

    def test_classify_entry_does_not_modify_entry(self) -> None:
        """Should not modify the input CTagsEntry."""
        entry = make_entry(name="HttpClient", scope="MyClass")
        original_name = entry.name
        original_scope = entry.scope

        classify_entry(entry)

        assert entry.name == original_name
        assert entry.scope == original_scope

    def test_first_matching_pattern_is_pure(self) -> None:
        """Should be a pure function (tuples are immutable)."""
        patterns = (r"(?i)http", r"(?i)rest")
        result1 = first_matching_pattern("HttpClient", patterns)
        result2 = first_matching_pattern("HttpClient", patterns)

        # Same input produces same output
        assert result1 == result2


class TestEdgeCases:
    """Tests for edge cases and boundary conditions."""

    def test_entry_with_none_fields(self) -> None:
        """Should handle entries with None fields gracefully."""
        entry = make_entry(name="HttpClient", scope=None, signature=None)
        matches = classify_entry(entry)

        # Should still match on name
        assert len(matches) >= 1
        types = [m[0] for m in matches]
        assert "http_rest" in types

    def test_entry_with_empty_string_fields(self) -> None:
        """Should handle entries with empty string fields."""
        entry = make_entry(name="HttpClient", scope="", signature="")
        matches = classify_entry(entry)

        # Should still match on name
        assert len(matches) >= 1

    def test_case_sensitivity_in_patterns(self) -> None:
        """Should match case-insensitively for name patterns."""
        entry1 = make_entry(name="HTTPCLIENT")
        entry2 = make_entry(name="httpclient")
        entry3 = make_entry(name="HttpClient")

        matches1 = classify_entry(entry1)
        matches2 = classify_entry(entry2)
        matches3 = classify_entry(entry3)

        # All should match http_rest
        assert any(m[0] == "http_rest" for m in matches1)
        assert any(m[0] == "http_rest" for m in matches2)
        assert any(m[0] == "http_rest" for m in matches3)

    def test_partial_matches_in_longer_names(self) -> None:
        """Should match patterns within longer names."""
        entry = make_entry(name="MyHttpClientWrapper")
        matches = classify_entry(entry)

        types = [m[0] for m in matches]
        assert "http_rest" in types

    def test_special_characters_in_signature(self) -> None:
        """Should handle special characters in signatures."""
        entry = make_entry(
            name="findUsers",
            signature='@Query("SELECT * FROM users WHERE name = :name")',
        )
        matches = classify_entry(entry)

        db_matches = [m for m in matches if m[0] == "database"]
        assert len(db_matches) >= 1
