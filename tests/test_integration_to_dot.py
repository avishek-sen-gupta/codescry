"""Tests for integration_to_dot module."""

from repo_surveyor.integration_detector import (
    EntityType,
    FileMatch,
    IntegrationSignal,
)
from repo_surveyor.integration_patterns.types import Confidence, IntegrationType
from repo_surveyor.symbol_resolver import (
    ResolutionResult,
    SymbolIntegration,
    SymbolIntegrationProfile,
)
from repo_surveyor.export.integration_to_dot import integration_to_dot


def _make_signal(
    integration_type: IntegrationType = IntegrationType.HTTP_REST,
    confidence: Confidence = Confidence.HIGH,
    file_path: str = "src/app.py",
    line: int = 10,
) -> IntegrationSignal:
    return IntegrationSignal(
        match=FileMatch(
            file_path=file_path,
            line_number=line,
            line_content="some code",
            language=None,
        ),
        integration_type=integration_type,
        confidence=confidence,
        matched_pattern="pattern",
        entity_type=EntityType.FILE_CONTENT,
        source="python",
    )


def _make_profile(
    name: str = "my_func",
    kind: str = "function",
    path: str = "src/app.py",
    signals: list[IntegrationSignal] = [],
) -> SymbolIntegrationProfile:
    integrations = tuple(
        SymbolIntegration(
            symbol_id=f"{path}:{name}:{kind}:1",
            symbol_name=name,
            symbol_kind=kind,
            signal=s,
        )
        for s in signals
    )
    return SymbolIntegrationProfile(
        symbol_id=f"{path}:{name}:{kind}:1",
        symbol_name=name,
        symbol_kind=kind,
        symbol_path=path,
        integrations=integrations,
    )


class TestIntegrationToDot:
    def test_empty_result_produces_valid_digraph(self):
        result = ResolutionResult(resolved=(), unresolved=(), profiles=())
        dot = integration_to_dot(result)
        assert dot.startswith("digraph")
        assert dot.endswith("}")

    def test_single_profile_with_two_types(self):
        sig_http = _make_signal(IntegrationType.HTTP_REST, Confidence.HIGH)
        sig_db = _make_signal(IntegrationType.DATABASE, Confidence.MEDIUM)
        profile = _make_profile(
            name="handle_request",
            signals=[sig_http, sig_db],
        )
        result = ResolutionResult(
            resolved=tuple(profile.integrations),
            unresolved=(),
            profiles=(profile,),
        )
        dot = integration_to_dot(result)

        # Should have cluster for the file
        assert "subgraph cluster_" in dot
        assert "src/app.py" in dot

        # Should have symbol node
        assert "handle_request" in dot

        # Should have integration type nodes
        assert "http_rest" in dot
        assert "database" in dot

        # Should have edges with confidence labels
        assert 'label="high"' in dot
        assert 'label="medium"' in dot

    def test_max_profiles_cap(self):
        profiles = tuple(
            _make_profile(
                name=f"func_{i}",
                signals=[_make_signal()],
            )
            for i in range(20)
        )
        all_resolved = tuple(si for p in profiles for si in p.integrations)
        result = ResolutionResult(
            resolved=all_resolved,
            unresolved=(),
            profiles=profiles,
        )
        dot = integration_to_dot(result, max_profiles=5)

        # Only first 5 function names should appear as nodes
        assert "func_0" in dot
        assert "func_4" in dot
        assert "func_5" not in dot

    def test_custom_title(self):
        result = ResolutionResult(resolved=(), unresolved=(), profiles=())
        dot = integration_to_dot(result, title="My Integrations")
        assert 'digraph "My Integrations"' in dot
