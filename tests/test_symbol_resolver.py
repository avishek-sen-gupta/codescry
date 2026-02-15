"""Tests for symbol_resolver — joining integration signals to code symbols."""

from pathlib import Path

import pytest

from repo_surveyor.ctags import CTagsEntry, CTagsResult
from repo_surveyor.integration_detector import (
    EntityType,
    FileMatch,
    IntegrationDetectorResult,
    IntegrationSignal,
)
from repo_surveyor.integration_patterns import Confidence, IntegrationType
from repo_surveyor.symbol_resolver import (
    ResolutionResult,
    SymbolIntegration,
    SymbolIntegrationProfile,
    resolve_integration_signals,
)


def _entry(
    name: str,
    path: str,
    kind: str,
    line: int,
    end: int | None = None,
    scope: str | None = None,
    scope_kind: str | None = None,
    language: str | None = None,
) -> CTagsEntry:
    """Convenience factory for CTagsEntry."""
    return CTagsEntry(
        name=name,
        path=path,
        kind=kind,
        line=line,
        end=end,
        scope=scope,
        scope_kind=scope_kind,
        signature=None,
        language=language,
    )


def _ctags_result(entries: list[CTagsEntry]) -> CTagsResult:
    return CTagsResult(entries=entries, raw_output="", return_code=0)


def _signal(
    file_path: str,
    line_number: int,
    integration_type: IntegrationType = IntegrationType.HTTP_REST,
    entity_type: EntityType = EntityType.FILE_CONTENT,
) -> IntegrationSignal:
    return IntegrationSignal(
        match=FileMatch(
            file_path=file_path,
            line_number=line_number,
            line_content="some line",
            language=None,
        ),
        integration_type=integration_type,
        confidence=Confidence.HIGH,
        matched_pattern="test_pattern",
        entity_type=entity_type,
        source="test",
    )


def _integration_result(
    signals: list[IntegrationSignal],
) -> IntegrationDetectorResult:
    return IntegrationDetectorResult(integration_points=signals, files_scanned=1)


REPO_PATH = "/repo"


class TestIndexBuilding:
    """Symbols with end lines produce correct ranges."""

    def test_symbol_with_explicit_end(self) -> None:
        entries = [_entry("getUser", "src/Foo.java", "method", 10, end=20)]
        result = resolve_integration_signals(
            _ctags_result(entries),
            _integration_result([_signal("/repo/src/Foo.java", 15)]),
            REPO_PATH,
        )

        assert len(result.resolved) == 1
        assert result.resolved[0].symbol_name == "getUser"

    def test_line_at_start_boundary(self) -> None:
        entries = [_entry("getUser", "src/Foo.java", "method", 10, end=20)]
        result = resolve_integration_signals(
            _ctags_result(entries),
            _integration_result([_signal("/repo/src/Foo.java", 10)]),
            REPO_PATH,
        )

        assert len(result.resolved) == 1

    def test_line_at_end_boundary(self) -> None:
        entries = [_entry("getUser", "src/Foo.java", "method", 10, end=20)]
        result = resolve_integration_signals(
            _ctags_result(entries),
            _integration_result([_signal("/repo/src/Foo.java", 20)]),
            REPO_PATH,
        )

        assert len(result.resolved) == 1


class TestMostSpecificMatch:
    """A line inside a method inside a class resolves to the method."""

    def test_method_inside_class(self) -> None:
        entries = [
            _entry("UserController", "src/Foo.java", "class", 1, end=50),
            _entry("getUser", "src/Foo.java", "method", 10, end=20),
        ]
        result = resolve_integration_signals(
            _ctags_result(entries),
            _integration_result([_signal("/repo/src/Foo.java", 15)]),
            REPO_PATH,
        )

        assert len(result.resolved) == 1
        assert result.resolved[0].symbol_name == "getUser"
        assert result.resolved[0].symbol_kind == "method"

    def test_line_outside_method_but_inside_class(self) -> None:
        entries = [
            _entry("UserController", "src/Foo.java", "class", 1, end=50),
            _entry("getUser", "src/Foo.java", "method", 10, end=20),
        ]
        result = resolve_integration_signals(
            _ctags_result(entries),
            _integration_result([_signal("/repo/src/Foo.java", 30)]),
            REPO_PATH,
        )

        assert len(result.resolved) == 1
        assert result.resolved[0].symbol_name == "UserController"


class TestUnresolvedSignals:
    """Line outside any symbol range goes to unresolved."""

    def test_line_beyond_all_symbols(self) -> None:
        entries = [_entry("getUser", "src/Foo.java", "method", 10, end=20)]
        result = resolve_integration_signals(
            _ctags_result(entries),
            _integration_result([_signal("/repo/src/Foo.java", 100)]),
            REPO_PATH,
        )

        assert len(result.resolved) == 0
        assert len(result.unresolved) == 1

    def test_file_not_in_ctags(self) -> None:
        entries = [_entry("getUser", "src/Foo.java", "method", 10, end=20)]
        result = resolve_integration_signals(
            _ctags_result(entries),
            _integration_result([_signal("/repo/src/Bar.java", 5)]),
            REPO_PATH,
        )

        assert len(result.resolved) == 0
        assert len(result.unresolved) == 1


class TestDirectorySignals:
    """Directory-level signals go directly to unresolved."""

    def test_directory_entity_type_unresolved(self) -> None:
        entries = [_entry("getUser", "src/Foo.java", "method", 10, end=20)]
        dir_signal = _signal(
            "/repo/src/controllers",
            0,
            entity_type=EntityType.DIRECTORY,
        )
        result = resolve_integration_signals(
            _ctags_result(entries),
            _integration_result([dir_signal]),
            REPO_PATH,
        )

        assert len(result.resolved) == 0
        assert len(result.unresolved) == 1
        assert result.unresolved[0].entity_type == EntityType.DIRECTORY


class TestPathNormalisation:
    """Absolute paths are correctly stripped to relative."""

    def test_strips_repo_prefix(self) -> None:
        entries = [_entry("handler", "api/routes.py", "function", 5, end=15)]
        result = resolve_integration_signals(
            _ctags_result(entries),
            _integration_result([_signal("/repo/api/routes.py", 10)]),
            REPO_PATH,
        )

        assert len(result.resolved) == 1
        assert result.resolved[0].symbol_name == "handler"

    def test_repo_path_with_trailing_slash(self) -> None:
        entries = [_entry("handler", "api/routes.py", "function", 5, end=15)]
        result = resolve_integration_signals(
            _ctags_result(entries),
            _integration_result([_signal("/repo/api/routes.py", 10)]),
            "/repo/",
        )

        assert len(result.resolved) == 1


class TestProfileGrouping:
    """Multiple signals in the same method produce one profile."""

    def test_multiple_signals_same_symbol(self) -> None:
        entries = [_entry("handler", "api/routes.py", "function", 5, end=25)]
        signals = [
            _signal("/repo/api/routes.py", 10, IntegrationType.HTTP_REST),
            _signal("/repo/api/routes.py", 15, IntegrationType.DATABASE),
        ]
        result = resolve_integration_signals(
            _ctags_result(entries),
            _integration_result(signals),
            REPO_PATH,
        )

        assert len(result.profiles) == 1
        profile = result.profiles[0]
        assert profile.symbol_name == "handler"
        assert len(profile.integrations) == 2
        types = {si.signal.integration_type for si in profile.integrations}
        assert types == {IntegrationType.HTTP_REST, IntegrationType.DATABASE}

    def test_signals_in_different_symbols(self) -> None:
        entries = [
            _entry("get_user", "api/routes.py", "function", 5, end=15),
            _entry("save_user", "api/routes.py", "function", 20, end=30),
        ]
        signals = [
            _signal("/repo/api/routes.py", 10),
            _signal("/repo/api/routes.py", 25),
        ]
        result = resolve_integration_signals(
            _ctags_result(entries),
            _integration_result(signals),
            REPO_PATH,
        )

        assert len(result.profiles) == 2
        names = {p.symbol_name for p in result.profiles}
        assert names == {"get_user", "save_user"}


class TestEmptyInputs:
    """No symbols or no signals produce empty result."""

    def test_no_symbols(self) -> None:
        result = resolve_integration_signals(
            _ctags_result([]),
            _integration_result([_signal("/repo/src/Foo.java", 10)]),
            REPO_PATH,
        )

        assert len(result.resolved) == 0
        assert len(result.unresolved) == 1

    def test_no_signals(self) -> None:
        entries = [_entry("getUser", "src/Foo.java", "method", 10, end=20)]
        result = resolve_integration_signals(
            _ctags_result(entries),
            _integration_result([]),
            REPO_PATH,
        )

        assert len(result.resolved) == 0
        assert len(result.unresolved) == 0
        assert len(result.profiles) == 0

    def test_both_empty(self) -> None:
        result = resolve_integration_signals(
            _ctags_result([]),
            _integration_result([]),
            REPO_PATH,
        )

        assert result == ResolutionResult(resolved=(), unresolved=(), profiles=())


class TestSymbolsWithoutEnd:
    """Symbols without end line use next symbol's start - 1."""

    def test_inferred_end_from_next_symbol(self) -> None:
        entries = [
            _entry("method_a", "src/Foo.java", "method", 10),
            _entry("method_b", "src/Foo.java", "method", 20),
        ]
        result = resolve_integration_signals(
            _ctags_result(entries),
            _integration_result([_signal("/repo/src/Foo.java", 15)]),
            REPO_PATH,
        )

        assert len(result.resolved) == 1
        assert result.resolved[0].symbol_name == "method_a"

    def test_line_at_inferred_boundary(self) -> None:
        """Line 19 (next symbol start - 1) should still be in method_a."""
        entries = [
            _entry("method_a", "src/Foo.java", "method", 10),
            _entry("method_b", "src/Foo.java", "method", 20),
        ]
        result = resolve_integration_signals(
            _ctags_result(entries),
            _integration_result([_signal("/repo/src/Foo.java", 19)]),
            REPO_PATH,
        )

        assert len(result.resolved) == 1
        assert result.resolved[0].symbol_name == "method_a"

    def test_last_symbol_without_end_uses_sentinel(self) -> None:
        """Last symbol without end gets a large sentinel, so high line numbers resolve."""
        entries = [_entry("main", "src/App.java", "method", 5)]
        result = resolve_integration_signals(
            _ctags_result(entries),
            _integration_result([_signal("/repo/src/App.java", 500)]),
            REPO_PATH,
        )

        assert len(result.resolved) == 1
        assert result.resolved[0].symbol_name == "main"


class TestToJson:
    """ResolutionResult.to_json() produces valid JSON."""

    def test_to_json_structure(self) -> None:
        entries = [_entry("handler", "api/routes.py", "function", 5, end=25)]
        signals = [
            _signal("/repo/api/routes.py", 10, IntegrationType.HTTP_REST),
            _signal("/repo/api/routes.py", 15, IntegrationType.DATABASE),
        ]
        result = resolve_integration_signals(
            _ctags_result(entries),
            _integration_result(signals),
            REPO_PATH,
        )

        import json

        parsed = json.loads(result.to_json())
        assert parsed["resolved_count"] == 2
        assert parsed["unresolved_count"] == 0
        assert len(parsed["profiles"]) == 1
        assert parsed["profiles"][0]["integration_count"] == 2
        assert sorted(parsed["profiles"][0]["integration_types"]) == [
            "database",
            "http_rest",
        ]


class TestSurveyEndToEnd:
    """End-to-end test via survey() with a synthetic repo."""

    def test_survey_returns_four_tuple(self, tmp_path: Path) -> None:
        """survey() should return a 4-tuple including ResolutionResult."""
        (tmp_path / "pyproject.toml").write_text(
            '[project]\ndependencies = ["fastapi"]\n'
        )
        src = tmp_path / "src"
        src.mkdir()
        (src / "app.py").write_text(
            "from fastapi import FastAPI\n"
            "\n"
            "app = FastAPI()\n"
            "\n"
            "@app.get('/health')\n"
            "def health():\n"
            "    return {'status': 'ok'}\n"
        )

        from repo_surveyor.surveyor import survey

        tech_report, structure_result, integration_result, resolution = survey(
            str(tmp_path)
        )

        assert tech_report is not None
        assert structure_result.success
        assert isinstance(resolution, ResolutionResult)
