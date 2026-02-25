"""Tests for evidence check infrastructure."""

import pytest

from repo_surveyor.integration_concretiser.evidence import (
    UNIVERSAL_CHECKS,
    EvidenceCheck,
    EvidenceCheckResult,
    EvidenceContext,
    EvidenceVerdict,
    evaluate_evidence,
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _ctx(
    file_path: str = "src/app.py",
    line_number: int = 10,
    line_content: str = "requests.get(url)",
    source_lines: tuple[str, ...] = ("import requests", "", "requests.get(url)"),
) -> EvidenceContext:
    return EvidenceContext(
        file_path=file_path,
        line_number=line_number,
        line_content=line_content,
        source_lines=source_lines,
    )


def _always_fires(ctx: EvidenceContext) -> bool:
    return True


def _never_fires(ctx: EvidenceContext) -> bool:
    return False


# ---------------------------------------------------------------------------
# Tests: evaluate_evidence core logic
# ---------------------------------------------------------------------------


class TestEvaluateEvidence:
    """Core evaluation logic: accumulation, clamping, result structure."""

    def test_no_checks_returns_raw_score(self):
        verdict = evaluate_evidence(_ctx(), checks=(), raw_score=0.65)
        assert verdict.adjusted_score == 0.65
        assert verdict.original_score == 0.65
        assert verdict.score_adjustment == 0.0
        assert verdict.results == ()

    def test_unfired_checks_produce_no_adjustment(self):
        checks = (
            EvidenceCheck(name="noop1", check=_never_fires, weight=-0.20),
            EvidenceCheck(name="noop2", check=_never_fires, weight=-0.10),
        )
        verdict = evaluate_evidence(_ctx(), checks=checks, raw_score=0.70)
        assert verdict.adjusted_score == 0.70
        assert verdict.score_adjustment == 0.0
        assert all(not r.fired for r in verdict.results)

    def test_single_suppression_reduces_score(self):
        checks = (EvidenceCheck(name="suppress", check=_always_fires, weight=-0.20),)
        verdict = evaluate_evidence(_ctx(), checks=checks, raw_score=0.70)
        assert verdict.adjusted_score == pytest.approx(0.50)
        assert verdict.score_adjustment == pytest.approx(-0.20)

    def test_single_boost_increases_score(self):
        checks = (EvidenceCheck(name="boost", check=_always_fires, weight=0.10),)
        verdict = evaluate_evidence(_ctx(), checks=checks, raw_score=0.55)
        assert verdict.adjusted_score == pytest.approx(0.65)
        assert verdict.score_adjustment == pytest.approx(0.10)

    def test_multiple_fired_checks_accumulate(self):
        checks = (
            EvidenceCheck(name="a", check=_always_fires, weight=-0.10),
            EvidenceCheck(name="b", check=_always_fires, weight=-0.15),
            EvidenceCheck(name="c", check=_never_fires, weight=-0.50),
        )
        verdict = evaluate_evidence(_ctx(), checks=checks, raw_score=0.70)
        assert verdict.adjusted_score == pytest.approx(0.45)
        assert verdict.score_adjustment == pytest.approx(-0.25)

    def test_clamped_to_zero(self):
        checks = (EvidenceCheck(name="nuke", check=_always_fires, weight=-2.0),)
        verdict = evaluate_evidence(_ctx(), checks=checks, raw_score=0.30)
        assert verdict.adjusted_score == 0.0

    def test_clamped_to_one(self):
        checks = (EvidenceCheck(name="mega", check=_always_fires, weight=2.0),)
        verdict = evaluate_evidence(_ctx(), checks=checks, raw_score=0.80)
        assert verdict.adjusted_score == 1.0

    def test_result_structure(self):
        checks = (
            EvidenceCheck(name="fired", check=_always_fires, weight=-0.10),
            EvidenceCheck(name="quiet", check=_never_fires, weight=-0.20),
        )
        verdict = evaluate_evidence(_ctx(), checks=checks, raw_score=0.65)
        assert len(verdict.results) == 2
        assert verdict.results[0] == EvidenceCheckResult(
            name="fired", fired=True, weight=-0.10
        )
        assert verdict.results[1] == EvidenceCheckResult(
            name="quiet", fired=False, weight=-0.20
        )


# ---------------------------------------------------------------------------
# Tests: Universal suppression checks
# ---------------------------------------------------------------------------


class TestInTestFile:
    """in_test_file check: matches test file path conventions."""

    @pytest.mark.parametrize(
        "path",
        [
            "tests/test_foo.py",
            "test/something.py",
            "src/__tests__/widget.tsx",
            "lib/test_utils.py",
            "foo/bar.test.js",
            "foo/bar.spec.ts",
        ],
    )
    def test_fires_on_test_paths(self, path: str):
        ctx = _ctx(file_path=path)
        verdict = evaluate_evidence(ctx, checks=UNIVERSAL_CHECKS, raw_score=0.70)
        result = next(r for r in verdict.results if r.name == "in_test_file")
        assert result.fired

    @pytest.mark.parametrize(
        "path",
        [
            "src/app.py",
            "lib/client.ts",
            "src/services/auth.py",
        ],
    )
    def test_does_not_fire_on_non_test_paths(self, path: str):
        ctx = _ctx(file_path=path)
        verdict = evaluate_evidence(ctx, checks=UNIVERSAL_CHECKS, raw_score=0.70)
        result = next(r for r in verdict.results if r.name == "in_test_file")
        assert not result.fired


class TestInVendorDir:
    """in_vendor_dir check: matches vendor/third-party directories."""

    @pytest.mark.parametrize(
        "path",
        [
            "vendor/lib/foo.py",
            "third_party/grpc/client.py",
            "thirdparty/utils.py",
            "node_modules/express/index.js",
            ".bundle/gems/foo.rb",
            "external/lib.c",
        ],
    )
    def test_fires_on_vendor_paths(self, path: str):
        ctx = _ctx(file_path=path)
        verdict = evaluate_evidence(ctx, checks=UNIVERSAL_CHECKS, raw_score=0.70)
        result = next(r for r in verdict.results if r.name == "in_vendor_dir")
        assert result.fired

    def test_does_not_fire_on_src(self):
        ctx = _ctx(file_path="src/services/client.py")
        verdict = evaluate_evidence(ctx, checks=UNIVERSAL_CHECKS, raw_score=0.70)
        result = next(r for r in verdict.results if r.name == "in_vendor_dir")
        assert not result.fired


class TestInGeneratedFile:
    """in_generated_file check: matches generated file paths or header comments."""

    def test_fires_on_generated_path(self):
        ctx = _ctx(file_path="src/generated/api_client.py")
        verdict = evaluate_evidence(ctx, checks=UNIVERSAL_CHECKS, raw_score=0.70)
        result = next(r for r in verdict.results if r.name == "in_generated_file")
        assert result.fired

    def test_fires_on_auto_gen_path(self):
        ctx = _ctx(file_path="src/autogen/models.py")
        verdict = evaluate_evidence(ctx, checks=UNIVERSAL_CHECKS, raw_score=0.70)
        result = next(r for r in verdict.results if r.name == "in_generated_file")
        assert result.fired

    def test_fires_on_header_comment(self):
        ctx = _ctx(
            file_path="src/api.py",
            source_lines=(
                "# Auto-generated by protobuf compiler",
                "# Do not edit",
                "import grpc",
                "",
                "class Service:",
            ),
        )
        verdict = evaluate_evidence(ctx, checks=UNIVERSAL_CHECKS, raw_score=0.70)
        result = next(r for r in verdict.results if r.name == "in_generated_file")
        assert result.fired

    def test_does_not_fire_on_normal_file(self):
        ctx = _ctx(
            file_path="src/app.py",
            source_lines=("import os", "def main():", "    pass"),
        )
        verdict = evaluate_evidence(ctx, checks=UNIVERSAL_CHECKS, raw_score=0.70)
        result = next(r for r in verdict.results if r.name == "in_generated_file")
        assert not result.fired


class TestInConfigDir:
    """in_config_dir check: matches config directories and file extensions."""

    @pytest.mark.parametrize(
        "path",
        [
            "config/database.py",
            ".config/settings.py",
            "app/settings.json",
            "myapp.yaml",
            "setup.cfg",
            "env.properties",
        ],
    )
    def test_fires_on_config_paths(self, path: str):
        ctx = _ctx(file_path=path)
        verdict = evaluate_evidence(ctx, checks=UNIVERSAL_CHECKS, raw_score=0.70)
        result = next(r for r in verdict.results if r.name == "in_config_dir")
        assert result.fired

    def test_does_not_fire_on_source_file(self):
        ctx = _ctx(file_path="src/client.py")
        verdict = evaluate_evidence(ctx, checks=UNIVERSAL_CHECKS, raw_score=0.70)
        result = next(r for r in verdict.results if r.name == "in_config_dir")
        assert not result.fired


class TestInStringLiteral:
    """in_string_literal check: matches lines dominated by string literals."""

    @pytest.mark.parametrize(
        "line",
        [
            '"https://api.example.com/v1/users",',
            "'SELECT * FROM users',",
            '"database connection string")',
        ],
    )
    def test_fires_on_string_lines(self, line: str):
        ctx = _ctx(line_content=line)
        verdict = evaluate_evidence(ctx, checks=UNIVERSAL_CHECKS, raw_score=0.70)
        result = next(r for r in verdict.results if r.name == "in_string_literal")
        assert result.fired

    def test_does_not_fire_on_code(self):
        ctx = _ctx(line_content="response = requests.get(url)")
        verdict = evaluate_evidence(ctx, checks=UNIVERSAL_CHECKS, raw_score=0.70)
        result = next(r for r in verdict.results if r.name == "in_string_literal")
        assert not result.fired


class TestInLogStatement:
    """in_log_statement check: matches logging calls."""

    @pytest.mark.parametrize(
        "line",
        [
            'logger.info("Connecting to database")',
            'logging.debug("Request sent to %s", url)',
            'console.log("fetching data")',
            'logger.warning("Connection timeout")',
        ],
    )
    def test_fires_on_log_lines(self, line: str):
        ctx = _ctx(line_content=line)
        verdict = evaluate_evidence(ctx, checks=UNIVERSAL_CHECKS, raw_score=0.70)
        result = next(r for r in verdict.results if r.name == "in_log_statement")
        assert result.fired

    def test_does_not_fire_on_normal_code(self):
        ctx = _ctx(line_content="result = db.query(sql)")
        verdict = evaluate_evidence(ctx, checks=UNIVERSAL_CHECKS, raw_score=0.70)
        result = next(r for r in verdict.results if r.name == "in_log_statement")
        assert not result.fired


class TestInConstantDecl:
    """in_constant_decl check: matches constant declaration patterns."""

    @pytest.mark.parametrize(
        "line",
        [
            'API_BASE_URL = "https://api.example.com"',
            "DATABASE_PORT = 5432",
            'const API_KEY = "abc123"',
            "MAX_RETRIES: int = 3",
        ],
    )
    def test_fires_on_constant_declarations(self, line: str):
        ctx = _ctx(line_content=line)
        verdict = evaluate_evidence(ctx, checks=UNIVERSAL_CHECKS, raw_score=0.70)
        result = next(r for r in verdict.results if r.name == "in_constant_decl")
        assert result.fired

    def test_does_not_fire_on_normal_code(self):
        ctx = _ctx(line_content="response = client.fetch(url)")
        verdict = evaluate_evidence(ctx, checks=UNIVERSAL_CHECKS, raw_score=0.70)
        result = next(r for r in verdict.results if r.name == "in_constant_decl")
        assert not result.fired


# ---------------------------------------------------------------------------
# Tests: UNIVERSAL_CHECKS tuple integrity
# ---------------------------------------------------------------------------


class TestUniversalChecks:
    """Verify UNIVERSAL_CHECKS tuple is well-formed."""

    def test_all_checks_have_negative_weights(self):
        for check in UNIVERSAL_CHECKS:
            assert check.weight < 0, f"{check.name} should have negative weight"

    def test_all_checks_have_unique_names(self):
        names = [c.name for c in UNIVERSAL_CHECKS]
        assert len(names) == len(set(names))

    def test_expected_check_count(self):
        assert len(UNIVERSAL_CHECKS) == 7

    def test_clean_context_has_no_suppression(self):
        """A normal source file with normal code should fire no universal checks."""
        ctx = _ctx(
            file_path="src/services/http_client.py",
            line_content="response = requests.get(url)",
            source_lines=("import requests", "", "response = requests.get(url)"),
        )
        verdict = evaluate_evidence(ctx, checks=UNIVERSAL_CHECKS, raw_score=0.70)
        assert verdict.adjusted_score == 0.70
        assert verdict.score_adjustment == 0.0
