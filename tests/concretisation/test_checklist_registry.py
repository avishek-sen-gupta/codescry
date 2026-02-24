"""Tests for ChecklistRegistry JSON loading and lookup logic."""

import json
from pathlib import Path

import pytest

from repo_surveyor.integration_concretiser.checklist_registry import (
    ChecklistRegistry,
)
from repo_surveyor.integration_concretiser.evidence_predicates import (
    PredicateName,
)


class TestChecklistLoading:
    def test_loads_default_predicates(self, tmp_path: Path):
        checklist_file = tmp_path / "checklist.json"
        checklist_file.write_text(
            json.dumps(
                {
                    "version": "1.0",
                    "default_predicates": [
                        {
                            "predicate": "IN_STRING_CONTEXT",
                            "weight": -0.3,
                            "pattern_arg": "",
                        },
                        {
                            "predicate": "IN_TEST_FILE",
                            "weight": -0.15,
                            "pattern_arg": "",
                        },
                    ],
                    "pattern_overrides": {},
                }
            ),
            encoding="utf-8",
        )
        registry = ChecklistRegistry(checklist_file)
        checklist = registry.get_checklist("common", "some_pattern")

        assert len(checklist) == 2
        assert checklist[0].predicate == PredicateName.IN_STRING_CONTEXT
        assert checklist[0].weight == -0.3
        assert checklist[1].predicate == PredicateName.IN_TEST_FILE

    def test_pattern_override_takes_precedence(self, tmp_path: Path):
        checklist_file = tmp_path / "checklist.json"
        checklist_file.write_text(
            json.dumps(
                {
                    "version": "1.0",
                    "default_predicates": [
                        {
                            "predicate": "IN_STRING_CONTEXT",
                            "weight": -0.3,
                            "pattern_arg": "",
                        },
                    ],
                    "pattern_overrides": {
                        "http_url_pattern": [
                            {
                                "predicate": "IN_LOG_STATEMENT",
                                "weight": -0.25,
                                "pattern_arg": "",
                            }
                        ]
                    },
                }
            ),
            encoding="utf-8",
        )
        registry = ChecklistRegistry(checklist_file)

        # Override should be used for matching pattern
        override = registry.get_checklist("common", "http_url_pattern")
        assert len(override) == 1
        assert override[0].predicate == PredicateName.IN_LOG_STATEMENT

        # Default should be used for non-matching pattern
        default = registry.get_checklist("common", "other_pattern")
        assert len(default) == 1
        assert default[0].predicate == PredicateName.IN_STRING_CONTEXT

    def test_qualified_key_takes_precedence_over_pattern(self, tmp_path: Path):
        checklist_file = tmp_path / "checklist.json"
        checklist_file.write_text(
            json.dumps(
                {
                    "version": "1.0",
                    "default_predicates": [],
                    "pattern_overrides": {
                        "http_url_pattern": [
                            {
                                "predicate": "IN_LOG_STATEMENT",
                                "weight": -0.25,
                                "pattern_arg": "",
                            }
                        ],
                        "java:http_url_pattern": [
                            {
                                "predicate": "IN_ASSERTION",
                                "weight": -0.2,
                                "pattern_arg": "",
                            }
                        ],
                    },
                }
            ),
            encoding="utf-8",
        )
        registry = ChecklistRegistry(checklist_file)

        # Qualified key should take precedence
        qualified = registry.get_checklist("java", "http_url_pattern")
        assert len(qualified) == 1
        assert qualified[0].predicate == PredicateName.IN_ASSERTION

        # Unqualified pattern for different source
        unqualified = registry.get_checklist("python", "http_url_pattern")
        assert len(unqualified) == 1
        assert unqualified[0].predicate == PredicateName.IN_LOG_STATEMENT

    def test_missing_file_returns_empty_checklist(self, tmp_path: Path):
        missing = tmp_path / "nonexistent.json"
        registry = ChecklistRegistry(missing)
        checklist = registry.get_checklist("common", "any_pattern")
        assert checklist == ()

    def test_invalid_json_returns_empty_checklist(self, tmp_path: Path):
        bad_file = tmp_path / "bad.json"
        bad_file.write_text("not valid json {{{", encoding="utf-8")
        registry = ChecklistRegistry(bad_file)
        checklist = registry.get_checklist("common", "any_pattern")
        assert checklist == ()

    def test_pattern_arg_defaults_to_empty(self, tmp_path: Path):
        checklist_file = tmp_path / "checklist.json"
        checklist_file.write_text(
            json.dumps(
                {
                    "version": "1.0",
                    "default_predicates": [
                        {
                            "predicate": "IN_STRING_CONTEXT",
                            "weight": -0.3,
                        }
                    ],
                    "pattern_overrides": {},
                }
            ),
            encoding="utf-8",
        )
        registry = ChecklistRegistry(checklist_file)
        checklist = registry.get_checklist("common", "any")
        assert checklist[0].pattern_arg == ""


class TestDefaultChecklistFile:
    """Verify the shipped default checklist file loads correctly."""

    def test_default_checklist_loads(self):
        default_path = Path("data/evidence_checklists/default.json")
        if not default_path.exists():
            pytest.skip(
                "Default checklist file not found (running outside project root)"
            )
        registry = ChecklistRegistry(default_path)
        checklist = registry.get_checklist("common", "any_pattern")
        assert len(checklist) > 0
        predicates = {entry.predicate for entry in checklist}
        assert PredicateName.IN_STRING_CONTEXT in predicates
        assert PredicateName.IN_TEST_FILE in predicates
