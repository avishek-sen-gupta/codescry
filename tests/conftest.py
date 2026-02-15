"""Shared pytest configuration and fixtures."""

import os

import pytest


def pytest_collection_modifyitems(config: pytest.Config, items: list[pytest.Item]) -> None:
    """Skip tests marked with local_repo when running in CI."""
    if not os.environ.get("CI"):
        return

    skip_marker = pytest.mark.skip(reason="requires local repository clones (CI)")
    for item in items:
        if "local_repo" in item.keywords:
            item.add_marker(skip_marker)
