"""Shared pytest configuration and fixtures."""

import os

import pytest


def pytest_collection_modifyitems(
    config: pytest.Config, items: list[pytest.Item]
) -> None:
    """Skip tests marked with local_repo when running in CI."""
    if not os.environ.get("CI"):
        return

    skip_local = pytest.mark.skip(reason="requires local repository clones (CI)")
    skip_ollama = pytest.mark.skip(reason="requires Ollama instance (CI)")
    for item in items:
        if "local_repo" in item.keywords:
            item.add_marker(skip_local)
        if "ollama" in item.keywords:
            item.add_marker(skip_ollama)
