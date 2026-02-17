# Testing & CI

## Running Tests

```bash
poetry run pytest
```

Every test run automatically measures code coverage and prints a per-file summary with missing line numbers. An HTML report is also generated:

```bash
open htmlcov/index.html   # browse detailed coverage report
```

Coverage is configured in `pyproject.toml` (`[tool.coverage.*]` sections). The `call_flow/`, `lsp_bridge/`, and `ml_classifier/` packages are excluded because they depend on external services.

## Local-Repo Integration Tests

Tests that depend on local repository clones (`~/code/mojo-lsp`, `~/code/smojol`) are marked with `@pytest.mark.local_repo`. These are automatically skipped when the `CI` environment variable is set (GitHub Actions sets this by default). Equivalent code paths are covered by synthetic `tmp_path`-based unit tests that run everywhere.

To run only the local-repo tests locally:

```bash
poetry run pytest -m local_repo
```

## Ollama LLM Tests

Tests that require a running Ollama instance are marked with `@pytest.mark.ollama`. These are automatically skipped when Ollama is not available (try-import guard) and in CI. To run them locally:

```bash
poetry run pytest -m ollama
```

## CI Pipeline

A GitHub Actions workflow is defined in `.github/workflows/ci.yml` with two jobs:

- **lint** — runs `black --check` on `src/` and `tests/`
- **test** — installs Universal CTags and runs `pytest` with coverage; `local_repo`-marked tests are auto-skipped via the `CI` environment variable

The pipeline runs on pushes to `main` and on pull requests targeting `main`. It can also be triggered manually via `workflow_dispatch`.
