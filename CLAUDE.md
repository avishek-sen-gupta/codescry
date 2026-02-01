# Cartographer - Claude Code Instructions

## Project Overview

Cartographer is a Python library for analyzing repository technology stacks and code structure. It detects languages, package managers, frameworks, and infrastructure from indicator files, and can extract code symbols using CTags.

## Commands

```bash
# Install dependencies
poetry install

# Run tests
poetry run pytest

# Run tests with verbose output
poetry run pytest -v

# Run specific test file
poetry run pytest tests/test_neo4j_persistence.py

# Format code
poetry run black src/ tests/

# Type check
poetry run mypy src/
```

## Build

Before committing anything, run all tests, fixing them if necessary. If test assertions are being removed, ask me to review them.

## Testing Patterns

- Use `pytest` with fixtures for test setup
- Do not patch with `unittest.mock.patch`. Use proper dependency injection, and then inject mock objects.
- Use `tmp_path` fixture for filesystem tests
- Tests requiring external repos (mojo-lsp, smojol) are integration tests

## Programming Patterns

- Use proper dependency injection for interfaces to external systems like Neo4J, OS, and File I/O. Do not hardcode importing the concrete modules in these cases.
- Minimise and/or avoid mutation
- Write your code in the Functional Programming style, but balance it with readability

## Dependencies

- Python 3.13+
- Poetry for dependency management
- Universal CTags (external) for code symbol extraction
- Neo4j (optional) for graph persistence

## Notes

- Use `poetry run` prefix for all Python commands
- If Talisman detects a potential secret, stop what you are doing, prompt me for what needs to be done, and only then should you update the `.talismanrc` file.
- Potential secrets in files trigger Talisman pre-commit hook - add to `.talismanrc` if needed. Don't overwrite existing `.talismanrc` entries, add at the end
- Integration tests depend on local repo paths (`~/code/mojo-lsp`, `~/code/smojol`)
