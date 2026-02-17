#!/usr/bin/env python3
"""Generate cfg_roles.json by classifying tree-sitter grammars for all languages.

Fetches ``language_definitions.json`` from the tree-sitter-language-pack GitHub
repo, iterates all languages, fetches each grammar.js, extracts node types,
classifies them via an LLM (Qwen via Ollama), and writes the results to a
static JSON config.

Usage::

    poetry run python scripts/generate_cfg_roles.py
    poetry run python scripts/generate_cfg_roles.py --output path/to/cfg_roles.json
    poetry run python scripts/generate_cfg_roles.py --model qwen2.5-coder:7b-instruct

Features:
- **Incremental**: skips languages already in ``languages`` or ``failures``
- **Crash-safe**: saves JSON after every language
- **Graceful failures**: catches exceptions, records in ``failures``, continues
"""

import argparse
import json
import sys
from datetime import datetime, timezone
from pathlib import Path

import requests

from repo_surveyor.cfg_constructor.grammar_classifier import (
    SYSTEM_PROMPT,
    build_user_prompt,
    extract_node_types,
    parse_classification_response,
)
from repo_surveyor.ml_classifier.qwen_model import QwenClassifierModel

_LANG_DEFS_URL = (
    "https://raw.githubusercontent.com/Goldziher/"
    "tree-sitter-language-pack/main/sources/language_definitions.json"
)

_DEFAULT_OUTPUT = (
    Path(__file__).resolve().parents[1]
    / "src"
    / "repo_surveyor"
    / "cfg_constructor"
    / "cfg_roles.json"
)

_DEFAULT_MODEL = "qwen2.5-coder:7b-instruct"


def _build_grammar_url(repo_url: str, rev: str, directory: str) -> str:
    """Build the raw GitHub URL for a grammar.js file.

    Args:
        repo_url: Full GitHub repository URL (e.g. ``https://github.com/tree-sitter/tree-sitter-java``).
        rev: Git revision (commit SHA or branch).
        directory: Subdirectory within the repo (may be empty).

    Returns:
        Raw GitHub content URL for the grammar.js file.
    """
    owner_repo = repo_url.removeprefix("https://github.com/")
    base = f"https://raw.githubusercontent.com/{owner_repo}/{rev}"
    path = f"{directory}/grammar.js" if directory else "grammar.js"
    return f"{base}/{path}"


def _fetch_grammar_js(url: str) -> str:
    """Fetch grammar.js content from a raw GitHub URL.

    Args:
        url: Raw GitHub content URL.

    Returns:
        The grammar.js file contents as a string.

    Raises:
        requests.HTTPError: If the request fails.
    """
    response = requests.get(url, timeout=30)
    response.raise_for_status()
    return response.text


def _classify_language(grammar_js: str, model: QwenClassifierModel) -> dict[str, str]:
    """Extract node types from a grammar and classify them via LLM.

    Args:
        grammar_js: Full text of a tree-sitter grammar.js file.
        model: The LLM classifier model instance.

    Returns:
        Mapping from node type to role value string (e.g. ``"branch"``).
    """
    node_types = extract_node_types(grammar_js)
    user_prompt = build_user_prompt(node_types)
    result = model.classify(SYSTEM_PROMPT, user_prompt)
    parsed = parse_classification_response(result.text)
    return {node_type: role.value for node_type, role in parsed.items()}


def _load_existing(path: Path) -> dict:
    """Load existing cfg_roles.json or return a fresh skeleton.

    Args:
        path: Path to the JSON config file.

    Returns:
        The parsed JSON dict, or a fresh skeleton if the file doesn't exist.
    """
    if path.exists():
        return json.loads(path.read_text())
    return {
        "meta": {"generated_at": "", "source": "tree-sitter-language-pack"},
        "languages": {},
        "failures": {},
    }


def _save(config: dict, path: Path) -> None:
    """Write the config dict to disk as formatted JSON.

    Args:
        config: The full config dict to persist.
        path: Destination file path.
    """
    config["meta"]["generated_at"] = datetime.now(timezone.utc).isoformat()
    path.write_text(json.dumps(config, indent=2) + "\n")


def generate(
    output_path: Path = _DEFAULT_OUTPUT, model_id: str = _DEFAULT_MODEL
) -> None:
    """Main orchestrator: fetch language definitions, classify all, save incrementally.

    Args:
        output_path: Where to write cfg_roles.json.
        model_id: Ollama model identifier for classification.
    """
    print(f"Fetching language definitions from {_LANG_DEFS_URL}...")
    lang_defs = requests.get(_LANG_DEFS_URL, timeout=30).json()

    config = _load_existing(output_path)
    model = QwenClassifierModel(model=model_id)

    languages = sorted(lang_defs.keys())
    total = len(languages)

    for i, lang_key in enumerate(languages, start=1):
        if lang_key in config["languages"] or lang_key in config.get("failures", {}):
            print(f"[{i}/{total}] Skipping {lang_key} (already processed)")
            continue

        print(f"[{i}/{total}] Classifying {lang_key}...")
        try:
            lang_def = lang_defs[lang_key]
            url = _build_grammar_url(
                repo_url=lang_def["repo"],
                rev=lang_def["rev"],
                directory=lang_def.get("directory", ""),
            )
            grammar_js = _fetch_grammar_js(url)
            node_specs = _classify_language(grammar_js, model)
            config["languages"][lang_key] = {"node_specs": node_specs}
            print(f"  -> {len(node_specs)} node types classified")
        except Exception as exc:
            config.setdefault("failures", {})[lang_key] = str(exc)
            print(f"  -> FAILED: {exc}")

        _save(config, output_path)

    print(
        f"\nDone. {len(config['languages'])} languages classified, "
        f"{len(config.get('failures', {}))} failures."
    )
    print(f"Output: {output_path}")


def main() -> None:
    """CLI entry point."""
    parser = argparse.ArgumentParser(
        description="Generate cfg_roles.json from tree-sitter grammars"
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=_DEFAULT_OUTPUT,
        help=f"Output path (default: {_DEFAULT_OUTPUT})",
    )
    parser.add_argument(
        "--model",
        default=_DEFAULT_MODEL,
        help=f"Ollama model ID (default: {_DEFAULT_MODEL})",
    )
    args = parser.parse_args()
    generate(output_path=args.output, model_id=args.model)


if __name__ == "__main__":
    main()
