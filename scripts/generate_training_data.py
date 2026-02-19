#!/usr/bin/env python3
"""Generate labelled training data for the integration signal classifier.

Orchestrates: coverage matrix -> generation -> validation -> export.

Usage:
    poetry run python scripts/generate_training_data.py
    poetry run python scripts/generate_training_data.py --languages Java Python --examples-per-triple 2
    poetry run python scripts/generate_training_data.py --languages Java --integration-types http_rest database
    poetry run python scripts/generate_training_data.py --dry-run
"""

import argparse
import json
import logging
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from repo_surveyor.integration_patterns.types import IntegrationType, Language
from repo_surveyor.training.coverage import build_coverage_matrix
from repo_surveyor.training.exporter import export_training_data
from repo_surveyor.training.generator import (
    generate_all,
    generate_all_batch,
    _load_batch_checkpoint,
)
from repo_surveyor.training.types import TrainingLabel
from repo_surveyor.training.validator import validate_batch

_DEFAULT_OUTPUT_DIR = Path("data/training")
_DEFAULT_EXAMPLES_PER_TRIPLE = 5


def _parse_languages(names: list[str]) -> list[Language]:
    """Resolve language names to Language enum members."""
    result: list[Language] = []
    for name in names:
        lang = Language.from_name(name)
        if lang is None:
            valid = [l.value for l in Language]
            print(f"Unknown language: {name!r}. Valid: {valid}", file=sys.stderr)
            sys.exit(1)
        result.append(lang)
    return result


def _parse_integration_types(names: list[str]) -> list[IntegrationType]:
    """Resolve integration type names to IntegrationType enum members."""
    result: list[IntegrationType] = []
    for name in names:
        try:
            result.append(IntegrationType(name))
        except ValueError:
            valid = [t.value for t in IntegrationType]
            print(
                f"Unknown integration type: {name!r}. Valid: {valid}", file=sys.stderr
            )
            sys.exit(1)
    return result


def _build_parser() -> argparse.ArgumentParser:
    """Build the CLI argument parser."""
    parser = argparse.ArgumentParser(
        description="Generate labelled training data for the integration classifier."
    )
    parser.add_argument(
        "--languages",
        nargs="+",
        default=[],
        help="Languages to generate for (default: all with patterns).",
    )
    parser.add_argument(
        "--integration-types",
        nargs="+",
        default=[],
        help="Integration types to generate for (default: all with patterns).",
    )
    parser.add_argument(
        "--examples-per-triple",
        type=int,
        default=_DEFAULT_EXAMPLES_PER_TRIPLE,
        help=f"Examples per (lang, type, label) triple (default: {_DEFAULT_EXAMPLES_PER_TRIPLE}).",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=_DEFAULT_OUTPUT_DIR,
        help=f"Output directory (default: {_DEFAULT_OUTPUT_DIR}).",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show coverage matrix without generating examples.",
    )
    parser.add_argument(
        "--model",
        default="claude-sonnet-4-20250514",
        help="Claude model ID to use for generation.",
    )
    parser.add_argument(
        "--api-key",
        default="",
        help="Anthropic API key (default: from ANTHROPIC_API_KEY env var).",
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=42,
        help="Random seed for train/val/test split (default: 42).",
    )
    parser.add_argument(
        "--batch",
        action="store_true",
        help="Use the Anthropic Batches API (async, 50%% cost savings).",
    )
    parser.add_argument(
        "--batch-status",
        action="store_true",
        help="Check the status of a previously submitted batch and exit.",
    )
    parser.add_argument(
        "--resume",
        action="store_true",
        help="Resume from checkpoint file, skipping already-completed triples.",
    )
    parser.add_argument(
        "--verbose",
        "-v",
        action="store_true",
        help="Enable verbose logging.",
    )
    return parser


def _handle_batch_status(args: argparse.Namespace) -> None:
    """Check and print the status of a previously submitted batch."""
    batch_checkpoint_path = args.output_dir / "batch_checkpoint.json"
    batch_id = _load_batch_checkpoint(batch_checkpoint_path)

    if not batch_id:
        print(
            f"No batch checkpoint found at {batch_checkpoint_path}",
            file=sys.stderr,
        )
        sys.exit(1)

    from repo_surveyor.ml_classifier.claude_model import ClaudeClassifierModel

    model = ClaudeClassifierModel(model=args.model, api_key=args.api_key)
    status = model.batch_status(batch_id)

    total = (
        status.succeeded
        + status.errored
        + status.expired
        + status.processing
        + status.canceled
    )
    print(f"\nBatch: {status.batch_id}")
    print(f"Status: {status.processing_status}")
    print(f"  Total requests: {total}")
    print(f"  Processing:     {status.processing}")
    print(f"  Succeeded:      {status.succeeded}")
    print(f"  Errored:        {status.errored}")
    print(f"  Expired:        {status.expired}")
    print(f"  Canceled:       {status.canceled}")


def main() -> None:
    """Run the training data generation pipeline."""
    args = _build_parser().parse_args()

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(asctime)s %(levelname)s %(name)s: %(message)s",
    )

    if args.batch_status:
        _handle_batch_status(args)
        return

    languages = _parse_languages(args.languages) if args.languages else []
    integration_types = (
        _parse_integration_types(args.integration_types)
        if args.integration_types
        else []
    )

    # Step 1: Build coverage matrix
    matrix = build_coverage_matrix(
        languages=languages,
        integration_types=integration_types,
    )

    print(f"\nCoverage matrix: {len(matrix.entries)} valid (language, type) pairs")
    print(f"  Languages covered: {matrix.languages_covered}")
    print(f"  Integration types covered: {matrix.integration_types_covered}")
    print(f"  Total triples (x3 labels): {matrix.total_triples}")
    print(
        f"  Estimated examples: {matrix.total_triples // len(TrainingLabel) * args.examples_per_triple * len(TrainingLabel)}"
    )

    # Write coverage report
    args.output_dir.mkdir(parents=True, exist_ok=True)
    coverage_path = args.output_dir / "coverage.json"
    with coverage_path.open("w", encoding="utf-8") as f:
        json.dump(matrix.to_dict(), f, indent=2)
    print(f"\nCoverage report written to {coverage_path}")

    if args.dry_run:
        if args.batch:
            print(
                "\nDry run (batch mode) — would submit 1 batch with "
                f"{matrix.total_triples} requests."
            )
        else:
            print("\nDry run — skipping generation.")
        return

    # Step 2: Generate examples
    from repo_surveyor.ml_classifier.claude_model import ClaudeClassifierModel

    model = ClaudeClassifierModel(model=args.model, api_key=args.api_key)

    if args.batch:
        batch_checkpoint_path = args.output_dir / "batch_checkpoint.json"
        results = generate_all_batch(
            model=model,
            entries=list(matrix.entries),
            examples_per_triple=args.examples_per_triple,
            batch_checkpoint_path=batch_checkpoint_path,
            resume=args.resume,
        )
    else:
        checkpoint_path = args.output_dir / "checkpoint.jsonl"
        results = generate_all(
            model=model,
            entries=list(matrix.entries),
            examples_per_triple=args.examples_per_triple,
            checkpoint_path=checkpoint_path,
            resume=args.resume,
        )

    all_examples = [ex for r in results for ex in r.examples]
    total_prompt_tokens = sum(r.prompt_tokens for r in results)
    total_completion_tokens = sum(r.completion_tokens for r in results)

    print(f"\nGenerated {len(all_examples)} raw examples")
    print(
        f"  Token usage: {total_prompt_tokens} prompt, {total_completion_tokens} completion"
    )

    # Step 3: Validate
    validation = validate_batch(all_examples)

    print(f"\nValidation: {validation.valid}/{validation.total} passed")
    if validation.invalid > 0:
        print(f"  {validation.invalid} examples failed validation")

    # Step 4: Export
    export_result = export_training_data(
        examples=list(validation.valid_examples),
        output_dir=args.output_dir,
        seed=args.seed,
    )

    print(f"\nExported to {export_result.output_dir}:")
    print(f"  Total: {export_result.total_examples}")
    print(f"  Train: {export_result.train_count}")
    print(f"  Val:   {export_result.val_count}")
    print(f"  Test:  {export_result.test_count}")


if __name__ == "__main__":
    main()
