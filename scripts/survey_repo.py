"""Run the full survey pipeline on a repository and export concretised signals by label."""

import argparse
import json
from pathlib import Path

from repo_surveyor import survey
from repo_surveyor.pipeline_timer import PipelineTimingObserver
from repo_surveyor.training.signal_classifier import NullSignalClassifier, SignalClassifier
from repo_surveyor.training.types import TrainingLabel


def _signal_to_dict(s) -> dict:
    return {
        "label": s.label.value,
        "integration_type": s.original_signal.integration_type.value,
        "confidence": s.original_signal.confidence.value,
        "line_content": s.original_signal.match.line_content.strip(),
        "file_path": s.original_signal.match.file_path,
        "line_number": s.original_signal.match.line_number,
        "matched_pattern": s.original_signal.matched_pattern,
        "source": s.original_signal.source,
        "ast_node_type": s.ast_context.node_type,
        "ast_start_line": s.ast_context.start_line,
        "ast_end_line": s.ast_context.end_line,
        "ast_node_text": s.ast_context.node_text,
    }


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("repo_path", help="Path to the repository to analyse")
    parser.add_argument(
        "--languages",
        nargs="+",
        default=[],
        metavar="LANG",
        help="Languages to filter (e.g. Java Python). Defaults to all.",
    )
    parser.add_argument(
        "--model",
        default="data/training/signal_classifier.joblib",
        metavar="PATH",
        help="Path to trained SignalClassifier joblib. Omit to use NullSignalClassifier.",
    )
    parser.add_argument(
        "--output-dir",
        default="data/survey_output",
        metavar="DIR",
        help="Directory to write per-label JSONL files.",
    )
    return parser.parse_args()


def main() -> None:
    args = _parse_args()

    model_path = Path(args.model)
    classifier = (
        SignalClassifier.load(model_path)
        if model_path.exists()
        else NullSignalClassifier()
    )

    print(f"Repo:      {args.repo_path}")
    print(f"Languages: {args.languages or 'all'}")
    print(f"Model:     {classifier.model_id}")
    print()

    timer = PipelineTimingObserver()
    _, _, integration, _, concretisation = survey(
        args.repo_path,
        languages=args.languages,
        classifier=classifier,
        timer=timer,
    )

    print(f"Files scanned:  {integration.files_scanned}")
    print(f"Signals found:  {len(integration.integration_points)}")
    print()
    print(f"Submitted:      {concretisation.signals_submitted}")
    print(f"Definite:       {concretisation.signals_definite}")
    print(f"Discarded:      {concretisation.signals_discarded}")
    print()

    print("=== Timings ===")
    for r in timer.completed:
        print(f"  {r.stage:<50s} {r.duration_seconds:>8.2f} s")
    total_s = sum(r.duration_seconds for r in timer.completed if "." not in r.stage)
    print(f"  {'TOTAL':<50s} {total_s:>8.2f} s")
    print()

    out = Path(args.output_dir)
    out.mkdir(parents=True, exist_ok=True)

    for label in TrainingLabel:
        signals = [s for s in concretisation.concretised if s.label == label]
        path = out / f"{label.value.lower()}.jsonl"
        path.write_text(
            "\n".join(json.dumps(_signal_to_dict(s)) for s in signals) + "\n",
            encoding="utf-8",
        )
        print(f"  {label.value}: {len(signals):4d} signals → {path}")


if __name__ == "__main__":
    main()
