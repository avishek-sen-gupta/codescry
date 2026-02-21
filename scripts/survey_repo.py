"""Run the full survey pipeline on a repository and export concretised signals by validity+direction."""

import argparse
import json
from pathlib import Path

from repo_surveyor import survey
from repo_surveyor.core.pipeline_timer import PipelineTimingObserver
from repo_surveyor.integration_concretiser.types import SignalValidity
from repo_surveyor.integration_patterns import SignalDirection
from repo_surveyor.training.signal_classifier import (
    NullSignalClassifier,
    SignalClassifier,
)
from repo_surveyor.training.types import TrainingLabel


def _signal_to_dict(s, classifier: SignalClassifier) -> dict:
    proba = classifier.predict_proba(s.original_signal.match.line_content)
    # Map (validity, direction) back to TrainingLabel for proba lookup
    _label_key = _validity_direction_to_training_label(s.validity, s.direction)
    ml_confidence = round(proba[_label_key], 4)
    base = s.original_signal.to_dict()
    return {
        "validity": s.validity.value,
        "direction": s.direction.value,
        "ml_confidence": ml_confidence,
        **base,
        "ast_node_type": s.ast_context.node_type,
        "ast_start_line": s.ast_context.start_line,
        "ast_end_line": s.ast_context.end_line,
        "ast_node_text": s.ast_context.node_text,
    }


def _validity_direction_to_training_label(
    validity: SignalValidity, direction: SignalDirection
) -> TrainingLabel:
    """Map (validity, direction) back to TrainingLabel for proba lookup."""
    if validity == SignalValidity.NOISE:
        return TrainingLabel.NOT_DEFINITE
    if direction == SignalDirection.INWARD:
        return TrainingLabel.DEFINITE_INWARD
    if direction == SignalDirection.OUTWARD:
        return TrainingLabel.DEFINITE_OUTWARD
    return TrainingLabel.NOT_DEFINITE


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
    print(f"Classified:     {concretisation.signals_classified}")
    print(f"Unclassified:   {concretisation.signals_unclassified}")
    print()

    print("=== Timings ===")
    for r in timer.completed:
        print(f"  {r.stage:<50s} {r.duration_seconds:>8.2f} s")
    total_s = sum(r.duration_seconds for r in timer.completed if "." not in r.stage)
    print(f"  {'TOTAL':<50s} {total_s:>8.2f} s")
    print()

    out = Path(args.output_dir)
    out.mkdir(parents=True, exist_ok=True)

    _OUTPUT_GROUPS = [
        ("inward", SignalValidity.SIGNAL, SignalDirection.INWARD),
        ("outward", SignalValidity.SIGNAL, SignalDirection.OUTWARD),
        ("ambiguous", SignalValidity.SIGNAL, SignalDirection.AMBIGUOUS),
        ("not_integration", SignalValidity.NOISE, SignalDirection.NOT_INTEGRATION),
        ("noise", SignalValidity.NOISE, None),
    ]

    for filename, validity, direction in _OUTPUT_GROUPS:
        signals = [
            s
            for s in concretisation.concretised
            if s.validity == validity
            and (direction is None or s.direction == direction)
        ]
        path = out / f"{filename}.jsonl"
        path.write_text(
            "\n".join(json.dumps(_signal_to_dict(s, classifier)) for s in signals)
            + "\n",
            encoding="utf-8",
        )
        print(f"  {filename}: {len(signals):4d} signals → {path}")


if __name__ == "__main__":
    main()
