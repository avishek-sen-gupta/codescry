#!/usr/bin/env python3
"""Compare all signal classification techniques and produce charts.

Loads JSONL outputs from every technique run, computes per-category counts
and binary classification metrics (against Gemini ground truth), and
produces a multi-panel comparison chart saved as PNG and SVG.

Signals are keyed by (file_path, line_number).
"""

import json
import logging
from collections import Counter
from dataclasses import dataclass
from pathlib import Path

import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import numpy as np

logger = logging.getLogger(__name__)

DATA_DIR = Path(__file__).parent

_CLASSIFIED_FILES = ["inward.jsonl", "outward.jsonl", "ambiguous.jsonl"]
_NOT_INTEGRATION_FILES = ["not_integration.jsonl", "noise.jsonl"]

_CATEGORY_ORDER = ["INWARD", "OUTWARD", "AMBIGUOUS", "NOT_INTEGRATION"]
_CATEGORY_COLOURS = {
    "INWARD": "#2ecc71",
    "OUTWARD": "#3498db",
    "AMBIGUOUS": "#f39c12",
    "NOT_INTEGRATION": "#e74c3c",
}

_TECHNIQUES: list[tuple[str, Path]] = [
    ("Gemini (GT)", DATA_DIR / "survey_output_gemini"),
    ("Gemini Flash", DATA_DIR / "survey_output_gemini_smojol"),
    ("Claude GT", DATA_DIR / "survey_output_claude_gt"),
    ("Pat-Emb BGE", DATA_DIR / "survey_output_pattern_embedding_bge_smojol"),
    (
        "FW-Gate+Gemini k5",
        DATA_DIR
        / "survey_output"
        / "fw_pattern_gate_gemini_flash_bge_BAAI--bge-base-en-v1.5_k5_t0.68_20260227T072254",
    ),
    (
        "FW-Gate+Gemini k1",
        DATA_DIR
        / "survey_output"
        / "fw_pattern_gate_gemini_flash_bge_BAAI--bge-base-en-v1.5_k1_t0.68_20260227T074029",
    ),
]


@dataclass(frozen=True)
class TechniqueSignals:
    name: str
    signals: dict[tuple[str, int], dict]
    category_counts: Counter


@dataclass(frozen=True)
class BinaryMetrics:
    tp: int
    fp: int
    tn: int
    fn: int
    precision: float
    recall: float
    f1: float
    accuracy: float


def _load_signals(directory: Path) -> dict[tuple[str, int], dict]:
    """Load signals from a technique output directory, keyed by (file_path, line_number)."""
    signals: dict[tuple[str, int], dict] = {}

    category_map = {
        "inward.jsonl": "INWARD",
        "outward.jsonl": "OUTWARD",
        "ambiguous.jsonl": "AMBIGUOUS",
        "not_integration.jsonl": "NOT_INTEGRATION",
        "noise.jsonl": "NOT_INTEGRATION",
    }

    for fname in _CLASSIFIED_FILES + _NOT_INTEGRATION_FILES:
        fpath = directory / fname
        if not fpath.exists():
            continue
        for line in fpath.read_text().strip().split("\n"):
            if not line.strip():
                continue
            record = json.loads(line)
            key = (record["file_path"], record["line_number"])
            if key not in signals:
                record["_category"] = category_map[fname]
                record["_is_integration"] = fname in _CLASSIFIED_FILES
                signals[key] = record

    return signals


def _count_categories(signals: dict[tuple[str, int], dict]) -> Counter:
    """Count signals per category."""
    return Counter(rec["_category"] for rec in signals.values())


def _load_technique(name: str, directory: Path) -> TechniqueSignals:
    """Load and summarise a single technique."""
    logger.info("Loading %s from %s", name, directory)
    signals = _load_signals(directory)
    counts = _count_categories(signals)
    logger.info(
        "  %s: %d signals (%s)",
        name,
        len(signals),
        ", ".join(f"{k}={v}" for k, v in sorted(counts.items())),
    )
    return TechniqueSignals(name=name, signals=signals, category_counts=counts)


def _compute_binary_metrics(
    gt: dict[tuple[str, int], dict],
    pred: dict[tuple[str, int], dict],
) -> BinaryMetrics:
    """Compute binary classification metrics (integration vs not) against ground truth."""
    common_keys = set(gt) & set(pred)
    tp = sum(
        1
        for k in common_keys
        if gt[k]["_is_integration"] and pred[k]["_is_integration"]
    )
    fp = sum(
        1
        for k in common_keys
        if not gt[k]["_is_integration"] and pred[k]["_is_integration"]
    )
    tn = sum(
        1
        for k in common_keys
        if not gt[k]["_is_integration"] and not pred[k]["_is_integration"]
    )
    fn = sum(
        1
        for k in common_keys
        if gt[k]["_is_integration"] and not pred[k]["_is_integration"]
    )
    total = tp + fp + tn + fn
    precision = tp / (tp + fp) if (tp + fp) > 0 else 0.0
    recall = tp / (tp + fn) if (tp + fn) > 0 else 0.0
    f1 = (
        2 * precision * recall / (precision + recall)
        if (precision + recall) > 0
        else 0.0
    )
    accuracy = (tp + tn) / total if total > 0 else 0.0
    return BinaryMetrics(
        tp=tp,
        fp=fp,
        tn=tn,
        fn=fn,
        precision=precision,
        recall=recall,
        f1=f1,
        accuracy=accuracy,
    )


def _compute_exact_match(
    gt: dict[tuple[str, int], dict],
    pred: dict[tuple[str, int], dict],
) -> float:
    """Compute exact category match rate against ground truth."""
    common_keys = set(gt) & set(pred)
    if not common_keys:
        return 0.0
    matches = sum(1 for k in common_keys if gt[k]["_category"] == pred[k]["_category"])
    return matches / len(common_keys)


def _plot_category_distribution(
    ax: plt.Axes,
    techniques: list[TechniqueSignals],
) -> None:
    """Plot grouped bar chart of category counts per technique."""
    x = np.arange(len(techniques))
    bar_width = 0.18
    offsets = np.arange(len(_CATEGORY_ORDER)) - (len(_CATEGORY_ORDER) - 1) / 2

    for i, cat in enumerate(_CATEGORY_ORDER):
        values = [t.category_counts.get(cat, 0) for t in techniques]
        ax.bar(
            x + offsets[i] * bar_width,
            values,
            bar_width,
            label=cat,
            color=_CATEGORY_COLOURS[cat],
            edgecolor="white",
            linewidth=0.5,
        )

    ax.set_xlabel("Technique")
    ax.set_ylabel("Signal Count")
    ax.set_title("Category Distribution by Technique")
    ax.set_xticks(x)
    ax.set_xticklabels(
        [t.name for t in techniques], rotation=25, ha="right", fontsize=8
    )
    ax.legend(fontsize=7)
    ax.yaxis.set_major_locator(mticker.MaxNLocator(integer=True))


def _plot_binary_metrics(
    ax: plt.Axes,
    names: list[str],
    metrics: list[BinaryMetrics],
) -> None:
    """Plot grouped bar chart of precision, recall, F1, accuracy."""
    x = np.arange(len(names))
    bar_width = 0.2
    metric_names = ["Precision", "Recall", "F1", "Accuracy"]
    colours = ["#1abc9c", "#9b59b6", "#e67e22", "#34495e"]

    for i, (metric_name, colour) in enumerate(zip(metric_names, colours)):
        values = [
            getattr(m, metric_name.lower()) if metric_name != "F1" else m.f1
            for m in metrics
        ]
        offset = (i - 1.5) * bar_width
        bars = ax.bar(x + offset, values, bar_width, label=metric_name, color=colour)
        for bar, val in zip(bars, values):
            if val > 0:
                ax.text(
                    bar.get_x() + bar.get_width() / 2,
                    bar.get_height() + 0.01,
                    f"{val:.0%}",
                    ha="center",
                    va="bottom",
                    fontsize=5,
                    rotation=90,
                )

    ax.set_xlabel("Technique")
    ax.set_ylabel("Score")
    ax.set_title("Binary Classification Metrics (vs Gemini GT)")
    ax.set_xticks(x)
    ax.set_xticklabels(names, rotation=25, ha="right", fontsize=8)
    ax.set_ylim(0, 1.15)
    ax.legend(fontsize=7)
    ax.yaxis.set_major_formatter(mticker.PercentFormatter(1.0))


def _plot_exact_match(
    ax: plt.Axes,
    names: list[str],
    exact_matches: list[float],
) -> None:
    """Plot horizontal bar chart of exact-match accuracy."""
    y = np.arange(len(names))
    colours = [
        "#2ecc71" if v >= 0.7 else "#f39c12" if v >= 0.4 else "#e74c3c"
        for v in exact_matches
    ]
    bars = ax.barh(y, exact_matches, color=colours, edgecolor="white", linewidth=0.5)

    for bar, val in zip(bars, exact_matches):
        ax.text(
            bar.get_width() + 0.01,
            bar.get_y() + bar.get_height() / 2,
            f"{val:.1%}",
            ha="left",
            va="center",
            fontsize=8,
            fontweight="bold",
        )

    ax.set_yticks(y)
    ax.set_yticklabels(names, fontsize=8)
    ax.set_xlabel("Exact-Match Accuracy")
    ax.set_title("4-Category Exact-Match Accuracy (vs Gemini GT)")
    ax.set_xlim(0, 1.15)
    ax.xaxis.set_major_formatter(mticker.PercentFormatter(1.0))
    ax.invert_yaxis()


def _plot_stacked_proportions(
    ax: plt.Axes,
    techniques: list[TechniqueSignals],
) -> None:
    """Plot stacked bar chart showing category proportions per technique."""
    names = [t.name for t in techniques]
    totals = [sum(t.category_counts.values()) for t in techniques]

    bottom = np.zeros(len(techniques))
    for cat in _CATEGORY_ORDER:
        proportions = [
            t.category_counts.get(cat, 0) / total if total > 0 else 0
            for t, total in zip(techniques, totals)
        ]
        ax.bar(
            names,
            proportions,
            bottom=bottom,
            label=cat,
            color=_CATEGORY_COLOURS[cat],
            edgecolor="white",
            linewidth=0.5,
        )
        bottom += np.array(proportions)

    ax.set_ylabel("Proportion")
    ax.set_title("Category Proportions by Technique")
    ax.set_xticklabels(names, rotation=25, ha="right", fontsize=8)
    ax.legend(fontsize=7, loc="upper right")
    ax.yaxis.set_major_formatter(mticker.PercentFormatter(1.0))


def main() -> None:
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s %(levelname)-8s %(name)s: %(message)s",
    )

    # --- Load all techniques ---
    techniques = [
        _load_technique(name, directory)
        for name, directory in _TECHNIQUES
        if directory.exists()
    ]

    if len(techniques) < 2:
        logger.error("Need at least 2 techniques to compare, found %d", len(techniques))
        return

    logger.info("Loaded %d techniques", len(techniques))

    # --- Compute metrics against ground truth (first technique = Gemini GT) ---
    gt = techniques[0]
    comparison_techniques = techniques[1:]

    binary_metrics = [
        _compute_binary_metrics(gt.signals, t.signals) for t in comparison_techniques
    ]
    exact_matches = [
        _compute_exact_match(gt.signals, t.signals) for t in comparison_techniques
    ]
    comparison_names = [t.name for t in comparison_techniques]

    # --- Print summary ---
    print(f"\n{'=' * 80}")
    print("ALL TECHNIQUES COMPARISON")
    print(f"{'=' * 80}")
    print(f"\nGround Truth: {gt.name} ({sum(gt.category_counts.values())} signals)")
    print()

    header = f"{'Technique':<25s} {'Total':>6s} {'INWARD':>7s} {'OUTWARD':>8s} {'AMBIG':>6s} {'NOT_INT':>8s}"
    print(header)
    print("-" * len(header))
    for t in techniques:
        total = sum(t.category_counts.values())
        print(
            f"{t.name:<25s} {total:>6d} "
            f"{t.category_counts.get('INWARD', 0):>7d} "
            f"{t.category_counts.get('OUTWARD', 0):>8d} "
            f"{t.category_counts.get('AMBIGUOUS', 0):>6d} "
            f"{t.category_counts.get('NOT_INTEGRATION', 0):>8d}"
        )

    print(f"\n{'--- Binary Metrics (vs Gemini GT) ---':^80s}")
    header2 = f"{'Technique':<25s} {'Prec':>7s} {'Recall':>7s} {'F1':>7s} {'Acc':>7s} {'ExMatch':>8s}"
    print(header2)
    print("-" * len(header2))
    for name, bm, em in zip(comparison_names, binary_metrics, exact_matches):
        print(
            f"{name:<25s} {bm.precision:>6.1%} {bm.recall:>6.1%} "
            f"{bm.f1:>6.1%} {bm.accuracy:>6.1%} {em:>7.1%}"
        )

    # --- Plot ---
    fig, axes = plt.subplots(2, 2, figsize=(16, 12))
    fig.suptitle(
        "Signal Classification: All Techniques Comparison",
        fontsize=14,
        fontweight="bold",
    )

    _plot_category_distribution(axes[0, 0], techniques)
    _plot_stacked_proportions(axes[0, 1], techniques)
    _plot_binary_metrics(axes[1, 0], comparison_names, binary_metrics)
    _plot_exact_match(axes[1, 1], comparison_names, exact_matches)

    fig.tight_layout(rect=[0, 0, 1, 0.95])

    output_png = DATA_DIR / "all_techniques_comparison.png"
    output_svg = DATA_DIR / "all_techniques_comparison.svg"
    fig.savefig(output_png, dpi=150, bbox_inches="tight")
    fig.savefig(output_svg, bbox_inches="tight")
    logger.info("Charts saved to %s and %s", output_png, output_svg)
    print(f"\nCharts saved to:\n  {output_png}\n  {output_svg}")
    plt.close(fig)


if __name__ == "__main__":
    main()
