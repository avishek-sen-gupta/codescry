#!/usr/bin/env python3
"""Three-way comparison of signal classifications: Gemini, BGE, and Claude.

Loads all three ground truths and produces:
- Three-way confusion matrices (binary: integration vs not)
- Pairwise agreement rates
- Signals where all three agree vs disagree
- Per-integration-type breakdown
- TSV file with all signals and all three classifications
"""

import json
import logging
from collections import Counter
from pathlib import Path

logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")
logger = logging.getLogger(__name__)

DATA_DIR = Path(__file__).parent

GEMINI_DIR = DATA_DIR / "survey_output_gemini_smojol"
BGE_DIR = DATA_DIR / "survey_output_pattern_embedding_bge_smojol"
CLAUDE_DIR = DATA_DIR / "survey_output_claude_gt"

INTEGRATION_FILES = ["inward.jsonl", "outward.jsonl", "ambiguous.jsonl"]
NOT_INTEGRATION_FILES = ["not_integration.jsonl", "noise.jsonl"]

REPORT_PATH = DATA_DIR / "three_way_comparison_report.txt"
TSV_PATH = DATA_DIR / "three_way_comparison.tsv"


def load_binary_signals(directory: Path) -> dict[tuple[str, int], dict]:
    """Load signals keyed by (file_path, line_number) with _is_integration flag."""
    signals: dict[tuple[str, int], dict] = {}

    for fname in INTEGRATION_FILES:
        fpath = directory / fname
        if not fpath.exists():
            continue
        for line in fpath.read_text().strip().split("\n"):
            if not line.strip():
                continue
            record = json.loads(line)
            key = (record["file_path"], record["line_number"])
            record["_is_integration"] = True
            record["_source_file"] = fname
            record["_direction"] = fname.replace(".jsonl", "")
            signals[key] = record

    for fname in NOT_INTEGRATION_FILES:
        fpath = directory / fname
        if not fpath.exists():
            continue
        for line in fpath.read_text().strip().split("\n"):
            if not line.strip():
                continue
            record = json.loads(line)
            key = (record["file_path"], record["line_number"])
            if key not in signals:
                record["_is_integration"] = False
                record["_source_file"] = fname
                record["_direction"] = "not_integration"
                signals[key] = record

    return signals


def fmt_pct(val: float) -> str:
    return f"{val * 100:.1f}%"


def compute_binary_metrics(
    gt_labels: dict[tuple[str, int], bool],
    pred_labels: dict[tuple[str, int], bool],
    keys: set[tuple[str, int]],
) -> dict:
    """Compute TP, FP, FN, TN, precision, recall, F1, accuracy."""
    tp = sum(1 for k in keys if gt_labels.get(k) and pred_labels.get(k))
    fp = sum(1 for k in keys if not gt_labels.get(k) and pred_labels.get(k))
    fn = sum(1 for k in keys if gt_labels.get(k) and not pred_labels.get(k))
    tn = sum(1 for k in keys if not gt_labels.get(k) and not pred_labels.get(k))
    total = tp + fp + fn + tn
    precision = tp / (tp + fp) if (tp + fp) > 0 else 0.0
    recall = tp / (tp + fn) if (tp + fn) > 0 else 0.0
    f1 = (
        2 * precision * recall / (precision + recall)
        if (precision + recall) > 0
        else 0.0
    )
    accuracy = (tp + tn) / total if total > 0 else 0.0
    return {
        "tp": tp,
        "fp": fp,
        "fn": fn,
        "tn": tn,
        "precision": precision,
        "recall": recall,
        "f1": f1,
        "accuracy": accuracy,
        "total": total,
    }


def format_confusion_matrix(metrics: dict, gt_name: str, pred_name: str) -> str:
    """Format a binary confusion matrix as a string."""
    lines = [
        f"  {pred_name:>30s}: Integration   {pred_name}: Not Integration",
        f"  {gt_name}: Integration       {metrics['tp']:>6} (TP)          {metrics['fn']:>6} (FN)",
        f"  {gt_name}: Not Integration   {metrics['fp']:>6} (FP)          {metrics['tn']:>6} (TN)",
        "",
        f"  Accuracy:   {fmt_pct(metrics['accuracy'])}  ({metrics['tp'] + metrics['tn']}/{metrics['total']})",
        f"  Precision:  {fmt_pct(metrics['precision'])}  ({metrics['tp']}/{metrics['tp'] + metrics['fp']})",
        f"  Recall:     {fmt_pct(metrics['recall'])}  ({metrics['tp']}/{metrics['tp'] + metrics['fn']})",
        f"  F1 Score:   {metrics['f1']:.3f}",
    ]
    return "\n".join(lines)


def write_tsv(
    all_keys: list[tuple[str, int]],
    gemini: dict[tuple[str, int], dict],
    bge: dict[tuple[str, int], dict],
    claude: dict[tuple[str, int], dict],
    output_path: Path,
) -> None:
    """Write full TSV with all signals and all three classifications."""
    header = "\t".join(
        [
            "file_path",
            "line_number",
            "line_content",
            "integration_type",
            "gemini_is_integration",
            "gemini_direction",
            "gemini_reason",
            "bge_is_integration",
            "bge_direction",
            "bge_nearest_score",
            "claude_is_integration",
            "claude_direction",
            "claude_reason",
            "all_agree",
            "agreement_pattern",
        ]
    )

    rows = [header]
    for key in sorted(all_keys):
        file_path, line_number = key
        g = gemini.get(key, {})
        b = bge.get(key, {})
        c = claude.get(key, {})

        g_is = g.get("_is_integration", False) if g else False
        b_is = b.get("_is_integration", False) if b else False
        c_is = c.get("_is_integration", False) if c else False

        match_details = (
            g.get("match_details", [])
            or b.get("match_details", [])
            or c.get("match_details", [])
        )
        integration_type = (
            match_details[0].get("integration_type", "") if match_details else ""
        )

        g_dir = g.get("_direction", "?") if g else "?"
        b_dir = b.get("_direction", "?") if b else "?"
        c_dir = c.get("_direction", "?") if c else "?"

        pattern = (
            f"{'I' if g_is else 'N'}-{'I' if b_is else 'N'}-{'I' if c_is else 'N'}"
        )
        all_agree = g_is == b_is == c_is

        line_content = (
            (
                g.get("line_content", "")
                or b.get("line_content", "")
                or c.get("line_content", "")
            )
            .replace("\t", " ")
            .replace("\n", " ")[:200]
        )

        row = "\t".join(
            [
                file_path,
                str(line_number),
                line_content,
                integration_type,
                str(g_is),
                g_dir,
                (
                    (g.get("gemini_reason", "") or "")[:100].replace("\t", " ")
                    if g
                    else ""
                ),
                str(b_is),
                b_dir,
                f"{b.get('nearest_score', 0):.3f}" if b else "",
                str(c_is),
                c_dir,
                c.get("claude_reason", "")[:100].replace("\t", " ") if c else "",
                str(all_agree),
                pattern,
            ]
        )
        rows.append(row)

    output_path.write_text("\n".join(rows) + "\n")
    logger.info("Wrote TSV with %d signals to %s", len(rows) - 1, output_path)


def main() -> None:
    gemini = load_binary_signals(GEMINI_DIR)
    bge = load_binary_signals(BGE_DIR)
    claude = load_binary_signals(CLAUDE_DIR)

    logger.info(
        "Loaded: Gemini=%d, BGE=%d, Claude=%d", len(gemini), len(bge), len(claude)
    )

    all_keys = sorted(set(gemini.keys()) | set(bge.keys()) | set(claude.keys()))
    common_keys = set(gemini.keys()) & set(bge.keys()) & set(claude.keys())

    g_labels = {k: v.get("_is_integration", False) for k, v in gemini.items()}
    b_labels = {k: v.get("_is_integration", False) for k, v in bge.items()}
    c_labels = {k: v.get("_is_integration", False) for k, v in claude.items()}

    report_lines: list[str] = []

    def emit(line: str = "") -> None:
        report_lines.append(line)

    emit("=" * 72)
    emit("THREE-WAY SIGNAL CLASSIFICATION COMPARISON")
    emit("Gemini LLM vs BGE Embeddings vs Claude Rule-Based")
    emit("=" * 72)
    emit()

    # Dataset sizes
    g_pos = sum(1 for v in g_labels.values() if v)
    g_neg = len(g_labels) - g_pos
    b_pos = sum(1 for v in b_labels.values() if v)
    b_neg = len(b_labels) - b_pos
    c_pos = sum(1 for v in c_labels.values() if v)
    c_neg = len(c_labels) - c_pos

    emit(
        f"Gemini:  {g_pos:>4} integration, {g_neg:>4} not_integration, {len(gemini):>4} total"
    )
    emit(
        f"BGE:     {b_pos:>4} integration, {b_neg:>4} not_integration, {len(bge):>4} total"
    )
    emit(
        f"Claude:  {c_pos:>4} integration, {c_neg:>4} not_integration, {len(claude):>4} total"
    )
    emit(f"Common:  {len(common_keys):>4} signals in all three")
    emit(f"Union:   {len(all_keys):>4} signals total")
    emit()

    # Pairwise comparisons
    pairs = [
        ("Gemini", "BGE", g_labels, b_labels),
        ("Gemini", "Claude", g_labels, c_labels),
        ("BGE", "Claude", b_labels, c_labels),
    ]

    for gt_name, pred_name, gt_lab, pred_lab in pairs:
        overlap = set(gt_lab.keys()) & set(pred_lab.keys())
        emit("=" * 72)
        emit(f"PAIRWISE: {gt_name} vs {pred_name} ({len(overlap)} overlapping signals)")
        emit("=" * 72)
        emit()

        metrics = compute_binary_metrics(gt_lab, pred_lab, overlap)
        emit(format_confusion_matrix(metrics, gt_name, pred_name))

        agree = sum(
            1 for k in overlap if gt_lab.get(k, False) == pred_lab.get(k, False)
        )
        emit()
        emit(
            f"  Agreement rate: {fmt_pct(agree / len(overlap))} ({agree}/{len(overlap)})"
        )
        emit()

    # Three-way agreement on common signals
    emit("=" * 72)
    emit(f"THREE-WAY AGREEMENT ({len(common_keys)} signals)")
    emit("=" * 72)
    emit()

    pattern_counts: Counter = Counter()
    for key in common_keys:
        g = g_labels.get(key, False)
        b = b_labels.get(key, False)
        c = c_labels.get(key, False)
        pattern = f"{'I' if g else 'N'}-{'I' if b else 'N'}-{'I' if c else 'N'}"
        pattern_counts[pattern] += 1

    all_agree_count = pattern_counts.get("I-I-I", 0) + pattern_counts.get("N-N-N", 0)
    two_agree_count = sum(
        v for k, v in pattern_counts.items() if k not in ("I-I-I", "N-N-N")
    )

    emit(
        f"  All three agree:     {all_agree_count:>4} ({fmt_pct(all_agree_count / len(common_keys))})"
    )
    emit(
        f"  Two agree, one not:  {two_agree_count:>4} ({fmt_pct(two_agree_count / len(common_keys))})"
    )
    emit()
    emit("  Pattern (Gemini-BGE-Claude)  Count")
    emit("  " + "-" * 40)
    for pattern, count in pattern_counts.most_common():
        labels = {
            "I-I-I": "All: Integration",
            "N-N-N": "All: Not Integration",
            "I-I-N": "Gemini+BGE=I, Claude=N",
            "I-N-I": "Gemini+Claude=I, BGE=N",
            "N-I-I": "BGE+Claude=I, Gemini=N",
            "I-N-N": "Only Gemini=I",
            "N-I-N": "Only BGE=I",
            "N-N-I": "Only Claude=I",
        }
        label = labels.get(pattern, pattern)
        emit(f"  {pattern:<10s} {count:>4}  {label}")
    emit()

    # Per-integration-type breakdown
    emit("=" * 72)
    emit("PER-INTEGRATION-TYPE BREAKDOWN")
    emit("=" * 72)
    emit()

    type_patterns: dict[str, Counter] = {}
    for key in common_keys:
        rec = gemini.get(key, {}) or bge.get(key, {}) or claude.get(key, {})
        match_details = rec.get("match_details", [])
        itype = (
            match_details[0].get("integration_type", "unknown")
            if match_details
            else "unknown"
        )

        g = g_labels.get(key, False)
        b = b_labels.get(key, False)
        c = c_labels.get(key, False)
        pattern = f"{'I' if g else 'N'}-{'I' if b else 'N'}-{'I' if c else 'N'}"

        if itype not in type_patterns:
            type_patterns[itype] = Counter()
        type_patterns[itype][pattern] += 1

    for itype in sorted(type_patterns.keys()):
        counts = type_patterns[itype]
        total = sum(counts.values())
        all_agree = counts.get("I-I-I", 0) + counts.get("N-N-N", 0)
        emit(f"  {itype} ({total} signals, {fmt_pct(all_agree / total)} agreement):")
        for pattern, count in counts.most_common():
            emit(f"    {pattern}: {count}")
    emit()

    # Disagreement examples
    emit("=" * 72)
    emit("NOTABLE DISAGREEMENTS (Claude vs Gemini)")
    emit("=" * 72)
    emit()

    claude_reclassified: list[tuple[tuple[str, int], dict, dict]] = []
    for key in common_keys:
        g_is = g_labels.get(key, False)
        c_is = c_labels.get(key, False)
        if g_is != c_is:
            claude_reclassified.append((key, gemini[key], claude[key]))

    emit(
        f"  Claude reclassified {len(claude_reclassified)} signals differently from Gemini"
    )
    emit()

    claude_demoted = [
        (k, g, c)
        for k, g, c in claude_reclassified
        if g.get("_is_integration") and not c.get("_is_integration")
    ]
    claude_promoted = [
        (k, g, c)
        for k, g, c in claude_reclassified
        if not g.get("_is_integration") and c.get("_is_integration")
    ]

    emit(f"  Gemini=I, Claude=N (demoted): {len(claude_demoted)}")
    for key, g, c in sorted(claude_demoted, key=lambda x: x[0])[:10]:
        short = Path(key[0]).name
        emit(f"    {short}:{key[1]}  reason: {c.get('claude_reason', '?')}")
        emit(f"      line: {g.get('line_content', '')[:100]}")
    if len(claude_demoted) > 10:
        emit(f"    ... and {len(claude_demoted) - 10} more")
    emit()

    emit(f"  Gemini=N, Claude=I (promoted): {len(claude_promoted)}")
    for key, g, c in sorted(claude_promoted, key=lambda x: x[0])[:10]:
        short = Path(key[0]).name
        emit(f"    {short}:{key[1]}  reason: {c.get('claude_reason', '?')}")
        emit(f"      line: {g.get('line_content', '')[:100]}")
    if len(claude_promoted) > 10:
        emit(f"    ... and {len(claude_promoted) - 10} more")
    emit()

    # Claude reason distribution
    emit("=" * 72)
    emit("CLAUDE CLASSIFICATION REASON DISTRIBUTION")
    emit("=" * 72)
    emit()

    reason_counts: Counter = Counter()
    for key in common_keys:
        c = claude.get(key, {})
        reason = c.get("claude_reason", "?") if c else "?"
        reason_counts[reason] += 1

    for reason, count in reason_counts.most_common():
        emit(f"  {count:>4}  {reason}")
    emit()

    # Write report
    report_text = "\n".join(report_lines)
    REPORT_PATH.write_text(report_text + "\n")
    logger.info("Wrote report to %s", REPORT_PATH)
    print(report_text)

    # Write TSV
    write_tsv(all_keys, gemini, bge, claude, TSV_PATH)


if __name__ == "__main__":
    main()
