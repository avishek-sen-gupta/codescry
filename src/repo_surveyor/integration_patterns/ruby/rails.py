"""Rails framework integration patterns."""

from ..types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Rails",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"< ApplicationController", Confidence.HIGH, SignalDirection.INWARD),
                (r"< ActionController::Base", Confidence.HIGH, SignalDirection.INWARD),
                (r"< ActionController::API", Confidence.HIGH, SignalDirection.INWARD),
                (r"\brender\b\s+json:", Confidence.HIGH, SignalDirection.INWARD),
                (r"\brespond_to\b\s+do", Confidence.HIGH, SignalDirection.INWARD),
                (r"\bbefore_action\b", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"\bafter_action\b", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"\bresources\b\s+:", Confidence.HIGH, SignalDirection.INWARD),
                (r"\bget\b\s+['\"]", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"\bpost\b\s+['\"]", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"\bput\b\s+['\"]", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"\bdelete\b\s+['\"]", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"\bpatch\b\s+['\"]", Confidence.MEDIUM, SignalDirection.INWARD),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"ActiveRecord::Schema\.define",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"\.transaction\s+do", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\.pluck\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\.select\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"\.group\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"\.order\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"\.limit\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"\.offset\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"\.having\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"\.count\b", Confidence.LOW, SignalDirection.OUTWARD),
                (r"\.exists\?", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"\.find_or_create_by", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\.find_or_initialize_by", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\.update_all\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\.delete_all\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\.destroy_all\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"\baccepts_nested_attributes_for\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (
                    r"\bdependent:\s+:destroy\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"\bforeign_key:", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bclass_name:", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"\bpolymorphic:\s+true\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bthrough:", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"\.arel_table", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Arel\.", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"ActiveGraph::Node", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"ActiveGraph::Relationship",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"ActionCable", Confidence.HIGH, SignalDirection.INWARD),
                (
                    r"< ApplicationCable::Channel",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                ),
                (
                    r"< ApplicationCable::Connection",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"< ApplicationMailer", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"< ActionMailer::Base", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\.deliver_now", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\.deliver_later", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"Rails\.cache\.", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bcaches_action\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"\bfragment_cache\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"config\.cache_store", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"< ApplicationJob", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"< ActiveJob::Base", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"\.perform_later", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"\.perform_now", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
