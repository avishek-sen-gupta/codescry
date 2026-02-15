"""Rails framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Rails",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"< ApplicationController", Confidence.HIGH),
                (r"< ActionController::Base", Confidence.HIGH),
                (r"< ActionController::API", Confidence.HIGH),
                (r"\brender\b\s+json:", Confidence.HIGH),
                (r"\brespond_to\b\s+do", Confidence.HIGH),
                (r"\bbefore_action\b", Confidence.MEDIUM),
                (r"\bafter_action\b", Confidence.MEDIUM),
                (r"\bresources\b\s+:", Confidence.HIGH),
                (r"\bget\b\s+['\"]", Confidence.MEDIUM),
                (r"\bpost\b\s+['\"]", Confidence.MEDIUM),
                (r"\bput\b\s+['\"]", Confidence.MEDIUM),
                (r"\bdelete\b\s+['\"]", Confidence.MEDIUM),
                (r"\bpatch\b\s+['\"]", Confidence.MEDIUM),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"ActiveRecord::Schema\.define", Confidence.HIGH),
                (r"\.transaction\s+do", Confidence.HIGH),
                (r"\.pluck\(", Confidence.HIGH),
                (r"\.select\(", Confidence.MEDIUM),
                (r"\.group\(", Confidence.MEDIUM),
                (r"\.order\(", Confidence.MEDIUM),
                (r"\.limit\(", Confidence.MEDIUM),
                (r"\.offset\(", Confidence.MEDIUM),
                (r"\.having\(", Confidence.MEDIUM),
                (r"\.count\b", Confidence.LOW),
                (r"\.exists\?", Confidence.MEDIUM),
                (r"\.find_or_create_by", Confidence.HIGH),
                (r"\.find_or_initialize_by", Confidence.HIGH),
                (r"\.update_all\(", Confidence.HIGH),
                (r"\.delete_all\b", Confidence.HIGH),
                (r"\.destroy_all\b", Confidence.HIGH),
                (r"\baccepts_nested_attributes_for\b", Confidence.HIGH),
                (r"\bdependent:\s+:destroy\b", Confidence.HIGH),
                (r"\bforeign_key:", Confidence.HIGH),
                (r"\bclass_name:", Confidence.MEDIUM),
                (r"\bpolymorphic:\s+true\b", Confidence.HIGH),
                (r"\bthrough:", Confidence.MEDIUM),
                (r"\.arel_table", Confidence.HIGH),
                (r"Arel\.", Confidence.HIGH),
                (r"ActiveGraph::Node", Confidence.HIGH),
                (r"ActiveGraph::Relationship", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"ActionCable", Confidence.HIGH),
                (r"< ApplicationCable::Channel", Confidence.HIGH),
                (r"< ApplicationCable::Connection", Confidence.HIGH),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"< ApplicationMailer", Confidence.HIGH),
                (r"< ActionMailer::Base", Confidence.HIGH),
                (r"\.deliver_now", Confidence.HIGH),
                (r"\.deliver_later", Confidence.HIGH),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"Rails\.cache\.", Confidence.HIGH),
                (r"\bcaches_action\b", Confidence.HIGH),
                (r"\bfragment_cache\b", Confidence.HIGH),
                (r"config\.cache_store", Confidence.HIGH),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"< ApplicationJob", Confidence.HIGH),
                (r"< ActiveJob::Base", Confidence.HIGH),
                (r"\.perform_later", Confidence.HIGH),
                (r"\.perform_now", Confidence.HIGH),
            ],
        },
    },
)
