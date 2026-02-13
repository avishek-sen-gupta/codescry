"""Connect framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Connect",
    patterns={
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r'"connectrpc\.com/connect"', Confidence.HIGH),
                (r"connect\.NewHandler", Confidence.HIGH),
            ],
        },
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"connect\.NewClient", Confidence.HIGH),
            ],
        },
    },
)
