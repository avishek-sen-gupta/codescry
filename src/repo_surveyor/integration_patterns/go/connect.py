"""Connect framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Connect",
    patterns={
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (
                    r'"connectrpc\.com/connect"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Connect RPC integration is imported via connectrpc package",
                        "Connect RPC service is accessed",
                    ),
                ),
                (
                    r"connect\.NewHandler",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Connect RPC handler is registered by NewHandler function",
                        "Connect RPC service handler is exposed for inbound calls",
                    ),
                ),
            ],
        },
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"connect\.NewClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Connect RPC client is created by NewClient function",
                        "Connect RPC service is called",
                    ),
                ),
            ],
        },
    },
)
