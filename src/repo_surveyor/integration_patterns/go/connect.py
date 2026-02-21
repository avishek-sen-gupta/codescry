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
                        "Connect connectrpc.com/connect package import for Connect RPC integration",
                        "This code uses Connect to interact with a Connect RPC service",
                    ),
                ),
                (
                    r"connect\.NewHandler",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Connect connect.NewHandler function registering an inbound Connect RPC handler",
                        "This code uses Connect to expose an inbound Connect RPC service handler",
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
                        "Connect connect.NewClient function creating an outbound Connect RPC client",
                        "This code uses Connect to call an outbound Connect RPC service",
                    ),
                ),
            ],
        },
    },
)
