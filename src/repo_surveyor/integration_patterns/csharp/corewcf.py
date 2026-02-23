"""CoreWCF framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="CoreWCF",
    patterns={
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r"using CoreWCF",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "CoreWCF SOAP service is enabled by namespace import",
                        "SOAP service is exposed for inbound requests",
                    ),
                ),
                (
                    r"CoreWCF\.Http",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP transport is configured for CoreWCF service",
                        "SOAP service is exposed over HTTP for inbound requests",
                    ),
                ),
            ],
        },
    },
)
