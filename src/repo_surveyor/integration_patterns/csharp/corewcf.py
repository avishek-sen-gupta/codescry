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
                        "CoreWCF namespace import enabling a CoreWCF SOAP service",
                        "This code uses CoreWCF to expose an inbound SOAP service",
                    ),
                ),
                (
                    r"CoreWCF\.Http",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "CoreWCF.Http namespace configuring HTTP transport for a CoreWCF service",
                        "This code uses CoreWCF to expose an inbound SOAP service over HTTP",
                    ),
                ),
            ],
        },
    },
)
