"""Boost framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Boost",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"boost::beast::http",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "HTTP operations are enabled via Beast",
                        "HTTP endpoint is accessed",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"boost::asio::ip::tcp",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TCP socket is created for async communication",
                        "TCP socket is accessed",
                    ),
                ),
                (
                    r"boost::asio::ip::udp",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "UDP socket is created for async communication",
                        "UDP socket is accessed",
                    ),
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r"boost::filesystem",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Filesystem is accessed via library",
                        "File system is accessed",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"\bdeadline_timer\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Timer is scheduled for timed callbacks",
                        "Timer task is scheduled",
                    ),
                ),
                (
                    r"\bsteady_timer\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Timer is scheduled with steady clock",
                        "Timer task is scheduled",
                    ),
                ),
            ],
        },
    },
)
