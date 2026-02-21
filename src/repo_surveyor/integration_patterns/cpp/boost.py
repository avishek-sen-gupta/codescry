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
                        "Boost Beast HTTP namespace for HTTP client/server operations",
                        "This code uses Boost Beast to interact with an HTTP endpoint",
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
                        "Boost Asio TCP socket for async network communication",
                        "This code uses Boost Asio to interact with a TCP socket",
                    ),
                ),
                (
                    r"boost::asio::ip::udp",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Boost Asio UDP socket for async network communication",
                        "This code uses Boost Asio to interact with a UDP socket",
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
                        "Boost Filesystem library for file system operations",
                        "This code uses Boost Filesystem to interact with the file system",
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
                        "Boost Asio deadline_timer for scheduling timed callbacks",
                        "This code uses Boost Asio to interact with scheduled timer tasks",
                    ),
                ),
                (
                    r"\bsteady_timer\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Boost Asio steady_timer for scheduling steady-clock callbacks",
                        "This code uses Boost Asio to interact with scheduled timer tasks",
                    ),
                ),
            ],
        },
    },
)
