"""Boost framework integration patterns."""

from ..types import (
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
                (r"boost::beast::http", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"boost::asio::ip::tcp", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"boost::asio::ip::udp", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"boost::filesystem", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"\bdeadline_timer\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"\bsteady_timer\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
