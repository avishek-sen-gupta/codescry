"""Boost framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Boost",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"boost::beast::http", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"boost::asio::ip::tcp", Confidence.HIGH),
                (r"boost::asio::ip::udp", Confidence.HIGH),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"boost::filesystem", Confidence.HIGH),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"\bdeadline_timer\b", Confidence.HIGH),
                (r"\bsteady_timer\b", Confidence.HIGH),
            ],
        },
    },
)
