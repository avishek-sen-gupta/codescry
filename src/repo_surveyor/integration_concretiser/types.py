"""Data types for integration signal AST context extraction."""

from dataclasses import dataclass
from enum import Enum
from typing import Protocol, runtime_checkable

from repo_surveyor.detection.integration_detector import (
    FileMatch,
    IntegrationSignal,
)
from repo_surveyor.integration_patterns import (
    Confidence,
    IntegrationType,
    SignalDirection,
)
from repo_surveyor.detection.integration_detector import EntityType


class SignalValidity(Enum):
    """Whether a concretised signal represents a real integration or noise."""

    SIGNAL = "SIGNAL"
    NOISE = "NOISE"


@runtime_checkable
class SignalLike(Protocol):
    """Structural protocol unifying IntegrationSignal and CompositeIntegrationSignal."""

    @property
    def match(self) -> FileMatch: ...

    @property
    def integration_type(self) -> IntegrationType: ...

    @property
    def confidence(self) -> Confidence: ...

    @property
    def matched_pattern(self) -> str: ...

    @property
    def entity_type(self) -> EntityType: ...

    @property
    def source(self) -> str: ...

    @property
    def direction(self) -> SignalDirection: ...

    def to_dict(self) -> dict: ...


@dataclass(frozen=True)
class CompositeIntegrationSignal:
    """Multiple IntegrationSignals merged for the same source line.

    Delegates all scalar properties to the first signal in the group,
    preserving all original match metadata in the signals tuple.
    """

    signals: tuple[IntegrationSignal, ...]

    @property
    def match(self) -> FileMatch:
        return self.signals[0].match

    @property
    def integration_type(self) -> IntegrationType:
        return self.signals[0].integration_type

    @property
    def confidence(self) -> Confidence:
        return self.signals[0].confidence

    @property
    def matched_pattern(self) -> str:
        return self.signals[0].matched_pattern

    @property
    def entity_type(self) -> EntityType:
        return self.signals[0].entity_type

    @property
    def source(self) -> str:
        return self.signals[0].source

    @property
    def direction(self) -> SignalDirection:
        return self.signals[0].direction

    def to_dict(self) -> dict:
        """Serialise to a JSON-friendly dict with all match details."""
        return {
            "line_content": self.match.line_content.strip(),
            "file_path": self.match.file_path,
            "line_number": self.match.line_number,
            "match_details": [
                {
                    "integration_type": sig.integration_type.value,
                    "confidence": sig.confidence.value,
                    "matched_pattern": sig.matched_pattern,
                    "source": sig.source,
                    "direction": sig.direction.value,
                }
                for sig in self.signals
            ],
        }


@dataclass(frozen=True)
class ASTContext:
    """Enclosing AST node context for an integration signal.

    Attributes:
        node_type: Tree-sitter node type, e.g. "function_definition".
        node_text: Full text of the enclosing node.
        start_line: 1-indexed start line of the enclosing node.
        end_line: 1-indexed end line of the enclosing node.
    """

    node_type: str
    node_text: str
    start_line: int
    end_line: int


@dataclass(frozen=True)
class ConcretisedSignal:
    """An integration signal with its classified validity and direction.

    Attributes:
        original_signal: The original integration signal (or composite).
        ast_context: The enclosing AST node context.
        validity: Whether this is a real integration (SIGNAL) or noise (NOISE).
        direction: Integration direction (INWARD, OUTWARD, AMBIGUOUS).
                   AMBIGUOUS when validity is NOISE or scores are too close.
    """

    original_signal: SignalLike
    ast_context: ASTContext
    validity: SignalValidity
    direction: SignalDirection

    @property
    def is_integration(self) -> bool:
        return self.validity == SignalValidity.SIGNAL


@dataclass(frozen=True)
class ConcretisationResult:
    """Result of running concretisation over a set of signal groups.

    Attributes:
        concretised: All concretised signals (classified and unclassified).
        signals_submitted: Total number of signals processed.
        signals_classified: Number of signals classified as integrations (SIGNAL).
        signals_unclassified: Number of signals classified as noise (NOISE).
    """

    concretised: tuple[ConcretisedSignal, ...]
    signals_submitted: int
    signals_classified: int
    signals_unclassified: int
