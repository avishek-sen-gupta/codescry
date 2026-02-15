"""Repo Surveyor - Analyze repository technology stacks."""

from .ctags import CTagsConfig, CTagsEntry, CTagsResult
from .analysis_graph_builder import AnalysisGraphBuilder, survey_and_persist
from .integration_patterns import Confidence, IntegrationType, Language
from .integration_detector import (
    EntityType,
    FileMatch,
    IntegrationDetectorResult,
    IntegrationSignal,
    detect_integrations,
)
from .ml_classifier import (
    ClassifiedLine,
    CompletionResult,
    FileClassification,
    LineClassifierModel,
    MLIntegrationType,
    RepositoryClassification,
    classify_file,
    classify_repository,
)
from .pipeline_timer import (
    Clock,
    NullPipelineTimer,
    PipelineTimer,
    PipelineTimingObserver,
    SystemClock,
)
from .report import DirectoryMarker, SurveyReport
from .surveyor import RepoSurveyor, survey
from .symbol_resolver import (
    ResolutionResult,
    SymbolIntegration,
    SymbolIntegrationProfile,
    resolve_integration_signals,
)

__all__ = [
    "AnalysisGraphBuilder",
    "ClassifiedLine",
    "CompletionResult",
    "Confidence",
    "Clock",
    "CTagsConfig",
    "CTagsEntry",
    "CTagsResult",
    "DirectoryMarker",
    "EntityType",
    "FileClassification",
    "FileMatch",
    "IntegrationDetectorResult",
    "IntegrationSignal",
    "IntegrationType",
    "Language",
    "LineClassifierModel",
    "NullPipelineTimer",
    "PipelineTimer",
    "PipelineTimingObserver",
    "MLIntegrationType",
    "RepoSurveyor",
    "RepositoryClassification",
    "ResolutionResult",
    "SurveyReport",
    "SymbolIntegration",
    "SymbolIntegrationProfile",
    "SystemClock",
    "classify_file",
    "classify_repository",
    "detect_integrations",
    "resolve_integration_signals",
    "survey",
    "survey_and_persist",
]
