"""Repo Surveyor - Analyze repository technology stacks."""

from repo_surveyor.symbols.ctags import CTagsConfig, CTagsEntry, CTagsResult
from repo_surveyor.graph.analysis_graph_builder import (
    AnalysisGraphBuilder,
    survey_and_persist,
)
from repo_surveyor.integration_concretiser import (
    ASTContext,
    ConcretisationResult,
    ConcretisedSignal,
    SignalGroup,
    concretise_integration_signals,
)
from repo_surveyor.integration_patterns import Confidence, IntegrationType, Language
from repo_surveyor.detection.integration_detector import (
    EntityType,
    FileMatch,
    IntegrationDetectorResult,
    IntegrationSignal,
    detect_integrations,
)
from repo_surveyor.ml_classifier import (
    ClassifiedLine,
    CompletionResult,
    FileClassification,
    LLMModel,
    MLIntegrationType,
    RepositoryClassification,
    classify_file,
    classify_repository,
)
from repo_surveyor.core.pipeline_timer import (
    Clock,
    NullPipelineTimer,
    PipelineTimer,
    PipelineTimingObserver,
    SystemClock,
)
from repo_surveyor.core.report import DirectoryMarker, SurveyReport
from repo_surveyor.core.surveyor import RepoSurveyor, survey
from repo_surveyor.symbols.symbol_resolver import (
    ResolutionResult,
    SymbolIntegration,
    SymbolIntegrationProfile,
    resolve_integration_signals,
)

__all__ = [
    "AnalysisGraphBuilder",
    "ASTContext",
    "ConcretisationResult",
    "ConcretisedSignal",
    "SignalGroup",
    "concretise_integration_signals",
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
    "LLMModel",
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
