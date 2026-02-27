# Architectural Decision Records

This document captures key architectural decisions made during the development of Codescry, recorded retroactively.

---

## ADR-001: Poetry for Dependency Management

**Date:** 2026-02-01
**Status:** Accepted

**Context:** The project needed a dependency management and packaging tool for Python 3.13+.

**Decision:** Use Poetry with a `src/` layout (`packages = [{include = "repo_surveyor", from = "src"}]`). Optional heavy ML/embedding dependencies (`torch`, `transformers`, `sentence-transformers`) are isolated in extras groups (`ml`, `hf-local`, `coderank`) to keep the default install lightweight.

**Consequences:** All commands must be prefixed with `poetry run`. CI enforces this. The `src/` layout cleanly separates installable package code from scripts and tests.

---

## ADR-002: Protocol-Based Dependency Injection for External Systems

**Date:** 2026-02-01
**Status:** Accepted

**Context:** The codebase interfaces with multiple external systems (Neo4j, LSP servers, LLMs, embedding services, clocks). Hardcoding concrete implementations makes testing difficult and violates the ports-and-adapters principle.

**Decision:** All external system boundaries are expressed as `typing.Protocol` interfaces. Concrete implementations are injected at construction time, never imported directly in calling code.

**Protocols defined:**
- `Neo4jDriver` — graph database access
- `LspBridgeClient` — Language Server Protocol operations
- `LLMModel` — LLM classification
- `EmbeddingClientProtocol` — embedding vector computation
- `PipelineTimer` — pipeline stage timing
- `Clock` — wall-clock time (for deterministic testing)
- `TSNode`, `TSParser`, `TSTree`, `ParserFactory` — tree-sitter abstractions
- `SignalClassifier` — ML-based signal classification

**Consequences:** Every external dependency can be replaced with a test double without inheritance or monkey-patching. Constructor signatures document exactly which external capabilities each component requires.

---

## ADR-003: Null Object Pattern Instead of None

**Date:** 2026-02-01
**Status:** Accepted

**Context:** Many injectable dependencies are optional (e.g., pipeline timer, signal classifier). Using `None` defaults would scatter `if x is not None` checks throughout the codebase.

**Decision:** All optional dependencies provide a Null Object implementation. Functions accept these as defaults rather than `None`.

**Examples:**
- `NullPipelineTimer` — no-op stage recording
- `NullSignalClassifier` — always predicts `NOT_DEFINITE`
- `FALLBACK_AST_CONTEXT` — empty AST context when no language parser is available

**Consequences:** Calling code never checks for `None`. The `survey()` function signature demonstrates this: `timer: PipelineTimer = NullPipelineTimer()`, `classifier: SignalClassifier = NullSignalClassifier()`.

---

## ADR-004: Functional Core / Imperative Shell

**Date:** 2026-02-01
**Status:** Accepted

**Context:** The analysis pipeline involves both pure data transformations and I/O operations (file reads, subprocess calls, database writes, API calls).

**Decision:** All data transformations are expressed as pure functions; I/O and side effects are isolated in an outer shell layer.

**Examples:**
- `graph/graph_builder.py` contains pure functions that transform survey data into graph representations — no database dependency
- `graph/analysis_graph_builder.py` handles all Cypher writes to Neo4j
- `symbol_resolver.py` is a pure function module with no side effects or I/O
- File reading is injected as `Callable[[str], bytes]` rather than hardcoded `open()` calls

**Consequences:** Core logic is trivially testable without mocking. The imperative shell is thin and concentrated at the boundaries.

---

## ADR-005: Frozen Immutable Dataclasses for All DTOs

**Date:** 2026-02-01
**Status:** Accepted

**Context:** Data flows through a multi-stage pipeline. Mutable shared state across stages is error-prone and difficult to debug.

**Decision:** All data transfer objects, pattern specifications, signals, and results use `@dataclass(frozen=True)`.

**Applies to:** `IntegrationSignal`, `FileMatch`, `CompositeIntegrationSignal`, `ASTContext`, `ConcretisedSignal`, `ConcretisationResult`, `BasePatternSpec`, `FrameworkPatternSpec`, `PatternDescription`, `StageTimingRecord`, `SignalGroup`, `EvidenceCheck`, `EvidenceVerdict`, `LanguagePlugin`, `IndicatorFileConfig`, `GlobIndicatorConfig`, `InfrastructureConfig`, and others.

**Consequences:** Data is safe to share across pipeline stages. Any transformation produces a new object rather than mutating in place.

---

## ADR-006: Declarative Language Plugin System via JSON

**Date:** 2026-02-12
**Status:** Accepted

**Context:** Language support was originally scattered across hardcoded Python dicts in multiple files. Adding a new language required editing several modules.

**Decision:** Language support is defined declaratively in `languages.json`. A `PluginRegistry` loads this at runtime and provides backward-compatible query APIs (`indicator_files()`, `extension_languages()`, `all_framework_patterns()`, etc.).

**Key features:**
- Shared framework patterns between related languages (TypeScript inherits JavaScript's, Kotlin inherits Java's) via `"shared_framework_patterns"` resolved at load time
- Dynamic integration module loading via `importlib.import_module()` based on the `"integration_module"` field
- Adding a new language requires editing `languages.json` and optionally adding a package parser / integration pattern module

**Consequences:** Single source of truth for language definitions. Downstream code (`detectors.py`, `integration_patterns/__init__.py`) derives its module-level exports from the registry unchanged.

---

## ADR-007: Three-Layer Pattern Merging for Integration Detection

**Date:** 2026-02-16
**Status:** Accepted

**Context:** Integration patterns vary in specificity: some are universal (e.g., `(?i)\bhttp\b`), some are language-specific (e.g., `import requests`), and some are framework-specific (e.g., `@RestController`). A flat list conflates confidence levels and causes false positives.

**Decision:** Integration patterns are composed from three ordered layers, merged at query time by `get_patterns_for_language()`:

1. **Common** (`common.py`) — language-agnostic, low confidence
2. **Language base** (`{lang}/base.py`) — always active for that language
3. **Framework-specific** (`{lang}/{framework}.py`) — active only when the framework is detected in the directory

Each pattern is a tuple of `(regex, Confidence)`. `FrameworkPatternSpec` carries optional `import_patterns` for import gating. A `source` label (`"common"`, language name, or framework name) is propagated into `IntegrationSignal.source`.

**Consequences:** Each layer is independently editable. Framework patterns only fire when the framework is actually present, reducing false positives. Import gating prevents generic patterns (e.g., `Map.get()`) from matching in files that don't import the framework.

---

## ADR-008: Tree-Sitter for Syntax Zone Filtering

**Date:** 2026-02-16
**Status:** Accepted

**Context:** Regex-based integration detection produces false positives from pattern matches in comments, string literals, and other non-executable code.

**Decision:** Use tree-sitter (via `tree-sitter-language-pack`) to classify file lines into syntax zones (CODE, COMMENT, STRING_LITERAL, IMPORT, PACKAGE_DECLARATION) before pattern matching. Only CODE and IMPORT zones are scanned; COMMENT and STRING_LITERAL are filtered out.

**Supported languages:** Java, Python, TypeScript, JavaScript, Go, Rust, C#, Kotlin, Scala, Ruby, PHP, C, C++, COBOL (14 languages). Languages without tree-sitter support fall back to `SyntaxRangeMap(ranges=())` (null-object pattern — scans all lines).

**Consequences:** Significant reduction in false positives. IMPORT lines are kept because they are a valuable signal (e.g., `import requests`). The same tree-sitter infrastructure is reused for AST context extraction and CFG construction.

---

## ADR-009: Universal CTags for Code Symbol Extraction

**Date:** 2026-02-01
**Status:** Accepted

**Context:** The project needs to extract code symbols (classes, methods, functions) across 16+ languages for symbol resolution.

**Decision:** Use Universal CTags as an external binary, invoked as a subprocess with `--output-format=json --fields=* --extras=+q -R`. Output is parsed from JSON Lines format into `CTagsEntry` dataclasses.

**Alternatives considered:** Writing language-specific parsers (too expensive for 16+ languages), tree-sitter queries (good for AST but not designed for symbol table extraction).

**Consequences:** CTags supports 40+ languages natively with no per-language parser code. The JSON output format provides structured, machine-parseable results. Requires Universal CTags as an external dependency.

---

## ADR-010: Neo4j as Optional Graph Persistence Layer

**Date:** 2026-02-01
**Status:** Accepted

**Context:** Analysis results have a natural graph structure (repositories contain directories, directories use technologies, symbols have integrations). Some users want to query this graph; others just need the in-memory results.

**Decision:** Neo4j is available as an optional persistence layer. The core pipeline works without it.

- `survey()` — runs the full pipeline, returns in-memory results
- `survey_and_persist()` — same pipeline plus Neo4j writes
- Graph data is first built as plain Python dicts in `graph_builder.py` (pure), then persisted by `analysis_graph_builder.py` (imperative shell)
- `AnalysisGraphBuilder` accepts a `Neo4jDriver` protocol, not a concrete driver

**Consequences:** The core analysis pipeline has no runtime dependency on Neo4j. Graph persistence is a thin, testable layer.

---

## ADR-011: Pipeline as Orchestrated Stage Sequence with Timer Observer

**Date:** 2026-02-16
**Status:** Accepted

**Context:** The analysis pipeline has multiple stages (tech stacks, coarse structure, integration detection, signal deduplication, symbol resolution, signal concretisation). Progress tracking and performance profiling are needed.

**Decision:** Each stage is bookended by `timer.stage_started(name)` / `timer.stage_completed(name)` calls on an injected `PipelineTimer`. The `PipelineTimingObserver` records wall-clock duration per stage and serializes to JSON.

**Consequences:** Pipeline runs without timing overhead by default (via `NullPipelineTimer`). Stage boundaries are explicit and named. Timing data is available for profiling without instrumenting individual functions.

---

## ADR-012: Multi-Backend Signal Concretisation Pipeline

**Date:** 2026-02-19
**Status:** Accepted

**Context:** Raw regex-detected integration signals need classification as genuine integrations vs. noise, and directionality (inward/outward). No single approach is clearly superior.

**Decision:** Support multiple pluggable concretisation backends:

- **ML-based:** TF-IDF + Logistic Regression (`SignalClassifier`) — fast, no API dependency
- **Embedding-based:** Cosine similarity against directional descriptions (`GenericIntegrationDescriptionEmbeddingConcretiser`) — captures semantic meaning
- **Pattern-embedding:** Per-pattern description nearest-neighbor (`FrameworkSpecificIntegrationDescriptionEmbeddingConcretiser`) — framework-specific semantics
- **LLM-based:** Ollama (local) and Gemini Flash (cloud) — highest accuracy, highest cost
- **Hybrid:** Embedding gate + Gemini Flash direction — balances cost and accuracy

All backends produce `ConcretisedSignal` objects with two-stage classification: `validity` (SIGNAL/NOISE) and `direction` (INWARD/OUTWARD/AMBIGUOUS).

**Consequences:** Users choose the cost/accuracy trade-off appropriate to their use case. The `EmbeddingClientProtocol` allows new embedding backends to be added without modifying classification logic.

---

## ADR-013: Evidence Check System for Score Adjustment

**Date:** 2026-02-25
**Status:** Accepted

**Context:** Raw embedding similarity scores don't account for contextual signals (test files, vendor directories, log statements, generated code) that strongly indicate whether a pattern match is a true or false positive.

**Decision:** A composable evidence check system adjusts raw scores before threshold comparison. Seven universal suppression checks are defined: `in_test_file` (-0.15), `in_vendor_dir` (-0.25), `in_generated_file` (-0.30), `in_config_dir` (-0.10), `in_string_literal` (-0.30), `in_log_statement` (-0.25), `in_constant_decl` (-0.20). Framework-specific checks can be added per source pattern.

**Consequences:** Evidence weights accumulate and the adjusted score is clamped to [0, 1]. Metadata per signal includes `evidence_fired` for transparency. The system is extensible without modifying the core classifier.

---

## ADR-014: AST Context Extraction with Batch Optimisation

**Date:** 2026-02-23
**Status:** Accepted

**Context:** Concretisation needs the enclosing AST context (function/method body) for each integration signal. Naively parsing the file once per signal is O(N) redundant.

**Decision:** Signals are grouped by file before AST extraction. Each `(file, language)` pair is parsed once. Three granularities are available: `batch_extract_ast_contexts` (enclosing function/method), `batch_extract_invocation_contexts` (innermost call expression), `batch_extract_statement_contexts` (enclosing statement). File reading is injected as `Callable[[str], bytes]` for testability.

**Consequences:** Eliminates redundant parses. A file with 50 integration signals is parsed once, not 50 times.

---

## ADR-015: LSP Bridge via HTTP Protocol

**Date:** 2026-02-10
**Status:** Accepted

**Context:** Call-flow extraction needs Language Server Protocol operations (symbol lookup, go-to-definition). Integrating an LSP client library directly is complex and language-server-specific.

**Decision:** LSP operations are accessed through an external HTTP bridge server ([mojo-lsp](https://github.com/avishek-sen-gupta/mojo-lsp)). The `LspBridgeClient` protocol defines six operations (`start_server`, `stop_server`, `open_document`, `close_document`, `get_symbols`, `get_definition`), with `RequestsLspBridgeClient` as the HTTP implementation.

**Consequences:** Decoupled from any specific language server. The bridge can wrap any LSP-compliant server. Used only for call-flow extraction, not the main integration detection pipeline.

---

## ADR-016: CFG Construction via Tree-Sitter Protocol Abstraction

**Date:** 2026-02-17
**Status:** Accepted

**Context:** Control flow graph construction needs to work across 16 languages with a single algorithm.

**Decision:** A deterministic CFG builder operates on tree-sitter parse trees through protocol abstractions (`TSNode`, `TSParser`, `ParserFactory`). A role registry maps 99 tree-sitter node types to CFG roles (branch, loop, return, etc.), enabling language-independent graph construction.

**Consequences:** One algorithm covers all 16 languages (except PL/I, which lacks tree-sitter support). Adding a new language requires only adding node type mappings to the role registry.

---

## ADR-017: Absolute Imports Throughout

**Date:** 2026-02-20
**Status:** Accepted

**Context:** Relative imports make it harder to move modules, cause confusion about import paths, and make grep-based code navigation less reliable.

**Decision:** All imports use fully qualified module names (e.g., `from repo_surveyor.detection.syntax_zone import ...`). Relative imports are prohibited.

**Consequences:** Every import is unambiguous and searchable. Enforced by convention and code review.

---

## ADR-018: SignalDirection as First-Class Domain Concept

**Date:** 2026-02-19
**Status:** Accepted

**Context:** Integration signals have an inherent directionality (is this code exposing a service or consuming one?). Deriving direction post-hoc loses the domain knowledge embedded in pattern definitions.

**Decision:** `SignalDirection` (INWARD/OUTWARD/AMBIGUOUS/NOT_INTEGRATION) is a first-class enum annotated directly on all `FrameworkPatternSpec` pattern definitions. Two human-readable description strings per pattern capture the semantic meaning for embedding-based classification.

**Consequences:** Direction is available from the moment a pattern is defined. Enables both rule-based and embedding-based direction classification. Description strings serve as nearest-neighbor targets for the pattern-embedding concretiser.

---

## ADR-019: Embedding Cache with Content-Hash Invalidation

**Date:** 2026-02-21
**Status:** Accepted

**Context:** Embedding ~3,430 pattern descriptions via external APIs takes ~2 minutes and costs money. Descriptions change infrequently.

**Decision:** Pattern description embeddings are cached to JSON files, keyed by SHA-256 hash of all description texts. The cache auto-invalidates when patterns change. Cached files are tracked in Git LFS.

**Consequences:** Subsequent runs skip the ~2 minute embedding warm-up. Cache invalidation is automatic and reliable.

---

## ADR-020: Datalog/Soufflé as Experimental Query Layer

**Date:** 2026-02-19
**Status:** Experimental

**Context:** Some integration patterns are structural (e.g., a `@RestController` class with `@GetMapping` methods) and cannot be expressed as line-level regex.

**Decision:** A tree-sitter to Soufflé Datalog emission pipeline converts parse trees into a 16-relation ontology. Soufflé queries express structural patterns declaratively. A Spring PoC demonstrates annotation-based, type-reference-based, and instantiation-based detection.

**Consequences:** Proof-of-concept only; not integrated into the main `survey()` pipeline. Requires Soufflé as an external dependency. Demonstrates that structural patterns can complement regex-based detection.

---

## ADR-021: Constants Encapsulated in Classes

**Date:** 2026-02-01
**Status:** Accepted

**Context:** Bare module-level constants are hard to organise, discover, and namespace.

**Decision:** All string and numeric constants are wrapped in classes (e.g., `TechCategory(StrEnum)`, `MarkerKey`, `TechLabel`, `IntegrationLabel`). No raw global variables exposed at module level.

**Consequences:** Constants are namespaced, discoverable via IDE autocompletion, and type-safe where `StrEnum` is used.

---

## ADR-022: Test Organisation by Functional Subdirectory

**Date:** 2026-02-20
**Status:** Accepted

**Context:** Tests needed to be organised in a way that scales with the codebase. A flat test directory becomes unwieldy.

**Decision:** Tests are organised into functional subdirectories mirroring source module names: `tests/cfg/`, `tests/concretisation/`, `tests/datalog/`, `tests/detection/`, `tests/export/`, `tests/surveyor/`, `tests/training/`. A `local_repo` pytest marker skips tests requiring external repo clones in CI.

**Consequences:** Test files are co-located with the modules they test (by naming convention). Easy to run a subset of tests for a specific subsystem.

---

## ADR-023: No Static Methods

**Date:** 2026-02-01
**Status:** Accepted

**Context:** Static methods cannot be overridden via dependency injection and make testing harder. They often indicate a function that should be a module-level function or a method on an instance.

**Decision:** Static methods are categorically prohibited. Use module-level functions or instance methods instead.

**Consequences:** All behaviour is injectable and testable. Enforced by convention and code review.
