# Architecture

## Overview

```
languages.json  (declarative: languages, indicators, parsers, framework patterns, infrastructure)
      |
      v
 PluginRegistry  (loads JSON, builds lookup tables, resolves shared patterns)
      |
      +---> detectors.py        (INDICATOR_FILES, EXTENSION_LANGUAGES, FRAMEWORK_PATTERNS,
      |                          GLOB_PATTERNS, K8S_MARKERS derived from registry)
      |
      +---> integration_patterns/__init__.py  (EXTENSION_TO_LANGUAGE, LANGUAGE_MODULES
                                               derived from registry)

survey()             (full analysis pipeline, no persistence)
+-- PipelineTimer (observer, optional -- records stage durations)
+-- (same stages as survey_and_persist, without Neo4j writes)

survey_and_persist()  (full analysis pipeline with Neo4j)
+-- RepoSurveyor.tech_stacks()
|   +-- detectors.detect_indicator_files_with_directories()
|   |   +-- package_parsers.parse_dependencies() -> match_frameworks()
|   +-- detectors.detect_from_glob_patterns()
|   |   +-- package_parsers.parse_dependencies() -> match_frameworks()
|   +-- detectors.detect_kubernetes()
|   +-- detectors.detect_languages_from_extensions()
|
+-- RepoSurveyor.coarse_structure()
|   +-- ctags.run_ctags()
|       +-- _build_ctags_command()
|       +-- _parse_ctags_json_output()
|
+-- integration_detector.detect_integrations()
|       (directory_frameworks built from tech_stacks() DirectoryMarkers)
|   +-- _build_import_gated_framework_map()  (import gating sub-stage)
|   |   +-- _filter_frameworks_by_imports() per file
|   +-- integration_patterns.get_patterns_for_language()
|   |   +-- common + language base + framework-specific patterns
|   +-- scan_file_for_integrations()  (receives pre-gated frameworks)
|   |   +-- syntax_zone.parse_file_zones()  (tree-sitter comment/string filtering)
|   +-- classify_directory()
|
+-- symbol_resolver.resolve_integration_signals()
|   +-- _SymbolLineIndex (spatial index from CTags entries)
|   +-- joins signals to symbols by file path + line containment
|
+-- AnalysisGraphBuilder.persist_tech_stacks()
|   +-- graph_builder.build_tech_stack_graph()
|
+-- AnalysisGraphBuilder.persist_coarse_structure()
|   +-- graph_builder.build_coarse_structure_graph()
|
+-- AnalysisGraphBuilder.persist_integrations()
    +-- graph_builder.build_integration_graph()

(standalone -- not part of survey_and_persist)
+-- call_flow.extract_call_tree()
|   +-- lsp_bridge.LspBridgeClient (Protocol)
|   |   +-- RequestsLspBridgeClient (HTTP -> mojo-lsp)
|   +-- tree-sitter (source parsing)
```

## Plugin Architecture

Language support is defined declaratively in `languages.json` and loaded by the `PluginRegistry` class in `language_plugin.py`. This replaces the previous approach of hardcoded dicts scattered across multiple files.

**`languages.json`** contains two top-level sections:
- `languages` — one entry per language with extensions, indicator files, framework patterns, glob indicators, package manager indicators, and integration module references
- `infrastructure` — Docker, Terraform, Kubernetes definitions

**`PluginRegistry`** loads the JSON at import time and provides backward-compatible query APIs that produce the same dict shapes as the old hardcoded constants. This means `detectors.py` and `integration_patterns/__init__.py` simply derive their module-level exports from the registry — all downstream code (surveyor, integration detector, tests) works unchanged.

Key features:
- **Shared framework patterns**: TypeScript inherits JavaScript's patterns via `"shared_framework_patterns": "JavaScript"`, Kotlin inherits Java's. Resolved at load time by merging.
- **Additional languages**: `build.gradle.kts` signals both Java and Kotlin via `"additional_languages": ["Kotlin"]`.
- **Dynamic module loading**: Integration pattern packages (e.g. `integration_patterns/java/`) are loaded via `importlib.import_module()` based on the `"integration_module"` field. Each package auto-discovers its framework files at import time.

### Adding a New Language

To add support for a new language, edit `languages.json`:

```json
"MyLanguage": {
  "extensions": [".ml"],
  "indicator_files": {
    "myconfig.toml": { "package_managers": ["MyPM"], "parser": "myconfig_toml" }
  },
  "framework_patterns": {
    "myframework": "MyFramework"
  },
  "integration_module": "mylanguage"
}
```

Then:
1. If the language has a config file parser, add a `package_parser/` sub-package under `integration_patterns/{lang}/` containing the parser module(s), then register the import in `package_parsers/__init__.py`
2. If the language needs integration pattern detection, add a package directory in `integration_patterns/` with a `base.py` exporting a `BASE = BasePatternSpec(patterns={...})` instance for base patterns, and optional per-framework files (e.g. `spring.py`) each exporting a `FRAMEWORK = FrameworkPatternSpec(name="...", patterns={...})` instance. For frameworks with generic patterns that could cause false positives (e.g. `\w*\.get\(`), add `import_patterns=(r"import pattern",)` to gate those patterns on file-level imports. The language's `__init__.py` auto-discovers framework files via `pkgutil`. Also add a `Language` enum member in `integration_patterns/types.py`
3. Run `poetry run pytest` to verify

## Tech Stack Analysis

**Entry point:** `surveyor.py` -> `RepoSurveyor.tech_stacks()`

The tech stack analysis runs five detection stages in sequence, aggregating results into a `SurveyReport`:

**Stage 1 — Indicator file walk** (`detectors.py` -> `detect_indicator_files_with_directories()`): Walks the repository directory tree (skipping `.git`, `node_modules`, `__pycache__`, etc.) and matches filenames against the `INDICATOR_FILES` dict (derived from `languages.json` via `PluginRegistry`). Each match (e.g. `pyproject.toml` -> Python + Poetry, `pom.xml` -> Java + Maven) produces a `DirectoryMarker` recording what was found and in which directory.

**Stage 2 — Framework detection from config files** (`detectors.py` -> `detect_frameworks_for_file()`): For each indicator file found in Stage 1, the file's content is parsed for framework dependencies. This delegates to the package parser subsystem (see below). The detected frameworks are merged into the marker's `frameworks` list.

**Stage 3 — Glob pattern detection** (`detectors.py` -> `detect_from_glob_patterns()`): Matches glob patterns like `*.csproj`, `*.sln`, and `*.tf` across the repo. Matched files are also parsed for framework dependencies via the package parser subsystem.

**Stage 4 — Kubernetes detection** (`detectors.py` -> `detect_kubernetes()`): Scans all `.yaml`/`.yml` files for Kubernetes markers (`apiVersion:`, `kind: Deployment`, etc.). Returns a boolean.

**Stage 5 — Language extension scan** (`detectors.py` -> `detect_languages_from_extensions()`): Walks the repo and maps file extensions (`.py`, `.java`, `.rs`, etc.) to language names using the `EXTENSION_LANGUAGES` dict. This catches languages not represented by indicator files (e.g. a `.go` file in a Python project).

The final `SurveyReport` (defined in `report.py`) contains sorted, deduplicated lists of languages, package managers, frameworks, infrastructure, and the per-directory `DirectoryMarker` list. `SurveyReport.to_text()` renders a human-readable plain text report.

### Package Parser Subsystem

**Entry point:** `package_parsers/__init__.py` -> `parse_dependencies(filename, content)`

The parser dispatcher maps exact filenames (e.g. `"pom.xml"`, `"package.json"`) to format-specific parsers, with extension-based fallback for variable names (e.g. `MyApp.csproj`). Each parser returns a list of `ParsedDependency(name, source)` objects. The shared `ParsedDependency` type lives in `package_parsers/types.py`, while individual parser modules are co-located with their language under `integration_patterns/{lang}/package_parser/`. The central dispatcher in `package_parsers/__init__.py` re-imports from these locations.

| Parser module | Location | File format | Technique |
|---|---|---|---|
| `pyproject_toml` | `python/package_parser/` | `pyproject.toml` | `tomllib` — reads PEP 621 `[project.dependencies]` and Poetry `[tool.poetry.dependencies]` |
| `requirements_txt` | `python/package_parser/` | `requirements.txt` | Line parsing — extracts PEP 508 package names |
| `pipfile` | `python/package_parser/` | `Pipfile` | `tomllib` — reads `[packages]` and `[dev-packages]` |
| `setup_py` | `python/package_parser/` | `setup.py` | Regex — extracts `install_requires=[...]` |
| `package_json` | `javascript/package_parser/` | `package.json` | `json` — reads `dependencies`, `devDependencies`, `peerDependencies` |
| `pom_xml` | `java/package_parser/` | `pom.xml` | `xml.etree` — extracts `<artifactId>` from `<dependency>` elements |
| `build_gradle` | `java/package_parser/` | `build.gradle` / `.kts` | Regex — extracts `implementation`, `api`, `compile` declarations |
| `go_mod` | `go/package_parser/` | `go.mod` | Line parsing — extracts module paths from `require` blocks |
| `cargo_toml` | `rust/package_parser/` | `Cargo.toml` | `tomllib` — reads `[dependencies]`, `[dev-dependencies]`, `[build-dependencies]` |
| `csproj` | `csharp/package_parser/` | `*.csproj` | `xml.etree` — extracts `<PackageReference Include="...">` attributes |
| `packages_config` | `csharp/package_parser/` | `packages.config` | `xml.etree` — extracts `<package id="...">` attributes |
| `vcpkg_json` | `cpp/package_parser/` | `vcpkg.json` | `json` — reads `dependencies` array (string or object entries) |
| `conanfile_txt` | `cpp/package_parser/` | `conanfile.txt` | Line parsing — extracts package names from `[requires]` section |

All location paths are relative to `integration_patterns/`. Shared parsers (JS<->TS, Java<->Kotlin, C++<->C) live in the primary language's directory.

**Framework matching** (`_dep_matches_pattern()` + `match_frameworks()`): Parsed dependency names are matched against the `FRAMEWORK_PATTERNS` dict using four rules tried in order:

1. **Exact match**: `"fastapi"` == `"fastapi"`
2. **Prefix-separator**: `"spring-boot-starter-web"` starts with `"spring-boot-"` -> match
3. **Path subsequence**: `"github.com/gin-gonic/gin"` contains `"/gin-gonic/gin/"` -> match
4. **npm scoped**: `"@nestjs/core"` -> scope `"nestjs"` or name `"core"` matches pattern

## Code Structure Extraction

**Entry point:** `surveyor.py` -> `RepoSurveyor.coarse_structure()` -> `ctags.py` -> `run_ctags()`

This subsystem extracts code symbols (classes, methods, fields, functions, etc.) by shelling out to Universal CTags.

**Step 1 — Build command** (`ctags.py` -> `_build_ctags_command()`): Constructs a `ctags` CLI invocation from a `CTagsConfig` object:
```
ctags --output-format=json --fields=* --extras=+q -R
      [--languages=Java,Python] [--exclude=.git] [--exclude=target] ...
```

**Step 2 — Execute** (`ctags.py` -> `run_ctags()`): Runs ctags as a subprocess with a 300-second timeout. Captures stdout (JSON Lines) and stderr.

**Step 3 — Parse** (`ctags.py` -> `_parse_ctags_json_output()`): Reads the JSON Lines output line by line. Each line with `"_type": "tag"` is converted into a `CTagsEntry` via `CTagsEntry.from_json()`. Metadata lines are skipped.

The result is a `CTagsResult` containing a list of `CTagsEntry` objects (each with `name`, `path`, `kind`, `line`, `end`, `scope`, `scope_kind`, `signature`, `language`), the raw output, the return code, and a `success` property. The `end` field captures the symbol's end line when CTags provides it (via `--fields=*`); it is `None` when not available.

## Integration Point Detection

**Entry point:** `integration_detector.py` -> `detect_integrations()`

This subsystem scans source files for regex patterns that indicate system integration points (HTTP/REST, SOAP, messaging, sockets, databases, file I/O, FTP/SFTP, gRPC, GraphQL, email, caching, SSE/streaming, scheduling).

### Pattern Organisation

Patterns are organised in three layers, merged by `get_patterns_for_language()` in `integration_patterns/__init__.py`:

```
For each IntegrationType:
  1. Common patterns (common.py -> COMMON: BasePatternSpec) -- low-confidence, language-agnostic
     e.g. (?i)\bhttp\b, (?i)\bkafka\b
  2. Language base patterns (java/base.py -> BASE: BasePatternSpec) -- always active for that language
     e.g. @Entity, import requests, std::fs
  3. Framework-specific patterns (java/spring.py -> FRAMEWORK: FrameworkPatternSpec) -- active only when framework detected in directory
     e.g. @RestController (Spring), @app.get (FastAPI)
```

Each pattern is a tuple of `(regex, Confidence)` where Confidence is HIGH, MEDIUM, or LOW. The `BasePatternSpec` and `FrameworkPatternSpec` frozen dataclasses (defined in `types.py`) enforce an explicit type contract and immutability for all pattern definitions. `FrameworkPatternSpec` also carries an optional `import_patterns` tuple of regex strings; when non-empty, the framework's patterns are only applied to files whose content matches at least one import pattern (see [Import Gating](api-reference.md#import-gating)). When `get_patterns_for_language()` merges the three layers, each tuple is extended to `(regex, Confidence, source)` where `source` is `"common"`, the language display name (e.g. `"Java"`), or the framework name (e.g. `"Spring"`). This source label is propagated into the `IntegrationSignal.source` field and included in the JSON output.

### File Scanning Flow

1. **File iteration** (`_get_source_files()`): Walks the repo, filters by file extension (using `EXTENSION_TO_LANGUAGE`), and skips excluded directories.

2. **Framework resolution** (`_find_frameworks_for_file()`): For each file, walks up the directory hierarchy looking up entries in the `directory_frameworks` mapping. This allows a file in `backend/api/routes.py` to inherit frameworks declared for `"backend"` or `"."`.

3. **Import gating** (`_build_import_gated_framework_map()`): A separate timed sub-stage (`integration_detection.import_gating`) that runs before file scanning. For each source file, reads the file content and filters the candidate framework list by checking whether the file contains an import for each gated framework (`_filter_frameworks_by_imports()`). Frameworks with empty `import_patterns` (ungated) pass through unconditionally. This prevents false positives like `Map.get()` matching as a Javalin HTTP route in files that don't import Javalin. The result is a `dict[Path, list[str]]` mapping each file to its import-gated frameworks.

4. **Syntax zone filtering** (`syntax_zone.py` -> `parse_file_zones()`): Before regex matching, each file is parsed with tree-sitter to build a `SyntaxRangeMap` identifying which lines belong to one of four syntax zones:

   | Zone | Description | Filtered? |
   |------|-------------|-----------|
   | `CODE` | Default — executable source lines | No (scanned for patterns) |
   | `COMMENT` | Line comments, block comments | Yes (skipped) |
   | `STRING_LITERAL` | String literals, raw strings, template strings, heredocs | Yes (skipped) |
   | `IMPORT` | Import/use/using declarations | No (scanned for patterns) |

   Only `COMMENT` and `STRING_LITERAL` zones are filtered out. `IMPORT` lines are kept because import statements are a valuable signal for integration detection (e.g. `import requests`, `use actix_web`). Mixed-zone lines (code + trailing comment on the same line) are conservatively kept as `CODE`. Languages without tree-sitter support gracefully fall back to scanning all lines via a null-object empty range map (`SyntaxRangeMap(ranges=())`).

   Tree-sitter zone classification is supported for: Java, Python, TypeScript, JavaScript, Go, Rust, C#, Kotlin, Scala, Ruby, PHP, C, C++, and COBOL. Languages without tree-sitter support (PL/I) scan all lines without filtering.

5. **Line-by-line scanning** (`scan_file_for_integrations()`): Reads the file, determines the language from the extension, calls `get_patterns_for_language(language, frameworks)` with the pre-gated framework list to get the merged pattern set, then tests each non-comment/non-string line against each regex. Every match yields an `IntegrationSignal` with the match location, integration type, confidence, matched pattern, entity type, and source (which layer contributed the pattern).

6. **Directory classification** (`classify_directory()`): After scanning files, directory names themselves are matched against the directory patterns from `common.py` (e.g. `controllers` -> HTTP_REST, `proto` -> GRPC). These produce directory-level `IntegrationSignal` entries.

The result is an `IntegrationDetectorResult` containing all `IntegrationSignal` instances and the count of files scanned.

## Symbol Resolution

**Entry point:** `symbol_resolver.py` -> `resolve_integration_signals()`

This subsystem joins integration signals to their containing code symbols, producing per-symbol integration profiles. It is a pure function module with no side effects or I/O.

**Step 1 — Build spatial index** (`_SymbolLineIndex`): Groups `CTagsEntry` objects by file path and builds a list of `_SymbolSpan` (start line, end line, symbol ID, name, kind) per file. Symbols with an explicit `end` field (from CTags `--fields=*`) use it directly; symbols without `end` use the next symbol's start line minus one as a heuristic boundary.

**Step 2 — Resolve signals**: For each `IntegrationSignal`:
- Signals with `entity_type == DIRECTORY` go directly to `unresolved` (directory-level, not symbol-level)
- For `FILE_CONTENT` signals, the absolute file path is normalised to a relative path by stripping the `repo_path` prefix
- The `(relative_path, line_number)` pair is looked up in the spatial index
- The **most specific** (narrowest span) containing symbol is selected
- If found -> `SymbolIntegration`; if no symbol contains the line -> `unresolved`

**Step 3 — Group into profiles**: Resolved integrations are grouped by `symbol_id` into `SymbolIntegrationProfile` objects, each listing all integration signals for that symbol.

The result is a `ResolutionResult` containing:
- `resolved`: tuple of `SymbolIntegration` instances
- `unresolved`: tuple of `IntegrationSignal` instances (directory-level or outside any symbol range)
- `profiles`: tuple of `SymbolIntegrationProfile` instances (grouped by symbol)

## Call Flow Extraction

**Entry point:** `call_flow/extractor.py` -> `extract_call_tree()`

This subsystem traces method call hierarchies within a single file using two tools: tree-sitter for identifying call sites, and LSP go-to-definition for resolving what each call site refers to.

**Step 1 — Parse the source** with tree-sitter to build a syntax tree. A language-specific query (currently only Java: `"(method_invocation name: (identifier) @name)"`) is used to locate call expressions.

**Step 2 — Get document symbols** via the LSP bridge (`client.get_symbols()`). This returns a tree of `DocumentSymbol` objects representing methods, constructors, and functions in the file.

**Step 3 — Build a method map** (`_build_method_map()`): Recursively collects all method-like symbols (LSP SymbolKinds 6, 9, 12) into a dict mapping method name -> list of `{start, end}` line ranges. Parameter signatures are stripped (e.g. `"layout(String)"` -> `"layout"`).

**Step 4 — Walk the call tree** starting from the entry method. For each method:
- Use the tree-sitter query to extract call identifiers within the method's line range (`_extract_call_identifiers()`)
- For each identifier, call `client.get_definition()` to resolve where it's defined
- Map the definition line back to a method name using `_find_method_containing()`
- If the callee is a different method in the same file, record the edge and recurse into it
- Cycle detection prevents infinite recursion

The result is a frozen `CallTree` (defined in `call_flow/types.py`) with an `entry_method` and `edges: dict[str, frozenset[str]]` mapping callers to callees. `format_call_tree()` renders this as an indented tree string with recursion markers.

## LSP Bridge

**Protocol:** `lsp_bridge/client_protocol.py` -> `LspBridgeClient` (a `typing.Protocol`)

The LSP bridge provides access to Language Server Protocol operations through an HTTP REST interface (the [mojo-lsp](https://github.com/avishek-sen-gupta/mojo-lsp) bridge server).

The `LspBridgeClient` protocol defines six operations: `start_server`, `stop_server`, `open_document`, `close_document`, `get_symbols`, and `get_definition`. Using a Protocol rather than an abstract base class allows any object with matching method signatures to satisfy the interface — this enables test doubles without inheritance.

**Concrete implementation:** `lsp_bridge/requests_client.py` -> `RequestsLspBridgeClient`

Each method is a POST to the mojo-lsp bridge:

| Method | Endpoint | Request body | Response |
|---|---|---|---|
| `start_server()` | `POST /start` | `{language, rootUri}` | Server config dict |
| `stop_server()` | `POST /stop` | `{}` | — |
| `open_document()` | `POST /document/open` | `{uri, languageId, text}` | — |
| `close_document()` | `POST /document/close` | `{uri}` | — |
| `get_symbols()` | `POST /symbols` | `{uri}` | `{symbols: [...]}` -> `list[DocumentSymbol]` |
| `get_definition()` | `POST /definition` | `{uri, line, character}` | `{locations: [...]}` -> `list[Location]` |

Response JSON is parsed into frozen dataclasses (`DocumentSymbol`, `Location`) defined in `lsp_bridge/types.py`. `DocumentSymbol` has recursive `children` (a tuple of `DocumentSymbol`) representing the symbol tree.

## Neo4j Persistence

**Entry point:** `analysis_graph_builder.py` -> `AnalysisGraphBuilder`

This subsystem persists tech stack and code structure analysis results into a Neo4j graph database. It uses Protocol-based dependency injection (`Neo4jDriver` protocol) for testability.

### Tech Stack Persistence

`persist_tech_stacks(report)`:

**Step 1 — Build graph data** (`graph_builder.py` -> `build_tech_stack_graph()`): Transforms the `SurveyReport` into four outputs:
- `directories`: List of directory node dicts (path, name, marker_file)
- `dir_relationships`: Parent-child directory edges (built by walking up each path to create intermediate directories)
- `tech_nodes`: Technology nodes (Language, PackageManager, Framework, Infrastructure) linked to their directories
- `top_level_dirs`: Directories that link directly to the Repository node

**Step 2 — Execute Cypher**: Creates nodes and relationships in five batched Cypher queries:
1. `CREATE (r:Repository {path, name})`
2. `UNWIND $directories ... CREATE (d:Directory {...})`
3. `UNWIND $relationships ... CREATE (parent)-[:CONTAINS_DIRECTORY]->(child)`
4. `UNWIND $top_level ... CREATE (r)-[:CONTAINS_DIRECTORY]->(d)`
5. For each tech type: `UNWIND $nodes ... CREATE (d)-[:USES_LANGUAGE|USES_FRAMEWORK|...]->(t:TechType {name})`

### Code Structure Persistence

`persist_coarse_structure(result, repo_path)`:

**Step 1 — Build graph data** (`graph_builder.py` -> `build_coarse_structure_graph()`):
- `_index_symbols()`: Creates a unique ID for each symbol (`{path}:{name}:{kind}:{line}`), computes a `qualified_name` by prepending the scope (e.g. `MyClass.myMethod`, or just `MyClass` for top-level symbols), and extracts the package name using language-aware heuristics (e.g. Java `src/main/java/com/example/` -> `com.example`). Builds a `(path, name)` index for parent lookup.
- `_resolve_relationships()`: For each symbol with a `scope`, looks up the parent symbol by matching `(path, scope_name)` and optionally `scope_kind`. Records `{child_id, parent_id}` relationships.
- Symbols with no resolved parent become `top_level_symbols`.

**Step 2 — Execute Cypher**: Creates nodes and relationships in three batched queries:
1. `UNWIND $symbols ... CREATE (s:CodeSymbol {...})`
2. `UNWIND $relationships ... CREATE (parent)-[:CONTAINS]->(child)`
3. `UNWIND $top_level ... CREATE (r:Repository)-[:CONTAINS]->(s:CodeSymbol)`

### Integration Persistence

`persist_integrations(resolution, integration_result, repo_path)`:

**Step 1 — Build graph data** (`graph_builder.py` -> `build_integration_graph()`): Extracts unique integration type names, builds flat resolved dicts from each signal, then consolidates them by `(symbol_id, file_path, line_number, integration_type)`. Each consolidated dict contains a `pattern_matches` list and a `confidence` set to the highest among contributing matches. Unresolved integration dicts are built separately (integration_type, confidence, matched_pattern, entity_type, file_path, line_number, line_content, source).

**Step 2 — Execute Cypher**: Creates nodes and relationships in up to three batched queries:
1. `UNWIND $names ... MERGE (t:IntegrationType {name})` — idempotent creation of integration type nodes
2. `UNWIND $integrations ... MATCH (CodeSymbol) MATCH (IntegrationType) CREATE (sig:IntegrationSignal {...}) CREATE (s)-[:HAS_INTEGRATION]->(sig) CREATE (sig)-[:OF_TYPE]->(t) WITH sig, i UNWIND i.pattern_matches AS pm CREATE (m:PatternMatch {...}) CREATE (sig)-[:MATCHED_BY]->(m)` — creates IntegrationSignal nodes with confidence, line content, line number, and file path, linked from symbols and to their integration types, with child PatternMatch nodes (matched_pattern, confidence, source) linked via MATCHED_BY
3. `UNWIND $signals ... MATCH (Repository) CREATE (u:UnresolvedIntegration {...}) CREATE (r)-[:HAS_UNRESOLVED_INTEGRATION]->(u)` — persists signals not resolved to any symbol

A convenience function `survey_and_persist()` orchestrates the full pipeline: runs `tech_stacks()`, `coarse_structure()`, `detect_integrations()` (using the per-directory framework mappings from tech stack detection), and `resolve_integration_signals()`, then persists tech stacks, code structure, and integration results to Neo4j. Like `survey()`, its `languages` parameter filters both CTags and integration detection and accepts both `str` and `Language` enum values.

## Design Patterns

The codebase uses several consistent patterns:

- **Protocol-based dependency injection**: External systems (Neo4j, LSP bridge) are accessed through `typing.Protocol` interfaces, with concrete implementations injected at construction time. This avoids hardcoded imports and enables testing with mock objects.
- **Frozen dataclasses**: All data transfer objects (`IntegrationSignal`, `FileMatch`, `CallTree`, `DocumentSymbol`, `Location`) and pattern specifications (`BasePatternSpec`, `FrameworkPatternSpec`) are frozen to prevent mutation after construction.
- **Pure graph-building functions**: `graph_builder.py` contains pure functions that transform survey data into graph representations without side effects — the actual database writes are in `analysis_graph_builder.py`.
- **Layered pattern merging**: Integration patterns are composed from three layers (common -> language base -> framework-specific) at query time, keeping each layer independently editable.
