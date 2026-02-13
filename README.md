# Cartographer

A Python library for experiments in analysing repository technology stacks and code structure.

**Note:** Most of this codebase was vibe-coded.

## Features

- Detects programming languages, package managers, frameworks, and infrastructure
- Structured parsing of config files (package.json, pyproject.toml, pom.xml, .csproj, etc.) for accurate framework detection — no false positives from substring matching
- Associates technologies with their containing directories (useful for monorepos)
- Extracts code symbols using Universal CTags
- Detects system integration points (HTTP/REST, SOAP, messaging, sockets, databases, GraphQL, email, caching, SSE/streaming, scheduling) with framework-aware pattern matching
- Extracts method call trees via [mojo-lsp](https://github.com/avishek-sen-gupta/mojo-lsp) LSP bridge and tree-sitter
- Persists analysis results to Neo4j graph database
- Generates plain text and JSON reports

## Installation

```bash
poetry install
```

## Supported Languages

| Language | Extensions | Indicator Files | Package Managers | Detected Frameworks |
|----------|-----------|-----------------|------------------|---------------------|
| Java | `.java` | `pom.xml`, `build.gradle`, `build.gradle.kts` | Maven, Gradle | Spring, JAX-RS, Micronaut, Quarkus, Javalin, Dropwizard, Vert.x, Play, Apache CXF, Apache Axis2, Spring WS, JAX-WS, Helidon |
| Python | `.py` | `pyproject.toml`, `requirements.txt`, `setup.py`, `Pipfile` | Poetry, pip, Pipenv | FastAPI, Django, Flask, Starlette, Tornado, Pyramid, aiohttp, Sanic, Litestar |
| TypeScript | `.ts`, `.tsx` | `tsconfig.json` | _(shared with JS)_ | _(shared with JS)_ |
| JavaScript | `.js`, `.jsx` | `package.json`, `yarn.lock`, `pnpm-lock.yaml` | npm, Yarn, pnpm | React, Vue.js, Angular, Next.js, Nuxt.js, Express, NestJS, Svelte, Gatsby, Fastify, Hono, Koa, Hapi |
| Go | `.go` | `go.mod` | — | Gin, Echo, Fiber, Chi, Gorilla, Connect |
| Rust | `.rs` | `Cargo.toml` | Cargo | Actix, Axum, Rocket, Warp |
| C# | `.cs` | `*.csproj`, `*.sln`, `packages.config` | NuGet | ASP.NET Core, ASP.NET Web API, ServiceStack, Nancy, Carter, WCF, CoreWCF |
| COBOL | `.cbl`, `.cob`, `.cpy` | — | — | — |
| PL/I | `.pli`, `.pl1`, `.plinc` | — | — | — |
| Ruby | `.rb` | `Gemfile` | Bundler | — |

Infrastructure detection: Docker (`Dockerfile`, `docker-compose.yml`), Terraform (`*.tf`), Kubernetes (`*.yaml` with k8s markers).

Framework detection uses structured parsing of config files rather than naive substring matching — see [Package Parser Subsystem](#package-parser-subsystem) for parser details. This prevents false positives like `"reactive-streams"` matching `"react"` or `"expression"` matching `"express"`.

## Supported Integration Types

| Type | Description | Example Patterns |
|------|-------------|------------------|
| `http_rest` | HTTP/REST endpoints and clients | `@RestController`, `@GetMapping`, `app.get(`, `use actix_web::` |
| `soap` | SOAP/XML web services | `@WebService`, `SOAPMessage`, `XML PARSE` |
| `messaging` | Message queues and event buses | `@KafkaListener`, `@RabbitListener`, `MQPUT`, `SqsClient` |
| `socket` | Raw sockets and WebSockets | `@ServerEndpoint`, `WebSocket`, `TcpListener` |
| `database` | Database connections and ORMs | `@Repository`, `@Entity`, `EXEC SQL`, `DynamoDbClient` |
| `file_io` | File I/O, uploads, and cloud storage | `FileInputStream`, `open(`, `os.Open`, `Azure.Storage.Blobs` |
| `grpc` | gRPC services and clients | `io.grpc`, `import grpc`, `tonic::`, `Grpc.Core` |
| `graphql` | GraphQL APIs and schemas | `@QueryMapping`, `apollo-server`, `graphene`, `HotChocolate` |
| `email` | Email and SMTP services | `javax.mail`, `smtplib`, `nodemailer`, `MailKit` |
| `caching` | Cache stores and distributed caching | `@Cacheable`, `RedisTemplate`, `ioredis`, `IDistributedCache` |
| `sse_streaming` | Server-sent events and streaming | `SseEmitter`, `StreamingResponse`, `EventSource`, `Sse<` |
| `scheduling` | Scheduled tasks and cron jobs | `@Scheduled`, `node-cron`, `Hangfire`, `robfig/cron` |

### Integration Pattern Coverage by Language

| Integration Type | Java | Python | TypeScript | JavaScript | Go | Rust | C# | COBOL | PL/I |
|------------------|:----:|:------:|:----------:|:----------:|:--:|:----:|:--:|:-----:|:----:|
| `http_rest` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `soap` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `messaging` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `socket` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `database` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `file_io` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `grpc` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — |
| `graphql` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — |
| `email` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — |
| `caching` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — |
| `sse_streaming` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — |
| `scheduling` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — |

All 12 integration types have language-specific patterns for Java, Python, TypeScript, JavaScript, Go, Rust, and C#. COBOL and PL/I cover the 6 core types relevant to mainframe systems. Language-agnostic common patterns also apply across all files.

## Usage

### Tech Stack Analysis

```python
from repo_surveyor import RepoSurveyor

surveyor = RepoSurveyor("/path/to/repo")
report = surveyor.tech_stacks()
print(report.to_text())
```

Example output:

```
Repository Survey: /path/to/repo

Languages:
  - JavaScript
  - Python
  - TypeScript

Package Managers:
  - Poetry
  - npm

Frameworks:
  - FastAPI
  - React

Infrastructure:
  - Docker

Directory Markers:
  ./
    Marker: docker-compose.yml
    Infrastructure: Docker Compose
  backend/
    Marker: pyproject.toml
    Languages: Python
    Package Managers: Poetry
    Frameworks: FastAPI
  frontend/
    Marker: package.json
    Languages: JavaScript
    Package Managers: npm
    Frameworks: React
```

Reports are also available as JSON via `to_json()`:

```python
print(report.to_json())          # pretty-printed (indent=2)
print(report.to_json(indent=None))  # compact, single-line
```

Example output:

```json
{
  "repo_path": "/path/to/repo",
  "languages": ["JavaScript", "Python", "TypeScript"],
  "package_managers": ["Poetry", "npm"],
  "frameworks": ["FastAPI", "React"],
  "infrastructure": ["Docker"],
  "directory_markers": [
    {
      "directory": "backend",
      "marker_file": "pyproject.toml",
      "languages": ["Python"],
      "package_managers": ["Poetry"],
      "frameworks": ["FastAPI"],
      "infrastructure": []
    }
  ]
}
```

### Code Structure Analysis with CTags

Extract code symbols (classes, methods, fields, etc.) using Universal CTags:

```python
from repo_surveyor import RepoSurveyor

surveyor = RepoSurveyor("/path/to/repo")
result = surveyor.coarse_structure(languages=["Java"])

for entry in result.entries:
    print(f"{entry.kind}: {entry.name} at {entry.path}:{entry.line}")
```

Options:
- `languages`: Filter by language (e.g., `["Java", "Python"]`)
- `exclude_patterns`: Directories to exclude (defaults: `.git`, `.idea`, `target`, `node_modules`, etc.)
- `verbose`: Enable verbose CTags output

Each `CTagsEntry` contains:
- `name`, `path`, `kind`, `line`
- `scope`, `scope_kind` (containing class/function)
- `signature` (for methods)
- `language`

Requires [Universal CTags](https://github.com/universal-ctags/ctags) to be installed.

### Integration Point Detection

Detect system integration points in source code using pattern matching:

```python
from repo_surveyor import detect_integrations, Language

result = detect_integrations("/path/to/repo", languages=[Language.JAVA])

for point in result.integration_points:
    print(f"{point.integration_type.value}: {point.match.file_path}:{point.match.line_number}")
    print(f"  Pattern: {point.matched_pattern}")
    print(f"  Confidence: {point.confidence.value}")
```

Example output:

```
http_rest: /path/to/repo/src/UserController.java:15
  Pattern: @RestController
  Confidence: high
database: /path/to/repo/src/UserRepository.java:8
  Pattern: @Repository
  Confidence: high
messaging: /path/to/repo/src/OrderListener.java:12
  Pattern: @KafkaListener
  Confidence: high
```

Results are also available as JSON via `to_json()`:

```python
print(result.to_json())             # pretty-printed (indent=2)
print(result.to_json(indent=None))  # compact, single-line
```

Example output:

```json
{
  "files_scanned": 3,
  "integration_points": [
    {
      "integration_type": "http_rest",
      "confidence": "high",
      "matched_pattern": "@RestController",
      "entity_type": "file_content",
      "match": {
        "file_path": "/path/to/repo/src/UserController.java",
        "line_number": 15,
        "line_content": "@RestController",
        "language": "Java"
      }
    }
  ]
}
```

#### Framework-Aware Detection

Integration patterns are grouped per framework. When you provide a `directory_frameworks` mapping, framework-specific patterns are applied only in directories where that framework is active. Base language patterns always apply regardless.

```python
from repo_surveyor import detect_integrations

# Map directories to their active frameworks (from tech stack detection)
result = detect_integrations(
    "/path/to/repo",
    directory_frameworks={
        "backend": ["FastAPI"],
        "frontend": ["Express"],
    },
)
```

This means:
- A Python file in `backend/` will match FastAPI patterns (`@app.get`, `from fastapi import`) alongside base Python patterns (`import requests`, `from sqlalchemy import`)
- A JS file in `frontend/` will match Express patterns (`app.get(`, `require('express')`) alongside base JavaScript patterns (`require('kafkajs')`, `fetch(`)
- Files in directories without a framework mapping only match common and base language patterns

### LSP Bridge Client

Communicate with language servers via the [mojo-lsp](https://github.com/avishek-sen-gupta/mojo-lsp) REST bridge to get symbols, definitions, and more:

```python
from repo_surveyor.lsp_bridge import RequestsLspBridgeClient

client = RequestsLspBridgeClient("http://localhost:3000")

client.start_server("java", "file:///path/to/project")
client.open_document("file:///path/to/File.java", "java", source_text)

symbols = client.get_symbols("file:///path/to/File.java")
locations = client.get_definition("file:///path/to/File.java", line=10, character=5)

client.close_document("file:///path/to/File.java")
client.stop_server()
```

The module provides:
- `LspBridgeClient` — a Protocol for dependency injection
- `RequestsLspBridgeClient` — concrete implementation using HTTP requests
- `DocumentSymbol`, `Location` — frozen dataclasses for typed LSP responses

### Call-Flow Extraction

Extract a method call tree from a source file using LSP go-to-definition. The `call_flow` module walks calls recursively starting from an entry method, resolving each call site via the language server:

```python
from repo_surveyor.lsp_bridge import RequestsLspBridgeClient
from repo_surveyor.call_flow import extract_call_tree, format_call_tree

client = RequestsLspBridgeClient("http://localhost:3000")
client.start_server("java", "file:///path/to/project")
client.open_document(file_uri, "java", source_text)

call_tree = extract_call_tree(client, file_uri, "/path/to/File.java", source_text, "layout", "java")
print(format_call_tree(call_tree))

# call_tree.edges is a dict[str, frozenset[str]] mapping caller → callees
for caller, callees in sorted(call_tree.edges.items()):
    for callee in sorted(callees):
        print(f"  {caller}() -> {callee}()")
```

The module provides:
- `extract_call_tree()` — traces calls from an entry method, returns a frozen `CallTree`
- `format_call_tree()` — renders a `CallTree` as an indented string
- `CallTree` — frozen dataclass with `entry_method` and `edges`

See `examples/extract_java_flow.py` for a full end-to-end example using JDTLS.

### Persisting to Neo4j

Persist analysis results to a Neo4j graph database:

```python
from repo_surveyor import RepoSurveyor
from repo_surveyor.analysis_graph_builder import create_analysis_graph_builder

surveyor = RepoSurveyor("/path/to/repo")
report = surveyor.tech_stacks()
structure = surveyor.coarse_structure(languages=["Java"])

with create_analysis_graph_builder("bolt://localhost:7687", "neo4j", "password") as builder:
    builder.persist_tech_stacks(report)
    builder.persist_coarse_structure(structure, "/path/to/repo")
```

This creates a graph with:
- `Repository` nodes
- `Directory` nodes with hierarchy relationships
- `Language`, `PackageManager`, `Framework`, `Infrastructure` nodes
- `CodeSymbol` nodes with parent-child relationships based on scope

## Technical Documentation

This section describes the internal code flows and architecture of each major subsystem.

### Architecture Overview

```
languages.json  (declarative: languages, indicators, parsers, framework patterns, infrastructure)
      │
      ▼
PluginRegistry  (loads JSON, builds lookup tables, resolves shared patterns)
      │
      ├──► detectors.py        (INDICATOR_FILES, EXTENSION_LANGUAGES, FRAMEWORK_PATTERNS,
      │                          GLOB_PATTERNS, K8S_MARKERS derived from registry)
      │
      └──► integration_patterns/__init__.py  (EXTENSION_TO_LANGUAGE, LANGUAGE_MODULES
                                               derived from registry)

survey_and_persist()  (full analysis pipeline)
├── RepoSurveyor.tech_stacks()
│   ├── detectors.detect_indicator_files_with_directories()
│   │   └── package_parsers.parse_dependencies() → match_frameworks()
│   ├── detectors.detect_from_glob_patterns()
│   │   └── package_parsers.parse_dependencies() → match_frameworks()
│   ├── detectors.detect_kubernetes()
│   └── detectors.detect_languages_from_extensions()
│
├── RepoSurveyor.coarse_structure()
│   └── ctags.run_ctags()
│       ├── _build_ctags_command()
│       └── _parse_ctags_json_output()
│
├── integration_detector.detect_integrations()
│   │   (directory_frameworks built from tech_stacks() DirectoryMarkers)
│   ├── integration_patterns.get_patterns_for_language()
│   │   └── common + language base + framework-specific patterns
│   ├── scan_file_for_integrations()
│   └── classify_directory()
│
├── AnalysisGraphBuilder.persist_tech_stacks()
│   └── graph_builder.build_tech_stack_graph()
│
└── AnalysisGraphBuilder.persist_coarse_structure()
    └── graph_builder.build_coarse_structure_graph()

(standalone — not part of survey_and_persist)
├── call_flow.extract_call_tree()
│   ├── lsp_bridge.LspBridgeClient (Protocol)
│   │   └── RequestsLspBridgeClient (HTTP → mojo-lsp)
│   └── tree-sitter (source parsing)
```

### Plugin Architecture

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
1. If the language has a config file parser, add a module in `package_parsers/` and register it in `package_parsers/__init__.py`
2. If the language needs integration pattern detection, add a package directory in `integration_patterns/` with a `base.py` exporting a `BASE = BasePatternSpec(patterns={...})` instance for base patterns, and optional per-framework files (e.g. `spring.py`) each exporting a `FRAMEWORK = FrameworkPatternSpec(name="...", patterns={...})` instance. The language's `__init__.py` auto-discovers framework files via `pkgutil`. Also add a `Language` enum member in `integration_patterns/types.py`
3. Run `poetry run pytest` to verify

### Tech Stack Analysis: `RepoSurveyor.tech_stacks()`

**Entry point:** `surveyor.py` → `RepoSurveyor.tech_stacks()`

The tech stack analysis runs five detection stages in sequence, aggregating results into a `SurveyReport`:

**Stage 1 — Indicator file walk** (`detectors.py` → `detect_indicator_files_with_directories()`): Walks the repository directory tree (skipping `.git`, `node_modules`, `__pycache__`, etc.) and matches filenames against the `INDICATOR_FILES` dict (derived from `languages.json` via `PluginRegistry`). Each match (e.g. `pyproject.toml` → Python + Poetry, `pom.xml` → Java + Maven) produces a `DirectoryMarker` recording what was found and in which directory.

**Stage 2 — Framework detection from config files** (`detectors.py` → `detect_frameworks_for_file()`): For each indicator file found in Stage 1, the file's content is parsed for framework dependencies. This delegates to the package parser subsystem (see below). The detected frameworks are merged into the marker's `frameworks` list.

**Stage 3 — Glob pattern detection** (`detectors.py` → `detect_from_glob_patterns()`): Matches glob patterns like `*.csproj`, `*.sln`, and `*.tf` across the repo. Matched files are also parsed for framework dependencies via the package parser subsystem.

**Stage 4 — Kubernetes detection** (`detectors.py` → `detect_kubernetes()`): Scans all `.yaml`/`.yml` files for Kubernetes markers (`apiVersion:`, `kind: Deployment`, etc.). Returns a boolean.

**Stage 5 — Language extension scan** (`detectors.py` → `detect_languages_from_extensions()`): Walks the repo and maps file extensions (`.py`, `.java`, `.rs`, etc.) to language names using the `EXTENSION_LANGUAGES` dict. This catches languages not represented by indicator files (e.g. a `.go` file in a Python project).

The final `SurveyReport` (defined in `report.py`) contains sorted, deduplicated lists of languages, package managers, frameworks, infrastructure, and the per-directory `DirectoryMarker` list. `SurveyReport.to_text()` renders a human-readable plain text report.

#### Package Parser Subsystem

**Entry point:** `package_parsers/__init__.py` → `parse_dependencies(filename, content)`

The parser dispatcher maps exact filenames (e.g. `"pom.xml"`, `"package.json"`) to format-specific parsers, with extension-based fallback for variable names (e.g. `MyApp.csproj`). Each parser returns a list of `ParsedDependency(name, source)` objects. Supported formats:

| Parser module | File format | Technique |
|---|---|---|
| `pyproject_toml` | `pyproject.toml` | `tomllib` — reads PEP 621 `[project.dependencies]` and Poetry `[tool.poetry.dependencies]` |
| `requirements_txt` | `requirements.txt` | Line parsing — extracts PEP 508 package names |
| `pipfile` | `Pipfile` | `tomllib` — reads `[packages]` and `[dev-packages]` |
| `setup_py` | `setup.py` | Regex — extracts `install_requires=[...]` |
| `package_json` | `package.json` | `json` — reads `dependencies`, `devDependencies`, `peerDependencies` |
| `pom_xml` | `pom.xml` | `xml.etree` — extracts `<artifactId>` from `<dependency>` elements |
| `build_gradle` | `build.gradle` / `.kts` | Regex — extracts `implementation`, `api`, `compile` declarations |
| `go_mod` | `go.mod` | Line parsing — extracts module paths from `require` blocks |
| `cargo_toml` | `Cargo.toml` | `tomllib` — reads `[dependencies]`, `[dev-dependencies]`, `[build-dependencies]` |
| `csproj` | `*.csproj` | `xml.etree` — extracts `<PackageReference Include="...">` attributes |
| `packages_config` | `packages.config` | `xml.etree` — extracts `<package id="...">` attributes |

**Framework matching** (`_dep_matches_pattern()` + `match_frameworks()`): Parsed dependency names are matched against the `FRAMEWORK_PATTERNS` dict using four rules tried in order:

1. **Exact match**: `"fastapi"` == `"fastapi"`
2. **Prefix-separator**: `"spring-boot-starter-web"` starts with `"spring-boot-"` → match
3. **Path subsequence**: `"github.com/gin-gonic/gin"` contains `"/gin-gonic/gin/"` → match
4. **npm scoped**: `"@nestjs/core"` → scope `"nestjs"` or name `"core"` matches pattern

### Code Structure Extraction: `RepoSurveyor.coarse_structure()`

**Entry point:** `surveyor.py` → `RepoSurveyor.coarse_structure()` → `ctags.py` → `run_ctags()`

This subsystem extracts code symbols (classes, methods, fields, functions, etc.) by shelling out to Universal CTags.

**Step 1 — Build command** (`ctags.py` → `_build_ctags_command()`): Constructs a `ctags` CLI invocation from a `CTagsConfig` object:
```
ctags --output-format=json --fields=* --extras=+q -R
      [--languages=Java,Python] [--exclude=.git] [--exclude=target] ...
```

**Step 2 — Execute** (`ctags.py` → `run_ctags()`): Runs ctags as a subprocess with a 300-second timeout. Captures stdout (JSON Lines) and stderr.

**Step 3 — Parse** (`ctags.py` → `_parse_ctags_json_output()`): Reads the JSON Lines output line by line. Each line with `"_type": "tag"` is converted into a `CTagsEntry` via `CTagsEntry.from_json()`. Metadata lines are skipped.

The result is a `CTagsResult` containing a list of `CTagsEntry` objects (each with `name`, `path`, `kind`, `line`, `scope`, `scope_kind`, `signature`, `language`), the raw output, the return code, and a `success` property.

### Integration Point Detection: `detect_integrations()`

**Entry point:** `integration_detector.py` → `detect_integrations()`

This subsystem scans source files for regex patterns that indicate system integration points (HTTP/REST, SOAP, messaging, sockets, databases, file I/O, gRPC, GraphQL, email, caching, SSE/streaming, scheduling).

#### Pattern organisation

Patterns are organised in three layers, merged by `get_patterns_for_language()` in `integration_patterns/__init__.py`:

```
For each IntegrationType:
  1. Common patterns (common.py → COMMON: BasePatternSpec) — low-confidence, language-agnostic
     e.g. (?i)\bhttp\b, (?i)\bkafka\b
  2. Language base patterns (java/base.py → BASE: BasePatternSpec) — always active for that language
     e.g. @Entity, import requests, std::fs
  3. Framework-specific patterns (java/spring.py → FRAMEWORK: FrameworkPatternSpec) — active only when framework detected in directory
     e.g. @RestController (Spring), @app.get (FastAPI)
```

Each pattern is a tuple of `(regex, Confidence)` where Confidence is HIGH, MEDIUM, or LOW. The `BasePatternSpec` and `FrameworkPatternSpec` frozen dataclasses (defined in `types.py`) enforce an explicit type contract and immutability for all pattern definitions.

#### File scanning flow

1. **File iteration** (`_get_source_files()`): Walks the repo, filters by file extension (using `EXTENSION_TO_LANGUAGE`), and skips excluded directories.

2. **Framework resolution** (`_find_frameworks_for_file()`): For each file, walks up the directory hierarchy looking up entries in the `directory_frameworks` mapping. This allows a file in `backend/api/routes.py` to inherit frameworks declared for `"backend"` or `"."`.

3. **Line-by-line scanning** (`scan_file_for_integrations()`): Reads the file, determines the language from the extension, calls `get_patterns_for_language(language, frameworks)` to get the merged pattern set, then tests each line against each regex. Every match yields an `IntegrationSignal` with the match location, integration type, confidence, matched pattern, and entity type.

4. **Directory classification** (`classify_directory()`): After scanning files, directory names themselves are matched against the directory patterns from `common.py` (e.g. `controllers` → HTTP_REST, `proto` → GRPC). These produce directory-level `IntegrationSignal` entries.

The result is an `IntegrationDetectorResult` containing all `IntegrationSignal` instances and the count of files scanned.

### Call Flow Extraction: `extract_call_tree()`

**Entry point:** `call_flow/extractor.py` → `extract_call_tree()`

This subsystem traces method call hierarchies within a single file using two tools: tree-sitter for identifying call sites, and LSP go-to-definition for resolving what each call site refers to.

**Step 1 — Parse the source** with tree-sitter to build a syntax tree. A language-specific query (currently only Java: `"(method_invocation name: (identifier) @name)"`) is used to locate call expressions.

**Step 2 — Get document symbols** via the LSP bridge (`client.get_symbols()`). This returns a tree of `DocumentSymbol` objects representing methods, constructors, and functions in the file.

**Step 3 — Build a method map** (`_build_method_map()`): Recursively collects all method-like symbols (LSP SymbolKinds 6, 9, 12) into a dict mapping method name → list of `{start, end}` line ranges. Parameter signatures are stripped (e.g. `"layout(String)"` → `"layout"`).

**Step 4 — Walk the call tree** starting from the entry method. For each method:
- Use the tree-sitter query to extract call identifiers within the method's line range (`_extract_call_identifiers()`)
- For each identifier, call `client.get_definition()` to resolve where it's defined
- Map the definition line back to a method name using `_find_method_containing()`
- If the callee is a different method in the same file, record the edge and recurse into it
- Cycle detection prevents infinite recursion

The result is a frozen `CallTree` (defined in `call_flow/types.py`) with an `entry_method` and `edges: dict[str, frozenset[str]]` mapping callers to callees. `format_call_tree()` renders this as an indented tree string with recursion markers.

**Limitations**: Currently only supports Java. Only traces calls within a single file (cross-file calls are ignored).

### LSP Bridge

**Protocol:** `lsp_bridge/client_protocol.py` → `LspBridgeClient` (a `typing.Protocol`)

The LSP bridge provides access to Language Server Protocol operations through an HTTP REST interface (the [mojo-lsp](https://github.com/avishek-sen-gupta/mojo-lsp) bridge server).

The `LspBridgeClient` protocol defines six operations: `start_server`, `stop_server`, `open_document`, `close_document`, `get_symbols`, and `get_definition`. Using a Protocol rather than an abstract base class allows any object with matching method signatures to satisfy the interface — this enables test doubles without inheritance.

**Concrete implementation:** `lsp_bridge/requests_client.py` → `RequestsLspBridgeClient`

Each method is a POST to the mojo-lsp bridge:

| Method | Endpoint | Request body | Response |
|---|---|---|---|
| `start_server()` | `POST /start` | `{language, rootUri}` | Server config dict |
| `stop_server()` | `POST /stop` | `{}` | — |
| `open_document()` | `POST /document/open` | `{uri, languageId, text}` | — |
| `close_document()` | `POST /document/close` | `{uri}` | — |
| `get_symbols()` | `POST /symbols` | `{uri}` | `{symbols: [...]}` → `list[DocumentSymbol]` |
| `get_definition()` | `POST /definition` | `{uri, line, character}` | `{locations: [...]}` → `list[Location]` |

Response JSON is parsed into frozen dataclasses (`DocumentSymbol`, `Location`) defined in `lsp_bridge/types.py`. `DocumentSymbol` has recursive `children` (a tuple of `DocumentSymbol`) representing the symbol tree.

### Neo4j Persistence: `AnalysisGraphBuilder`

**Entry point:** `analysis_graph_builder.py` → `AnalysisGraphBuilder`

This subsystem persists tech stack and code structure analysis results into a Neo4j graph database. It uses Protocol-based dependency injection (`Neo4jDriver` protocol) for testability.

#### Tech stack persistence: `persist_tech_stacks(report)`

**Step 1 — Build graph data** (`graph_builder.py` → `build_tech_stack_graph()`): Transforms the `SurveyReport` into four outputs:
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

#### Code structure persistence: `persist_coarse_structure(result, repo_path)`

**Step 1 — Build graph data** (`graph_builder.py` → `build_coarse_structure_graph()`):
- `_index_symbols()`: Creates a unique ID for each symbol (`{path}:{name}:{kind}:{line}`) and extracts the package name using language-aware heuristics (e.g. Java `src/main/java/com/example/` → `com.example`). Builds a `(path, name)` index for parent lookup.
- `_resolve_relationships()`: For each symbol with a `scope`, looks up the parent symbol by matching `(path, scope_name)` and optionally `scope_kind`. Records `{child_id, parent_id}` relationships.
- Symbols with no resolved parent become `top_level_symbols`.

**Step 2 — Execute Cypher**: Creates nodes and relationships in three batched queries:
1. `UNWIND $symbols ... CREATE (s:CodeSymbol {...})`
2. `UNWIND $relationships ... CREATE (parent)-[:CONTAINS]->(child)`
3. `UNWIND $top_level ... CREATE (r:Repository)-[:CONTAINS]->(s:CodeSymbol)`

A convenience function `survey_and_persist()` orchestrates the full pipeline: runs `tech_stacks()`, `coarse_structure()`, and `detect_integrations()` (using the per-directory framework mappings from tech stack detection), then persists tech stacks and code structure to Neo4j.

### Design Patterns

The codebase uses several consistent patterns:

- **Protocol-based dependency injection**: External systems (Neo4j, LSP bridge) are accessed through `typing.Protocol` interfaces, with concrete implementations injected at construction time. This avoids hardcoded imports and enables testing with mock objects.
- **Frozen dataclasses**: All data transfer objects (`IntegrationSignal`, `FileMatch`, `CallTree`, `DocumentSymbol`, `Location`) and pattern specifications (`BasePatternSpec`, `FrameworkPatternSpec`) are frozen to prevent mutation after construction.
- **Pure graph-building functions**: `graph_builder.py` contains pure functions that transform survey data into graph representations without side effects — the actual database writes are in `analysis_graph_builder.py`.
- **Layered pattern merging**: Integration patterns are composed from three layers (common → language base → framework-specific) at query time, keeping each layer independently editable.

## Running Tests

```bash
poetry run pytest
```

## License

This project is licensed under the MIT License — see [LICENSE.md](LICENSE.md) for details.
