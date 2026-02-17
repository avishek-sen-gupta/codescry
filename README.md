# Cartographer

A Python library for experiments in analysing repository technology stacks and code structure.

**Note:** Most of this codebase was vibe-coded.

## Features

- Detects programming languages, package managers, frameworks, and infrastructure
- Structured parsing of config files (package.json, pyproject.toml, pom.xml, .csproj, etc.) for accurate framework detection — no false positives from substring matching
- Associates technologies with their containing directories (useful for monorepos)
- Extracts code symbols using Universal CTags
- Detects system integration points (HTTP/REST, SOAP, messaging, sockets, databases, FTP/SFTP, GraphQL, email, caching, SSE/streaming, scheduling) with framework-aware pattern matching and tree-sitter syntax zone filtering (skips comments and string literals to reduce false positives)
- Resolves integration signals to their containing code symbols (via CTags line ranges), producing per-symbol integration profiles
- Extracts method call trees via [mojo-lsp](https://github.com/avishek-sen-gupta/mojo-lsp) LSP bridge and tree-sitter
- Persists analysis results to Neo4j graph database
- Generates plain text and JSON reports
- Pipeline timing observer for measuring stage durations as JSON

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
| C++ | `.cpp`, `.hpp` | `CMakeLists.txt`, `vcpkg.json`, `conanfile.txt` | CMake, vcpkg, Conan | Qt, Boost, Crow, Drogon, POCO |
| C | `.c`, `.h` | _(shared with C++)_ | _(shared with C++)_ | _(shared with C++)_ |
| COBOL | `.cbl`, `.cob`, `.cpy` | — | — | — |
| PL/I | `.pli`, `.pl1`, `.plinc` | — | — | — |
| Ruby | `.rb` | `Gemfile` | Bundler | Rails, Sinatra, Hanami |

Infrastructure detection: Docker (`Dockerfile`, `docker-compose.yml`), Terraform (`*.tf`), Kubernetes (`*.yaml` with k8s markers).

Framework detection uses structured parsing of config files rather than naive substring matching — see [Package Parser Subsystem](#package-parser-subsystem) for parser details. This prevents false positives like `"reactive-streams"` matching `"react"` or `"expression"` matching `"express"`.

## Supported Integration Types

| Type | Description | Example Patterns |
|------|-------------|------------------|
| `http_rest` | HTTP/REST endpoints and clients | `@RestController`, `@GetMapping`, `app.get(`, `use actix_web::`, `curl_easy_init` |
| `soap` | SOAP/XML web services | `@WebService`, `SOAPMessage`, `XML PARSE`, `soap_init` |
| `messaging` | Message queues and event buses | `@KafkaListener`, `@RabbitListener`, `MQPUT`, `SqsClient`, `rd_kafka_new` |
| `socket` | Raw sockets and WebSockets | `@ServerEndpoint`, `WebSocket`, `TcpListener`, `sys/socket.h` |
| `database` | Database connections and ORMs | `@Repository`, `@Entity`, `EXEC SQL`, `DynamoDbClient`, `ActiveRecord::Base`, `has_many`, `PanacheEntity`, `db.Model`, `SQLModel`, `TypeOrmModule`, `AddDbContext`, `OrmLite`, `sqlite3_open`, `GraphDatabase.driver` (Neo4j), `neo4j-driver`, `neo4rs`, `ActiveGraph::Node` |
| `file_io` | File I/O, uploads, and cloud storage | `FileInputStream`, `open(`, `os.Open`, `Azure.Storage.Blobs`, `std::fstream` |
| `grpc` | gRPC services and clients | `io.grpc`, `import grpc`, `tonic::`, `Grpc.Core`, `grpc::ServerBuilder` |
| `graphql` | GraphQL APIs and schemas | `@QueryMapping`, `apollo-server`, `graphene`, `HotChocolate` |
| `email` | Email and SMTP services | `javax.mail`, `smtplib`, `nodemailer`, `MailKit`, `SMTPClientSession`, `mailio::smtp`, `vmime::net::smtp` |
| `caching` | Cache stores and distributed caching | `@Cacheable`, `RedisTemplate`, `ioredis`, `IDistributedCache`, `hiredis` |
| `sse_streaming` | Server-sent events and streaming | `SseEmitter`, `StreamingResponse`, `EventSource`, `Sse<` |
| `ftp_sftp` | FTP/SFTP file transfer connections | `import ftplib`, `import paramiko`, `ssh2-sftp-client`, `FTPClient`, `JSch`, `SftpClient`, `Net::SFTP` |
| `scheduling` | Scheduled tasks and cron jobs | `@Scheduled`, `node-cron`, `Hangfire`, `robfig/cron`, `timer_create` |

### Integration Pattern Coverage by Language

| Integration Type | Java | Python | TypeScript | JavaScript | Go | Rust | C# | C/C++ | Ruby | COBOL | PL/I |
|------------------|:----:|:------:|:----------:|:----------:|:--:|:----:|:--:|:-----:|:----:|:-----:|:----:|
| `http_rest` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `soap` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `messaging` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `socket` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `database` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `file_io` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `grpc` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — |
| `graphql` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | ✓ | — | — |
| `email` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — |
| `caching` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — |
| `sse_streaming` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | ✓ | — | — |
| `ftp_sftp` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — |
| `scheduling` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — |

All 13 integration types have language-specific patterns for Java, Python, TypeScript, JavaScript, Go, Rust, C#, and Ruby. C and C++ share a common pattern module covering 11 base types (database, HTTP, messaging, sockets, gRPC, file I/O, FTP/SFTP, caching, SOAP, scheduling, email) with framework-specific patterns for Qt, Boost, POCO, Crow, and Drogon. COBOL and PL/I cover the 6 core types relevant to mainframe systems. Language-agnostic common patterns also apply across all files.

## Installation

```bash
poetry install
```

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

### Full Analysis Pipeline (without Neo4j)

Run tech stack detection, code structure extraction, integration point detection, and symbol resolution in a single call:

```python
from repo_surveyor import survey

tech_report, structure_result, integration_result, resolution = survey("/path/to/repo", languages=["Java"])

print(tech_report.to_text())
print(f"Symbols: {len(structure_result.entries)}")
print(f"Integration points: {len(integration_result.integration_points)}")
print(f"Resolved to symbols: {len(resolution.resolved)}")
print(f"Symbol profiles: {len(resolution.profiles)}")
```

Skip additional directories (e.g. test directories) from both tech stack and integration scanning:

```python
tech_report, structure_result, integration_result, resolution = survey(
    "/path/to/repo", extra_skip_dirs=["test", "tests"]
)
```

The `languages` parameter filters both CTags symbol extraction and integration detection, so only files of the requested languages are scanned across the entire pipeline. It accepts CTags language name strings (e.g., `"Java"`) and/or `Language` enum members (e.g., `Language.JAVA`) — they can be mixed freely. Strings that match a `Language` enum value are normalised automatically; strings without a matching enum (e.g., `"Awk"`) are passed to CTags only and silently skipped by integration detection.

The `survey()` function automatically wires detected frameworks from `tech_stacks()` into `detect_integrations()` via the `directory_frameworks` mapping, so framework-specific patterns are applied in the right directories. The `extra_skip_dirs` parameter is propagated to both `tech_stacks()` and `detect_integrations()`. After integration detection, `resolve_integration_signals()` joins each signal to its containing code symbol using CTags line ranges, producing per-symbol integration profiles. For Neo4j persistence, use `survey_and_persist()` instead.

### Pipeline Timing

Pass a `PipelineTimingObserver` to `survey()` or `survey_and_persist()` to measure how long each stage takes:

```python
from repo_surveyor import survey, PipelineTimingObserver

timer = PipelineTimingObserver()
tech_report, structure_result, integration_result, resolution = survey("/path/to/repo", timer=timer)

print(timer.to_json())
```

Example output:

```json
{
  "stages": [
    {"stage": "tech_stacks", "start_time": "2026-02-16T02:53:09.338835+05:30", "end_time": "2026-02-16T02:53:09.475769+05:30", "duration_seconds": 0.137},
    {"stage": "coarse_structure", "start_time": "2026-02-16T02:53:09.475807+05:30", "end_time": "2026-02-16T02:53:47.705246+05:30", "duration_seconds": 38.229},
    {"stage": "integration_detection.import_gating", "start_time": "2026-02-16T02:53:47.705356+05:30", "end_time": "2026-02-16T02:53:47.950000+05:30", "duration_seconds": 0.245},
    {"stage": "integration_detection.file_scanning", "start_time": "2026-02-16T02:53:47.950010+05:30", "end_time": "2026-02-16T02:53:48.484939+05:30", "duration_seconds": 0.535},
    {"stage": "integration_detection.directory_classification", "start_time": "2026-02-16T02:53:48.484950+05:30", "end_time": "2026-02-16T02:53:48.527112+05:30", "duration_seconds": 0.042},
    {"stage": "integration_detection", "start_time": "2026-02-16T02:53:47.705280+05:30", "end_time": "2026-02-16T02:53:48.527123+05:30", "duration_seconds": 0.822},
    {"stage": "symbol_resolution", "start_time": "2026-02-16T02:53:48.527200+05:30", "end_time": "2026-02-16T02:53:48.528100+05:30", "duration_seconds": 0.001}
  ],
  "total_seconds": 39.189
}
```

Sub-stages use dot-notation (e.g. `integration_detection.file_scanning`). The `total_seconds` field sums only top-level stages to avoid double-counting. A `NullPipelineTimer` no-op is used by default when no observer is provided.

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
- `name`, `path`, `kind`, `line`, `end` (end line of the symbol, if available)
- `scope`, `scope_kind` (containing class/function)
- `signature` (for methods)
- `language`

Requires [Universal CTags](https://github.com/universal-ctags/ctags) to be installed.

### Integration Point Detection

Detect system integration points in source code using pattern matching:

```python
from repo_surveyor import detect_integrations, Language

result = detect_integrations("/path/to/repo", languages=[Language.JAVA], extra_skip_dirs=["test"])

for point in result.integration_points:
    print(f"{point.integration_type.value}: {point.match.file_path}:{point.match.line_number}")
    print(f"  Pattern: {point.matched_pattern}")
    print(f"  Confidence: {point.confidence.value}")
    print(f"  Source: {point.source}")
```

Example output:

```
http_rest: /path/to/repo/src/UserController.java:15
  Pattern: @RestController
  Confidence: high
  Source: Spring
database: /path/to/repo/src/UserRepository.java:8
  Pattern: @Repository
  Confidence: high
  Source: Spring
messaging: /path/to/repo/src/OrderListener.java:12
  Pattern: @KafkaListener
  Confidence: high
  Source: Spring
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
      "source": "Spring",
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

#### Import Gating

Some frameworks have generic patterns (e.g. `\w*\.get\(`) that could match non-framework code like `Map.get()` or `List.get()`. To avoid false positives, these frameworks are **import-gated**: their patterns are only applied to files that contain a matching import statement. Frameworks with highly distinctive patterns (Spring annotations, Rails DSL, etc.) are ungated and apply to all files in the directory.

Import-gated frameworks:

| Framework | Required Import |
|-----------|----------------|
| Javalin | `import io.javalin` |
| Express (JS) | `require('express')` |
| Express (TS) | `from 'express'` |
| Sanic | `from sanic import` or `import sanic` |
| Hono | `from 'hono'` |

Base language patterns and common patterns always apply regardless of import gating.

### Symbol Resolution

After running the full pipeline, each integration signal is resolved to its containing code symbol using CTags line ranges. This produces per-symbol integration profiles showing which symbols expose endpoints, make database calls, etc.

```python
from repo_surveyor import survey

_, _, _, resolution = survey("/path/to/repo", languages=["Java"])

for profile in resolution.profiles:
    print(f"{profile.symbol_kind} {profile.symbol_name} in {profile.symbol_path}:")
    for si in profile.integrations:
        print(f"  {si.signal.integration_type.value} (line {si.signal.match.line_number})")

# Unresolved signals (directory-level or outside any symbol range)
print(f"Unresolved: {len(resolution.unresolved)}")
```

Results are also available as JSON via `to_json()`:

```python
print(resolution.to_json())
```

The resolver can also be called standalone with pre-computed results:

```python
from repo_surveyor import resolve_integration_signals

resolution = resolve_integration_signals(ctags_result, integration_result, "/path/to/repo")
```

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
- `CodeSymbol` nodes with parent-child relationships based on scope (each node has a `qualified_name` field combining scope and name, e.g. `MyClass.myMethod`)
- `IntegrationSignal` nodes (carrying confidence, matched line content, line number, and file path) linked from `CodeSymbol` via `HAS_INTEGRATION` and to `IntegrationType` nodes (e.g. `http_rest`, `database`) via `OF_TYPE`. Each signal has one or more `PatternMatch` child nodes (carrying matched pattern, confidence, and source) linked via `MATCHED_BY` — multiple matches for the same line/type/symbol are consolidated into a single signal with reinforcing pattern matches, and the signal's confidence is the highest among its contributing matches
- `UnresolvedIntegration` nodes for signals not resolved to any symbol, linked from `Repository` via `HAS_UNRESOLVED_INTEGRATION`

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

survey()             (full analysis pipeline, no persistence)
├── PipelineTimer (observer, optional — records stage durations)
├── (same stages as survey_and_persist, without Neo4j writes)

survey_and_persist()  (full analysis pipeline with Neo4j)
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
│   ├── _build_import_gated_framework_map()  (import gating sub-stage)
│   │   └── _filter_frameworks_by_imports() per file
│   ├── integration_patterns.get_patterns_for_language()
│   │   └── common + language base + framework-specific patterns
│   ├── scan_file_for_integrations()  (receives pre-gated frameworks)
│   │   └── syntax_zone.parse_file_zones()  (tree-sitter comment/string filtering)
│   └── classify_directory()
│
├── symbol_resolver.resolve_integration_signals()
│   ├── _SymbolLineIndex (spatial index from CTags entries)
│   └── joins signals to symbols by file path + line containment
│
├── AnalysisGraphBuilder.persist_tech_stacks()
│   └── graph_builder.build_tech_stack_graph()
│
├── AnalysisGraphBuilder.persist_coarse_structure()
│   └── graph_builder.build_coarse_structure_graph()
│
└── AnalysisGraphBuilder.persist_integrations()
    └── graph_builder.build_integration_graph()

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
2. If the language needs integration pattern detection, add a package directory in `integration_patterns/` with a `base.py` exporting a `BASE = BasePatternSpec(patterns={...})` instance for base patterns, and optional per-framework files (e.g. `spring.py`) each exporting a `FRAMEWORK = FrameworkPatternSpec(name="...", patterns={...})` instance. For frameworks with generic patterns that could cause false positives (e.g. `\w*\.get\(`), add `import_patterns=(r"import pattern",)` to gate those patterns on file-level imports. The language's `__init__.py` auto-discovers framework files via `pkgutil`. Also add a `Language` enum member in `integration_patterns/types.py`
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
| `vcpkg_json` | `vcpkg.json` | `json` — reads `dependencies` array (string or object entries) |
| `conanfile_txt` | `conanfile.txt` | Line parsing — extracts package names from `[requires]` section |

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

The result is a `CTagsResult` containing a list of `CTagsEntry` objects (each with `name`, `path`, `kind`, `line`, `end`, `scope`, `scope_kind`, `signature`, `language`), the raw output, the return code, and a `success` property. The `end` field captures the symbol's end line when CTags provides it (via `--fields=*`); it is `None` when not available.

### Integration Point Detection: `detect_integrations()`

**Entry point:** `integration_detector.py` → `detect_integrations()`

This subsystem scans source files for regex patterns that indicate system integration points (HTTP/REST, SOAP, messaging, sockets, databases, file I/O, FTP/SFTP, gRPC, GraphQL, email, caching, SSE/streaming, scheduling).

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

Each pattern is a tuple of `(regex, Confidence)` where Confidence is HIGH, MEDIUM, or LOW. The `BasePatternSpec` and `FrameworkPatternSpec` frozen dataclasses (defined in `types.py`) enforce an explicit type contract and immutability for all pattern definitions. `FrameworkPatternSpec` also carries an optional `import_patterns` tuple of regex strings; when non-empty, the framework's patterns are only applied to files whose content matches at least one import pattern (see [Import Gating](#import-gating)). When `get_patterns_for_language()` merges the three layers, each tuple is extended to `(regex, Confidence, source)` where `source` is `"common"`, the language display name (e.g. `"Java"`), or the framework name (e.g. `"Spring"`). This source label is propagated into the `IntegrationSignal.source` field and included in the JSON output.

#### File scanning flow

1. **File iteration** (`_get_source_files()`): Walks the repo, filters by file extension (using `EXTENSION_TO_LANGUAGE`), and skips excluded directories.

2. **Framework resolution** (`_find_frameworks_for_file()`): For each file, walks up the directory hierarchy looking up entries in the `directory_frameworks` mapping. This allows a file in `backend/api/routes.py` to inherit frameworks declared for `"backend"` or `"."`.

3. **Import gating** (`_build_import_gated_framework_map()`): A separate timed sub-stage (`integration_detection.import_gating`) that runs before file scanning. For each source file, reads the file content and filters the candidate framework list by checking whether the file contains an import for each gated framework (`_filter_frameworks_by_imports()`). Frameworks with empty `import_patterns` (ungated) pass through unconditionally. This prevents false positives like `Map.get()` matching as a Javalin HTTP route in files that don't import Javalin. The result is a `dict[Path, list[str]]` mapping each file to its import-gated frameworks.

4. **Syntax zone filtering** (`syntax_zone.py` → `parse_file_zones()`): Before regex matching, each file is parsed with tree-sitter to build a `SyntaxRangeMap` identifying which lines belong to one of four syntax zones:

   | Zone | Description | Filtered? |
   |------|-------------|-----------|
   | `CODE` | Default — executable source lines | No (scanned for patterns) |
   | `COMMENT` | Line comments, block comments | Yes (skipped) |
   | `STRING_LITERAL` | String literals, raw strings, template strings, heredocs | Yes (skipped) |
   | `IMPORT` | Import/use/using declarations | No (scanned for patterns) |

   Only `COMMENT` and `STRING_LITERAL` zones are filtered out. `IMPORT` lines are kept because import statements are a valuable signal for integration detection (e.g. `import requests`, `use actix_web`). Mixed-zone lines (code + trailing comment on the same line) are conservatively kept as `CODE`. Languages without tree-sitter support gracefully fall back to scanning all lines via a null-object empty range map (`SyntaxRangeMap(ranges=())`).

   Tree-sitter zone classification is supported for: Java, Python, TypeScript, JavaScript, Go, Rust, C#, Kotlin, Scala, Ruby, PHP, C, C++, and COBOL. Languages without tree-sitter support (PL/I) scan all lines without filtering.

5. **Line-by-line scanning** (`scan_file_for_integrations()`): Reads the file, determines the language from the extension, calls `get_patterns_for_language(language, frameworks)` with the pre-gated framework list to get the merged pattern set, then tests each non-comment/non-string line against each regex. Every match yields an `IntegrationSignal` with the match location, integration type, confidence, matched pattern, entity type, and source (which layer contributed the pattern).

6. **Directory classification** (`classify_directory()`): After scanning files, directory names themselves are matched against the directory patterns from `common.py` (e.g. `controllers` → HTTP_REST, `proto` → GRPC). These produce directory-level `IntegrationSignal` entries.

The result is an `IntegrationDetectorResult` containing all `IntegrationSignal` instances and the count of files scanned.

### Symbol Resolution: `resolve_integration_signals()`

**Entry point:** `symbol_resolver.py` → `resolve_integration_signals()`

This subsystem joins integration signals to their containing code symbols, producing per-symbol integration profiles. It is a pure function module with no side effects or I/O.

**Step 1 — Build spatial index** (`_SymbolLineIndex`): Groups `CTagsEntry` objects by file path and builds a list of `_SymbolSpan` (start line, end line, symbol ID, name, kind) per file. Symbols with an explicit `end` field (from CTags `--fields=*`) use it directly; symbols without `end` use the next symbol's start line minus one as a heuristic boundary.

**Step 2 — Resolve signals**: For each `IntegrationSignal`:
- Signals with `entity_type == DIRECTORY` go directly to `unresolved` (directory-level, not symbol-level)
- For `FILE_CONTENT` signals, the absolute file path is normalised to a relative path by stripping the `repo_path` prefix
- The `(relative_path, line_number)` pair is looked up in the spatial index
- The **most specific** (narrowest span) containing symbol is selected
- If found → `SymbolIntegration`; if no symbol contains the line → `unresolved`

**Step 3 — Group into profiles**: Resolved integrations are grouped by `symbol_id` into `SymbolIntegrationProfile` objects, each listing all integration signals for that symbol.

The result is a `ResolutionResult` containing:
- `resolved`: tuple of `SymbolIntegration` instances
- `unresolved`: tuple of `IntegrationSignal` instances (directory-level or outside any symbol range)
- `profiles`: tuple of `SymbolIntegrationProfile` instances (grouped by symbol)

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
- `_index_symbols()`: Creates a unique ID for each symbol (`{path}:{name}:{kind}:{line}`), computes a `qualified_name` by prepending the scope (e.g. `MyClass.myMethod`, or just `MyClass` for top-level symbols), and extracts the package name using language-aware heuristics (e.g. Java `src/main/java/com/example/` → `com.example`). Builds a `(path, name)` index for parent lookup.
- `_resolve_relationships()`: For each symbol with a `scope`, looks up the parent symbol by matching `(path, scope_name)` and optionally `scope_kind`. Records `{child_id, parent_id}` relationships.
- Symbols with no resolved parent become `top_level_symbols`.

**Step 2 — Execute Cypher**: Creates nodes and relationships in three batched queries:
1. `UNWIND $symbols ... CREATE (s:CodeSymbol {...})`
2. `UNWIND $relationships ... CREATE (parent)-[:CONTAINS]->(child)`
3. `UNWIND $top_level ... CREATE (r:Repository)-[:CONTAINS]->(s:CodeSymbol)`

#### Integration persistence: `persist_integrations(resolution, integration_result, repo_path)`

**Step 1 — Build graph data** (`graph_builder.py` → `build_integration_graph()`): Extracts unique integration type names, builds flat resolved dicts from each signal, then consolidates them by `(symbol_id, file_path, line_number, integration_type)`. Each consolidated dict contains a `pattern_matches` list and a `confidence` set to the highest among contributing matches. Unresolved integration dicts are built separately (integration_type, confidence, matched_pattern, entity_type, file_path, line_number, line_content, source).

**Step 2 — Execute Cypher**: Creates nodes and relationships in up to three batched queries:
1. `UNWIND $names ... MERGE (t:IntegrationType {name})` — idempotent creation of integration type nodes
2. `UNWIND $integrations ... MATCH (CodeSymbol) MATCH (IntegrationType) CREATE (sig:IntegrationSignal {...}) CREATE (s)-[:HAS_INTEGRATION]->(sig) CREATE (sig)-[:OF_TYPE]->(t) WITH sig, i UNWIND i.pattern_matches AS pm CREATE (m:PatternMatch {...}) CREATE (sig)-[:MATCHED_BY]->(m)` — creates IntegrationSignal nodes with confidence, line content, line number, and file path, linked from symbols and to their integration types, with child PatternMatch nodes (matched_pattern, confidence, source) linked via MATCHED_BY
3. `UNWIND $signals ... MATCH (Repository) CREATE (u:UnresolvedIntegration {...}) CREATE (r)-[:HAS_UNRESOLVED_INTEGRATION]->(u)` — persists signals not resolved to any symbol

A convenience function `survey_and_persist()` orchestrates the full pipeline: runs `tech_stacks()`, `coarse_structure()`, `detect_integrations()` (using the per-directory framework mappings from tech stack detection), and `resolve_integration_signals()`, then persists tech stacks, code structure, and integration results to Neo4j. Like `survey()`, its `languages` parameter filters both CTags and integration detection and accepts both `str` and `Language` enum values.

### CFG Constructor: Role Schema

The `cfg_constructor` module provides a language-independent foundation for building control flow graphs (CFGs) from tree-sitter parse trees. Rather than writing a CFG builder per language, the approach uses a **control flow role schema** — a small enum of structural roles that map to CFG construction rules.

For each language, an LLM classifies tree-sitter node types into these roles, producing a static mapping. The CFG builder (not yet implemented) will dispatch on roles, not node type names.

#### Role Schema

| Role | CFG Semantics | Examples |
|------|--------------|----------|
| `SEQUENCE` | Ordered children execute one after another | `block`, `program`, `statement_list` |
| `BRANCH` | Condition selects one of N bodies | `if_statement`, `ternary_expression` |
| `SWITCH` | Multi-arm dispatch, possible fallthrough | `switch_statement`, `match_expression` |
| `LOOP` | Body may re-execute based on condition | `while_statement`, `for_statement` |
| `LOOP_POST_CONDITION` | Body executes at least once, then condition checked | `do_statement` |
| `RETURN` | Terminates function, transfers to caller | `return_statement` |
| `BREAK` | Exits enclosing loop/switch | `break_statement` |
| `CONTINUE` | Skips to next loop iteration | `continue_statement` |
| `THROW` | Transfers to nearest exception handler | `throw_statement`, `raise` |
| `TRY` | Introduces exception-handling boundary with body + handlers + optional finally | `try_statement` |
| `CALL` | Function/method invocation (sequential for now; hook point for inter-procedural edges) | `method_invocation`, `call_expression` |
| `LEAF` | No control flow effect (default for unmapped nodes) | expressions, assignments, declarations |

#### Types

- **`ControlFlowRole`** — enum with 12 members as listed above
- **`LanguageCFGSpec`** — frozen dataclass pairing a `Language` with a `dict[str, ControlFlowRole]` mapping tree-sitter node type strings to roles. Includes a `role_for(node_type)` helper that returns `LEAF` for unmapped types

#### Grammar Classifier (Exploratory)

The `grammar_classifier` module provides an LLM-based approach to classifying tree-sitter grammar node types into `ControlFlowRole` values. Instead of manually writing role mappings per language, it extracts the named node type names from a tree-sitter `grammar.js` file, sends that compact list to a local LLM (Qwen2.5-Coder 7B via Ollama), and parses the `NODE_TYPE|ROLE` response into a `dict[str, ControlFlowRole]`.

Components:
- **`js/extract_rules.js`** — Node.js stub script that intercepts the `grammar()` call in a tree-sitter `grammar.js` file and outputs the rule names as a JSON array. This is more robust than regex extraction because it executes the actual grammar definition, correctly handling any formatting, comments, or computed rule names
- **`extract_node_types(grammar_js)`** — writes the grammar content to a temp file, runs `js/extract_rules.js` via Node.js, and returns the sorted list of rule names
- **`SYSTEM_PROMPT`** — instructs the model to classify named node types into one of the 12 control flow roles, with definitions and examples for each role
- **`build_user_prompt(node_types)`** — formats a list of node type names as a compact user prompt
- **`parse_classification_response(text)`** — parses `NODE_TYPE|ROLE` lines from model output, skipping malformed lines and performing case-insensitive role matching

The classifier reuses the existing `LineClassifierModel` protocol and `QwenClassifierModel` from the `ml_classifier` module. Requires Node.js for rule extraction. This is exploratory — results depend on model capability and prompt tuning.

#### Static Config: Per-Language `cfg_roles.json`

The generator script classifies all 220 languages from tree-sitter-language-pack once and produces a monolithic intermediate file. The per-language role mappings are then stored as individual `cfg_roles.json` files inside each language's `integration_patterns/{lang}/` directory. Each file contains a flat mapping from tree-sitter node type to role string:

```json
{"if_statement": "branch", "while_statement": "loop", "method_invocation": "call"}
```

Languages with per-language cfg_roles.json: Java, Python, JavaScript, Go, Ruby, Rust, COBOL.

To regenerate (requires Ollama with the Qwen model running):

```bash
poetry run python scripts/generate_cfg_roles.py
poetry run python scripts/generate_cfg_roles.py --model qwen2.5-coder:7b-instruct
poetry run python scripts/generate_cfg_roles.py --split  # also distribute to per-language dirs
```

The script is incremental — it skips languages already in `languages` or `failures` and saves after every language for crash safety. The `--split` flag distributes the monolithic output into per-language `cfg_roles.json` files under `integration_patterns/{lang}/`.

#### Loader API: `cfg_role_registry`

The loader module reads per-language `cfg_roles.json` files at runtime with zero ML or Node.js dependencies:

- **`load_cfg_roles(patterns_dir)`** — scans each `Language` enum member's directory for a `cfg_roles.json`, returns `dict[Language, LanguageCFGSpec]`
- **`get_cfg_spec(language, patterns_dir)`** — returns the spec for a single language; returns an empty null-object spec (all nodes map to `LEAF`) if not found

```python
from repo_surveyor.cfg_constructor import load_cfg_roles, get_cfg_spec
from repo_surveyor.integration_patterns.types import Language

# Load all available specs
specs = load_cfg_roles()
java_spec = specs[Language.JAVA]
print(java_spec.role_for("if_statement"))  # ControlFlowRole.BRANCH

# Or load a single language
spec = get_cfg_spec(Language.PYTHON)
print(spec.role_for("for_statement"))  # ControlFlowRole.LOOP
print(spec.role_for("unknown_node"))   # ControlFlowRole.LEAF
```

`Language` enum members are mapped to their `integration_patterns/` directory names via an internal `_LANG_TO_DIR` dict (e.g. `Language.CSHARP` → `"csharp"`, `Language.CPP` → `"cpp"`).

#### Design Decisions

- `BREAK`/`CONTINUE`/`RETURN`/`THROW` are separate roles (not a single `EXIT`) because they target different scopes and produce different CFG edges
- `SWITCH` is separate from `BRANCH` — fallthrough semantics (C/Java `switch` vs Rust `match`) change edge structure
- `LOOP_POST_CONDITION` is separate from `LOOP` — `do...while` guarantees at least one body execution, changing the entry edge
- `CALL` is treated as sequential for now, marking call sites for future inter-procedural expansion
- `LEAF` is the safe default — unclassified nodes produce sequential edges
- No field hints in the spec — the CFG builder will discover child field names dynamically from the tree-sitter node at runtime

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

Every test run automatically measures code coverage and prints a per-file summary with missing line numbers. An HTML report is also generated:

```bash
open htmlcov/index.html   # browse detailed coverage report
```

Coverage is configured in `pyproject.toml` (`[tool.coverage.*]` sections). The `call_flow/`, `lsp_bridge/`, and `ml_classifier/` packages are excluded because they depend on external services.

### Local-repo integration tests

Tests that depend on local repository clones (`~/code/mojo-lsp`, `~/code/smojol`) are marked with `@pytest.mark.local_repo`. These are automatically skipped when the `CI` environment variable is set (GitHub Actions sets this by default). Equivalent code paths are covered by synthetic `tmp_path`-based unit tests that run everywhere.

To run only the local-repo tests locally:

```bash
poetry run pytest -m local_repo
```

### Ollama LLM tests

Tests that require a running Ollama instance are marked with `@pytest.mark.ollama`. These are automatically skipped when Ollama is not available (try-import guard) and in CI. To run them locally:

```bash
poetry run pytest -m ollama
```

## CI

A GitHub Actions workflow is defined in `.github/workflows/ci.yml` with two jobs:

- **lint** — runs `black --check` on `src/` and `tests/`
- **test** — installs Universal CTags and runs `pytest` with coverage; `local_repo`-marked tests are auto-skipped via the `CI` environment variable

The pipeline runs on pushes to `main` and on pull requests targeting `main`. It can also be triggered manually via `workflow_dispatch`.

## License

This project is licensed under the MIT License — see [LICENSE.md](LICENSE.md) for details.
