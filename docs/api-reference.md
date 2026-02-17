# API Reference

## Tech Stack Analysis

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

## Full Analysis Pipeline (without Neo4j)

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

## Pipeline Timing

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

## Code Structure Analysis with CTags

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

## Integration Point Detection

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

### Framework-Aware Detection

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

### Import Gating

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

## Symbol Resolution

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

## LSP Bridge Client

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

## Call-Flow Extraction

Extract a method call tree from a source file using LSP go-to-definition. The `call_flow` module walks calls recursively starting from an entry method, resolving each call site via the language server:

```python
from repo_surveyor.lsp_bridge import RequestsLspBridgeClient
from repo_surveyor.call_flow import extract_call_tree, format_call_tree

client = RequestsLspBridgeClient("http://localhost:3000")
client.start_server("java", "file:///path/to/project")
client.open_document(file_uri, "java", source_text)

call_tree = extract_call_tree(client, file_uri, "/path/to/File.java", source_text, "layout", "java")
print(format_call_tree(call_tree))

# call_tree.edges is a dict[str, frozenset[str]] mapping caller -> callees
for caller, callees in sorted(call_tree.edges.items()):
    for callee in sorted(callees):
        print(f"  {caller}() -> {callee}()")
```

The module provides:
- `extract_call_tree()` — traces calls from an entry method, returns a frozen `CallTree`
- `format_call_tree()` — renders a `CallTree` as an indented string
- `CallTree` — frozen dataclass with `entry_method` and `edges`

See `examples/extract_java_flow.py` for a full end-to-end example using JDTLS.

**Limitations**: Currently only supports Java. Only traces calls within a single file (cross-file calls are ignored).

## Persisting to Neo4j

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

## CFG Builder

Construct a control flow graph from source code using tree-sitter:

```python
from tree_sitter_language_pack import get_parser
from repo_surveyor.cfg_constructor.builder import build_cfg
from repo_surveyor.cfg_constructor.cfg_role_registry import get_cfg_spec
from repo_surveyor.integration_patterns.types import Language

spec = get_cfg_spec(Language.JAVA)
parser = get_parser("java")
source = b"""
class A {
    void m(int x) {
        if (x > 0) {
            return x;
        }
        return -x;
    }
}
"""
graph = build_cfg(source, spec, parser)

print(f"Nodes: {len(graph.nodes)}")
print(f"Edges: {len(graph.edges)}")
for edge in graph.edges:
    print(f"  {edge.source} -> {edge.target} ({edge.kind.value})")
```

See [CFG Constructor](cfg-constructor.md) for full documentation of the role schema, graph types, and construction algorithm.
