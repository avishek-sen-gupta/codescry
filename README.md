# Cartographer

A Python library for analyzing repository technology stacks and code structure.

**Note:** Most of this codebase was vibe-coded.

## Features

- Detects programming languages, package managers, frameworks, and infrastructure
- Scans indicator files (package.json, pyproject.toml, Dockerfile, etc.)
- Associates technologies with their containing directories (useful for monorepos)
- Extracts code symbols using Universal CTags
- Detects system integration points (HTTP/REST, SOAP, messaging, sockets, databases)
- Extracts method call trees via [mojo-lsp](https://github.com/avishek-sen-gupta/mojo-lsp) LSP bridge and tree-sitter
- Persists analysis results to Neo4j graph database
- Generates plain text reports

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

#### Supported Integration Types

| Type | Description | Example Patterns |
|------|-------------|------------------|
| `http_rest` | HTTP/REST endpoints and clients | `@RestController`, `@GetMapping`, `app.get(`, `use actix_web::` |
| `soap` | SOAP/XML web services | `@WebService`, `SOAPMessage`, `XML PARSE` |
| `messaging` | Message queues and event buses | `@KafkaListener`, `@RabbitListener`, `MQPUT`, `MQGET` |
| `socket` | Raw sockets and WebSockets | `@ServerEndpoint`, `WebSocket`, `TcpListener` |
| `database` | Database connections and ORMs | `@Repository`, `@Entity`, `EXEC SQL`, `JdbcTemplate` |

#### Supported Languages

| Language | File Extensions | Notable Patterns |
|----------|-----------------|------------------|
| Java | `.java` | Spring annotations, JPA, JDBC, JMS |
| Python | `.py` | Flask, FastAPI, Django, SQLAlchemy, Celery |
| TypeScript | `.ts`, `.tsx` | NestJS, TypeORM, Prisma |
| JavaScript | `.js`, `.jsx` | Express, Mongoose, Sequelize |
| Rust | `.rs` | Actix, Axum, Diesel, SQLx |
| Go | `.go` | Gin, Echo, GORM |
| C# | `.cs` | ASP.NET, Entity Framework, SignalR |
| COBOL | `.cbl`, `.cob`, `.cpy` | CICS, DB2, IMS DB, IDMS, IBM MQ |
| PL/I | `.pli`, `.pl1`, `.plinc` | CICS, DB2, IMS DB, IDMS, IBM MQ |

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

## Detected Technologies

| Category | Indicator Files | Technologies |
|----------|-----------------|--------------|
| Python | `pyproject.toml`, `requirements.txt`, `setup.py`, `Pipfile` | Python, Poetry, pip, Pipenv |
| JavaScript/Node | `package.json`, `yarn.lock`, `pnpm-lock.yaml` | Node.js, npm, Yarn, pnpm |
| TypeScript | `tsconfig.json` | TypeScript |
| Java | `pom.xml`, `build.gradle`, `build.gradle.kts` | Java, Maven, Gradle |
| Go | `go.mod` | Go |
| Rust | `Cargo.toml` | Rust, Cargo |
| Ruby | `Gemfile` | Ruby, Bundler |
| .NET | `*.csproj`, `*.sln` | C#, .NET |
| Docker | `Dockerfile`, `docker-compose.yml` | Docker, Docker Compose |
| Terraform | `*.tf` | Terraform |
| Kubernetes | `*.yaml` with k8s markers | Kubernetes |

## Running Tests

```bash
poetry run pytest
```
