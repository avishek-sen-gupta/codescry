# Cartographer

A Python library for experiments in analysing repository technology stacks and code structure.

**Note:** Most of this codebase was vibe-coded.

## Features

- Detects programming languages, package managers, frameworks, and infrastructure
- Structured parsing of config files (package.json, pyproject.toml, pom.xml, .csproj, etc.) for accurate framework detection — no false positives from substring matching
- Associates technologies with their containing directories (useful for monorepos)
- Extracts code symbols using Universal CTags
- Detects system integration points (HTTP/REST, SOAP, messaging, sockets, databases) with framework-aware pattern matching
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

#### Supported Integration Types

| Type | Description | Example Patterns |
|------|-------------|------------------|
| `http_rest` | HTTP/REST endpoints and clients | `@RestController`, `@GetMapping`, `app.get(`, `use actix_web::` |
| `soap` | SOAP/XML web services | `@WebService`, `SOAPMessage`, `XML PARSE` |
| `messaging` | Message queues and event buses | `@KafkaListener`, `@RabbitListener`, `MQPUT`, `MQGET` |
| `socket` | Raw sockets and WebSockets | `@ServerEndpoint`, `WebSocket`, `TcpListener` |
| `database` | Database connections and ORMs | `@Repository`, `@Entity`, `EXEC SQL`, `JdbcTemplate` |

#### Supported Languages and Frameworks

| Language | File Extensions | Base Patterns | Framework-Specific Patterns |
|----------|-----------------|---------------|----------------------------|
| Java | `.java` | JPA, JDBC, JMS, servlet, SOAP | Spring, JAX-RS, Micronaut, Quarkus, Javalin, Dropwizard, Vert.x, Play, Apache CXF, Apache Axis2, Spring WS, JAX-WS |
| Python | `.py` | requests, SQLAlchemy, Celery, websockets | Flask, FastAPI, Django, Starlette, aiohttp, Tornado, Pyramid |
| TypeScript | `.ts`, `.tsx` | axios, TypeORM, Prisma, kafkajs | NestJS, Express, Angular, Next.js |
| JavaScript | `.js`, `.jsx` | axios, Mongoose, Sequelize, kafkajs | Express, Next.js |
| Rust | `.rs` | Diesel, SQLx, rdkafka, tungstenite | Actix, Axum, Rocket, Warp |
| Go | `.go` | net/http, GORM, sarama, gorilla/websocket | Gin, Echo, Fiber, Chi, Gorilla |
| C# | `.cs` | ASP.NET, Entity Framework, SignalR | ASP.NET Core, ASP.NET Web API, WCF, CoreWCF, ServiceStack, Nancy, Carter |
| COBOL | `.cbl`, `.cob`, `.cpy` | CICS, DB2, IMS DB, IDMS, IBM MQ | _(no framework detection yet)_ |
| PL/I | `.pli`, `.pl1`, `.plinc` | CICS, DB2, IMS DB, IDMS, IBM MQ | _(no framework detection yet)_ |

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
| COBOL | `.cbl`, `.cob`, `.cpy` | COBOL |
| .NET | `*.csproj`, `*.sln`, `packages.config` | C#, .NET, NuGet |
| Docker | `Dockerfile`, `docker-compose.yml` | Docker, Docker Compose |
| Terraform | `*.tf` | Terraform |
| Kubernetes | `*.yaml` with k8s markers | Kubernetes |

## Framework Detection

Framework detection uses structured parsing of config files rather than naive substring matching. Each file format has a dedicated parser that extracts actual dependency names:

| File | Parser | Technique |
|------|--------|-----------|
| `pyproject.toml` | tomllib | PEP 621 `[project.dependencies]`, Poetry `[tool.poetry.dependencies]` |
| `requirements.txt` | line parsing | PEP 508 name extraction |
| `Pipfile` | tomllib | `[packages]`, `[dev-packages]` |
| `setup.py` | regex | `install_requires=[...]` |
| `package.json` | json | `dependencies`, `devDependencies`, `peerDependencies` |
| `pom.xml` | xml.etree | `<artifactId>` from `<dependency>` elements |
| `build.gradle` / `.kts` | regex | `implementation`, `api`, `compile` declarations |
| `go.mod` | line parsing | `require` block module paths |
| `Cargo.toml` | tomllib | `[dependencies]`, `[dev-dependencies]`, `[build-dependencies]` |
| `*.csproj` | xml.etree | `<PackageReference Include="...">` |
| `packages.config` | xml.etree | `<package id="...">` |

Parsed dependency names are matched against framework patterns using smart matching rules:
- **Exact match**: `"fastapi"` == `"fastapi"`
- **Prefix-separator**: `"spring-boot-starter-web"` matches `"spring-boot"` (hyphen), `"microsoft.aspnetcore.mvc"` matches `"microsoft.aspnetcore"` (dot)
- **Path subsequence**: `"github.com/gin-gonic/gin"` matches `"gin-gonic/gin"`
- **npm scoped**: `"@nestjs/core"` matches `"nestjs"`

This prevents false positives like `"reactive-streams"` matching `"react"` or `"expression"` matching `"express"`.

### Detected Frameworks

| Ecosystem | Frameworks |
|-----------|-----------|
| Python | FastAPI, Django, Flask, Starlette, Tornado, Pyramid, aiohttp |
| JavaScript/Node | React, Vue.js, Angular, Next.js, Nuxt.js, Express, NestJS, Svelte, Gatsby, Fastify |
| Java | Spring, JAX-RS, Micronaut, Quarkus, Javalin, Dropwizard, Vert.x, Play, Apache CXF, Apache Axis2, Spring WS, JAX-WS |
| Go | Gin, Echo, Fiber, Chi, Gorilla |
| Rust | Actix, Axum, Rocket, Warp |
| .NET | ASP.NET Core, ASP.NET Web API, ServiceStack, Nancy, Carter, WCF, CoreWCF |

## Running Tests

```bash
poetry run pytest
```
