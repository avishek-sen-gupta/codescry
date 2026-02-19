<p align="center">
  <img src="docs/images/banner.svg" alt="Codescry" width="900"/>
</p>

<p align="center">
  <a href="https://github.com/avishek-sen-gupta/codescry/actions/workflows/ci.yml"><img src="https://github.com/avishek-sen-gupta/codescry/actions/workflows/ci.yml/badge.svg?branch=main" alt="CI"></a>
</p>

Codescry is a Python library for analysing technology stacks and code structure across a bunch of programming languages.

## What It Does

- **Tech stack detection**: Scans a repository and identifies languages, package managers, frameworks, and infrastructure, associating each with its containing directory (useful for monorepos)
- **Integration point detection**: Finds system integration points (HTTP/REST, SOAP, messaging, databases, gRPC, GraphQL, and 7 more types) using framework-aware pattern matching with tree-sitter syntax zone filtering to skip comments, string literals, import lines, and package/namespace declarations
- **Symbol resolution**: Resolves each integration signal to its containing code symbol via CTags line ranges, producing per-symbol integration profiles
- **CFG construction**: Builds control flow graphs from tree-sitter parse trees using a language-independent role schema, supporting all 15 languages (except PL/I) with a single algorithm
- **Call-flow extraction**: Traces method call trees within a file using [Mojo-LSP](https://github.com/avishek-sen-gupta/mojo-lsp) go-to-definition
- **Training data generation**: Generates labelled training data for a fine-tuned integration classifier by prompting Claude to produce realistic code snippets per (language, integration_type, label) triple, validated against the existing pattern registry and exported as stratified JSONL splits. Supports a `--batch` mode using the Anthropic Batches API for asynchronous processing at 50% cost savings, with checkpoint/resume support and `--batch-status` to check progress of a running batch
- **Signal classifier training**: Trains a lightweight TF-IDF (word + char n-grams) + Logistic Regression classifier that predicts `DEFINITE_INWARD`, `DEFINITE_OUTWARD`, or `NOT_DEFINITE` from a single `signal_line_content` string with no API dependency at inference time. Serialised with joblib for fast loading. Run with `poetry run python scripts/train_signal_classifier.py --train data/training/train.jsonl --val data/training/val.jsonl --test data/training/test.jsonl --output data/training/signal_classifier.joblib`
- **ML-based signal concretisation**: Groups FILE_CONTENT integration signals by their enclosing AST node (function/method/class body) via tree-sitter walk-up, then classifies each signal line with the trained `SignalClassifier` to produce `ConcretisedSignal` objects labelled `DEFINITE_INWARD`, `DEFINITE_OUTWARD`, or `NOT_DEFINITE`. No LLM or API calls required at inference time.
- **Neo4j persistence**: Persists all analysis results to a graph database

## Supported Languages

| Language | Extensions | Indicator Files | Package Managers | Detected Frameworks |
|----------|-----------|-----------------|------------------|---------------------|
| Java | `.java` | `pom.xml`, `build.gradle`, `build.gradle.kts` | Maven, Gradle | Spring, JAX-RS, Micronaut, Quarkus, Javalin, Dropwizard, Vert.x, Play, Apache CXF, Apache Axis2, Spring WS, JAX-WS, Helidon, jOOQ |
| Python | `.py` | `pyproject.toml`, `requirements.txt`, `setup.py`, `Pipfile` | Poetry, pip, Pipenv | FastAPI, Django, Flask, Starlette, Tornado, Pyramid, aiohttp, Sanic, Litestar |
| TypeScript | `.ts`, `.tsx` | `tsconfig.json` | _(shared with JS)_ | _(shared with JS)_ |
| JavaScript | `.js`, `.jsx` | `package.json`, `yarn.lock`, `pnpm-lock.yaml` | npm, Yarn, pnpm | React, Vue.js, Angular, Next.js, Nuxt.js, Express, NestJS, Svelte, Gatsby, Fastify, Hono, Koa, Hapi |
| Go | `.go` | `go.mod` | — | Gin, Echo, Fiber, Chi, Gorilla, Connect |
| Rust | `.rs` | `Cargo.toml` | Cargo | Actix, Axum, Rocket, Warp |
| C# | `.cs` | `*.csproj`, `*.sln`, `packages.config` | NuGet | ASP.NET Core, ASP.NET Web API, ServiceStack, Nancy, Carter, WCF, CoreWCF |
| C++ | `.cpp`, `.hpp` | `CMakeLists.txt`, `vcpkg.json`, `conanfile.txt` | CMake, vcpkg, Conan | Qt, Boost, Crow, Drogon, POCO |
| C | `.c`, `.h` | _(shared with C++)_ | _(shared with C++)_ | _(shared with C++)_ |
| Kotlin | `.kt` | _(shared with Java)_ | _(shared with Java)_ | _(shared with Java)_, Ktor, Exposed |
| Scala | `.scala` | `build.sbt` | sbt | Play, Akka HTTP, Akka, Pekko HTTP, Pekko, http4s, Slick, ZIO, Scalatra, Finch, Finatra |
| PHP | `.php` | `composer.json` | Composer | Laravel, Symfony, Slim, CodeIgniter, CakePHP, Yii, Laminas |
| COBOL | `.cbl`, `.cob`, `.cpy` | — | — | — |
| PL/I | `.pli`, `.pl1`, `.plinc` | — | — | — |
| Ruby | `.rb` | `Gemfile` | Bundler | Rails, Sinatra, Hanami |

Infrastructure detection: Docker (`Dockerfile`, `docker-compose.yml`), Terraform (`*.tf`), Kubernetes (`*.yaml` with k8s markers).

Framework detection uses structured parsing of config files rather than naive substring matching — see [Package Parser Subsystem](docs/architecture.md#package-parser-subsystem) for parser details. This prevents false positives like `"reactive-streams"` matching `"react"` or `"expression"` matching `"express"`.

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

| Integration Type | Java | Python | TypeScript | JavaScript | Go | Rust | C# | C/C++ | Ruby | Kotlin | Scala | PHP | COBOL | PL/I |
|------------------|:----:|:------:|:----------:|:----------:|:--:|:----:|:--:|:-----:|:----:|:------:|:-----:|:---:|:-----:|:----:|
| `http_rest` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `soap` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `messaging` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `socket` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `database` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `file_io` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `grpc` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — |
| `graphql` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | ✓ | ✓ | ✓ | ✓ | — | — |
| `email` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — |
| `caching` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — |
| `sse_streaming` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | ✓ | ✓ | ✓ | ✓ | — | — |
| `ftp_sftp` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — |
| `scheduling` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — |

All 13 integration types have language-specific patterns for Java, Python, TypeScript, JavaScript, Go, Rust, C#, Ruby, Kotlin, Scala, and PHP. C and C++ share a common pattern module covering 11 base types (database, HTTP, messaging, sockets, gRPC, file I/O, FTP/SFTP, caching, SOAP, scheduling, email) with framework-specific patterns for Qt, Boost, POCO, Crow, and Drogon. COBOL and PL/I cover the 6 core types relevant to mainframe systems. Language-agnostic common patterns also apply across all files.

## Installation

```bash
poetry install
```

## Quick Start

```python
from repo_surveyor import survey

tech_report, structure_result, integration_result, resolution = survey("/path/to/repo", languages=["Java"])

print(tech_report.to_text())
print(f"Symbols: {len(structure_result.entries)}")
print(f"Integration points: {len(integration_result.integration_points)}")
print(f"Resolved to symbols: {len(resolution.resolved)}")
```

See the [API Reference](docs/api-reference.md) for detailed usage of each subsystem.

## Dependencies

- Python 3.13+
- [Poetry](https://python-poetry.org/) for dependency management
- [Universal CTags](https://github.com/universal-ctags/ctags) for code symbol extraction
- [Neo4j](https://neo4j.com/) (optional) for graph persistence
- [Graphviz](https://graphviz.org/) (optional) for DOT diagram export — provides the `dot` CLI used by `scripts/generate_diagrams.py`

## Visual Examples

These diagrams are generated from Codescry's own codebase by `scripts/generate_diagrams.py`.

### Control Flow Graph

CFG for `_dep_matches_pattern` (package parser pattern matching with early returns):

![CFG Example](docs/images/cfg_example.svg)

### Integration Signal Diagram

Integration signals detected in this repository, resolved to their containing symbols:

![Integration Signals](docs/images/integration_signals.svg)

To regenerate:

```bash
poetry run python scripts/generate_diagrams.py
```

## Documentation

| Document | Description |
|----------|-------------|
| [API Reference](docs/api-reference.md) | Usage examples for all subsystems — tech stacks, integration detection, symbol resolution, LSP bridge, call flow, Neo4j, and CFG builder |
| [Architecture](docs/architecture.md) | Internal code flows, plugin system, subsystem internals, and design patterns |
| [CFG Constructor](docs/cfg-constructor.md) | Control flow graph construction — role schema, graph types, builder algorithm, and per-language config |
| [Testing & CI](docs/testing.md) | Running tests, coverage, local-repo tests, and CI pipeline |

## License

This project is licensed under the MIT License — see [LICENSE.md](LICENSE.md) for details.
