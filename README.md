<p align="center">
  <img src="docs/images/banner.svg" alt="Codescry" width="900"/>
</p>

<p align="center">
  <a href="https://github.com/avishek-sen-gupta/codescry/actions/workflows/ci.yml"><img src="https://github.com/avishek-sen-gupta/codescry/actions/workflows/ci.yml/badge.svg?branch=main" alt="CI"></a>
</p>

Codescry is a Python library for analysing technology stacks and code structure across a bunch of programming languages.

## What It Does

- **Tech stack detection**: Scans a repository and identifies languages, package managers, frameworks, and infrastructure, associating each with its containing directory (useful for monorepos)
- **Integration point detection**: Finds system integration points (HTTP/REST, SOAP, messaging, databases, gRPC, GraphQL, and 7 more types) using framework-aware pattern matching with tree-sitter syntax zone filtering to skip comments, string literals, import lines, and package/namespace declarations. Every pattern is annotated with a `SignalDirection` (`INWARD` / `OUTWARD` / `AMBIGUOUS`) indicating whether the detected code is exposing an integration point to callers or consuming an external system; this annotation is propagated onto each `IntegrationSignal.direction` field.
- **Signal deduplication**: Pipeline stage that runs immediately after integration detection, merging duplicate FILE_CONTENT signals sharing the same `(file_path, line_number, line_content)` into `CompositeIntegrationSignal` objects via a `SignalLike` protocol. All downstream consumers (symbol resolution, concretisers) receive uniformly deduplicated signals with no redundant processing
- **Symbol resolution**: Resolves each integration signal to its containing code symbol via CTags line ranges, producing per-symbol integration profiles
- **CFG construction**: Builds control flow graphs from tree-sitter parse trees using a language-independent role schema, supporting all 16 languages (except PL/I) with a single algorithm
- **Call-flow extraction**: Traces method call trees within a file using [Mojo-LSP](https://github.com/avishek-sen-gupta/mojo-lsp) go-to-definition
- **Training data generation**: Generates labelled training data for a fine-tuned integration classifier by prompting Claude to produce realistic code snippets per (language, integration_type, label) triple across the three active training labels (`DEFINITE_INWARD`, `DEFINITE_OUTWARD`, `NOT_DEFINITE`); a fourth label `REJECTED` exists in the enum for future use in validation/filtering workflows but is excluded from generation and training. Validated against the existing pattern registry and exported as stratified JSONL splits. Supports a `--batch` mode using the Anthropic Batches API for asynchronous processing at 50% cost savings, with checkpoint/resume support and `--batch-status` to check progress of a running batch
- **Signal classifier training**: Trains a lightweight TF-IDF (word + char n-grams) + Logistic Regression classifier that predicts `DEFINITE_INWARD`, `DEFINITE_OUTWARD`, or `NOT_DEFINITE` from a single `signal_line_content` string with no API dependency at inference time. Serialised with joblib for fast loading. Run with `poetry run python scripts/train_signal_classifier.py --train data/training/train.jsonl --val data/training/val.jsonl --test data/training/test.jsonl --output data/training/signal_classifier.joblib`
- **ML-based signal concretisation**: Groups FILE_CONTENT integration signals by their enclosing AST node (function/method/class body) via tree-sitter walk-up, then classifies each signal line with the trained `SignalClassifier` to produce `ConcretisedSignal` objects labelled `DEFINITE_INWARD`, `DEFINITE_OUTWARD`, or `NOT_DEFINITE`. No LLM or API calls required at inference time. Wired into the main `survey()` pipeline as the fifth stage; pass a loaded `SignalClassifier` via the `classifier` parameter (defaults to `NullSignalClassifier` which labels all signals `NOT_DEFINITE`).
- **GitHub training-data harvester**: `scripts/harvest_github.py` mines real code from GitHub code search using the pattern registry itself as queries. Each HIGH-confidence, non-AMBIGUOUS pattern becomes a search query; the pattern's `SignalDirection` annotation provides the label (INWARD → `DEFINITE_INWARD`, OUTWARD → `DEFINITE_OUTWARD`) with no LLM required. Pass `--not-definite` to additionally harvest `NOT_DEFINITE` examples: non-integration search terms (e.g. `"implements Serializable"`) are used to find files that coincidentally match integration patterns, providing false-positive cases the model must learn to reject. Harvested examples are validated and exported with the same stratified train/val/test split as the synthetic generator. Supports `--resume` (checkpoint-based), `--languages`, `--integration-types`, and `--dry-run`.
- **Ollama-based signal concretisation**: A parallel concretisation pipeline (`scripts/survey_repo_ollama.py`) that passes each unique file with detected signals to a local Ollama model, asking it to classify each signal as `DEFINITE_INWARD`, `DEFINITE_OUTWARD`, or `NOT_DEFINITE` with a confidence score and reason. Runs the same pre-concretisation stages as the ML pipeline; the Ollama step is implemented in `src/repo_surveyor/integration_concretiser/ollama_concretiser.py`. Supports `--model` (default: `qwen2.5-coder:7b-instruct`) and `--ollama-url` (default: `http://localhost:11434`).
- **Gemini Flash signal concretisation**: A cloud-based concretisation pipeline (`scripts/survey_repo_gemini_flash.py`) that mirrors the Ollama pipeline but uses Google's Gemini Flash API for classification, combining LLM reasoning with cloud-speed inference. Uses multi-file batching to group small files into a single API call (up to 80K chars per batch), reducing API round-trips by 5-7x on typical repos. Uses the `google-genai` SDK with structured JSON response mode and exponential backoff on rate limits. Implemented in `src/repo_surveyor/integration_concretiser/gemini_concretiser.py`; shared prompt/parsing logic extracted to `llm_shared.py`. Requires `GEMINI_001_EMBEDDING_API_KEY` env var. Supports `--model` (default: `gemini-2.5-flash`).
- **Embedding-based signal concretisation**: A third concretisation pipeline (`scripts/survey_repo_embedding.py`) that classifies each FILE_CONTENT integration signal by computing cosine similarity between its signal line's code embedding and 26 pre-embedded directional descriptions (13 integration types x 2 directions). Embeds the raw signal line content directly (not AST context) for best accuracy on real-world code including builder-pattern frameworks like Javalin. Uses a 0.56 similarity threshold and argmax over all 26 descriptions to jointly determine integration type and direction; signals below the threshold are labelled `REJECTED` rather than classified. AST walk-up (`extract_invocation_context`) is still used to attach structural context to each `ConcretisedSignal` but does not influence the embedding. Supports two embedding backends via `--backend`: **huggingface** (nomic-embed-code, requires `HUGGING_FACE_URL` and `HUGGING_FACE_API_TOKEN`) and **gemini** (gemini-embedding-001, requires `GEMINI_001_EMBEDDING_API_KEY`). Gemini produces significantly higher-confidence scores (0.52–0.64 vs 0.19–0.30 on smojol-api) with wider signal/noise separation. Implemented in `src/repo_surveyor/integration_concretiser/embedding_concretiser.py` with dependency-injected `EmbeddingClient`/`GeminiEmbeddingClient` for testability.
- **Embedding-based integration classification** (experiment): `scripts/embedding_matrix_test.py` validates that code embeddings from `nomic-embed-code` can classify integration types via cosine similarity against 13 natural-language descriptions. Tests 186 code fragments across all 16 supported languages and 13 integration types, achieving 100% argmax accuracy. A companion script `scripts/embedding_matrix_test_non_io.py` verifies that 96 non-I/O code fragments (sorting, math, string manipulation, validation, data structures, algorithms) score well below the classification threshold (max 0.28 vs 0.45+ for real integrations), confirming clean separation. A third script `scripts/embedding_direction_test.py` tests whether embeddings can classify signal *direction* (INWARD vs OUTWARD) using 97 fragments across 13 integration types with directional description pairs, achieving 97.9% direction-only accuracy (given known type from pattern matching). Requires `HUGGING_FACE_URL` and `HUGGING_FACE_API_TOKEN` environment variables pointing to a deployed nomic-embed-code endpoint
- **Neo4j persistence**: Persists all analysis results to a graph database
- **Datalog emission**: `query/treesitter_to_datalog.py` walks a tree-sitter parse tree and emits a rich 16-relation Datalog ontology covering raw structure (`node`, `token`, `position`, `parent`), semantic structure (`sem_contains`, `field`), identity (`name`, `declared_type`), scoping (`scope`, `scope_parent`, `declares`), use-def (`declaration`, `reference`, `refers_to`), and higher-level facts (`call`, `instantiation`). `query/analysis.dl` provides the companion Soufflé schema plus derived rules — `ancestor` (transitive containment), `call_on` (calls on a named receiver), `scope_chain` (transitive scope enclosure), `visible_from` (declarations visible from a scope), `call_in_scope` (call sites within a lexical scope), `method_decl` (using bridge facts), and `typed_decl`. A **language plugin system** (`query/datalog_plugins.py`) provides per-language configuration — callable node types, call node types, instantiation node types, and call-field extraction strategy — keyed by the `Language` enum. Plugins are injected as three Soufflé bridge-fact files (`callable_node_type.facts`, `call_node_type.facts`, `instantiation_node_type.facts`) so `analysis.dl` never hard-codes language-specific node-type strings. Built-in plugins cover Java, Python, JavaScript, TypeScript, Go, and Rust; the registry is extensible. Run with `souffle -F <facts_dir> -D <output_dir> query/analysis.dl` after exporting facts via `RichDatalogFactSet.to_souffle_facts(output_dir, plugin=plugin)`.
- **Datalog-based integration detection** (PoC): `query/spring_integration.dl` expresses Spring framework patterns (HTTP_REST, MESSAGING, DATABASE) as declarative Soufflé queries over the tree-sitter Datalog ontology. Instead of regex, it uses three structural rules — annotation-based (`@GetMapping` → HTTP_REST/INWARD), type-reference-based (`KafkaTemplate` field → MESSAGING/OUTWARD), and instantiation-based (`new JdbcTemplate()` → DATABASE/OUTWARD) — plus a composite `controller_endpoint` rule that links class-level `@RestController` with method-level HTTP mapping annotations (structural composition impossible with regex). Pattern mappings are data-driven via TSV bridge facts (`annotation_integration.facts`, `type_integration.facts`), so adding new patterns requires no Soufflé edits. The Python runner (`query/datalog_integration_runner.py`) provides `detect_integrations_datalog()` which emits facts, runs Soufflé, and returns `IntegrationSignal` objects with `source="Spring/Datalog"`. Currently a parallel path — does not modify the existing regex detector.

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
| Pascal | `.pas`, `.pp`, `.dpr`, `.lpr`, `.inc` | — | — | — |
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

| Integration Type | Java | Python | TypeScript | JavaScript | Go | Rust | C# | C/C++ | Ruby | Kotlin | Scala | PHP | COBOL | PL/I | Pascal |
|------------------|:----:|:------:|:----------:|:----------:|:--:|:----:|:--:|:-----:|:----:|:------:|:-----:|:---:|:-----:|:----:|:------:|
| `http_rest` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `soap` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `messaging` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `socket` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `database` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `file_io` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `grpc` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — | — |
| `graphql` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | ✓ | ✓ | ✓ | ✓ | — | — | — |
| `email` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — | ✓ |
| `caching` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — | — |
| `sse_streaming` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | ✓ | ✓ | ✓ | ✓ | — | — | — |
| `ftp_sftp` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — | ✓ |
| `scheduling` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | — | — | ✓ |

All 13 integration types have language-specific patterns for Java, Python, TypeScript, JavaScript, Go, Rust, C#, Ruby, Kotlin, Scala, and PHP. C and C++ share a common pattern module covering 11 base types (database, HTTP, messaging, sockets, gRPC, file I/O, FTP/SFTP, caching, SOAP, scheduling, email) with framework-specific patterns for Qt, Boost, POCO, Crow, and Drogon. Pascal covers 9 base types (HTTP/REST, SOAP, messaging, sockets, database, file I/O, email, FTP/SFTP, scheduling) with patterns for Indy, Synapse, FireDAC, ADO, BDE, dbExpress, and other Delphi/Free Pascal component libraries. COBOL and PL/I cover the 6 core types relevant to mainframe systems. Language-agnostic common patterns also apply across all files.

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
- [Soufflé](https://souffle-lang.github.io/) (optional) for Datalog analysis — provides the `souffle` CLI used by `query/analysis.dl` and `query/treesitter_to_datalog.py`

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
| [Presentation](presentation/index.html) | Reveal.js slide deck covering the pipeline, techniques, and architecture |

## License

This project is licensed under the MIT License — see [LICENSE.md](LICENSE.md) for details.
