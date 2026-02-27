![Codescry](docs/images/banner.svg)

# CodeScry

[![CI](https://github.com/avishek-sen-gupta/codescry/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/avishek-sen-gupta/codescry/actions/workflows/ci.yml)
[![Presentation](https://img.shields.io/badge/Presentation-Reveal.js-blue)](presentation/index.html)
[![License: MIT](https://img.shields.io/badge/License-MIT-green)](LICENSE.md)

Codescry is a Python library for analysing technology stacks and code structure across a bunch of programming languages.

## What It Does

- **Tech stack detection**: Scans a repository and identifies languages, package managers, frameworks, and infrastructure, associating each with its containing directory (useful for monorepos)
- **Integration point detection**: Finds system integration points (HTTP/REST, SOAP, messaging, databases, gRPC, GraphQL, and 7 more types) using framework-aware pattern matching with tree-sitter syntax zone filtering to skip comments, string literals, import lines, and package/namespace declarations. Every pattern is annotated with a `SignalDirection` (`INWARD` / `OUTWARD` / `AMBIGUOUS`) indicating whether the detected code is exposing an integration point to callers or consuming an external system; this annotation is propagated onto each `IntegrationSignal.direction` field. Each pattern also carries two human-readable description strings capturing framework-specific semantics using a subject-first passive-voice template optimised for code embedding similarity (e.g., `"GET route is registered for HTTP endpoint"`, `"REST endpoint is exposed for inbound requests"`); these descriptions are used by the pattern-embedding concretiser as nearest-neighbor classification targets. See [EXPERIMENTS.md](EXPERIMENTS.md) for the description writing rules and CodeT5 embedding experiments that motivated this style.
- **Signal deduplication**: Pipeline stage that runs immediately after integration detection, merging duplicate FILE_CONTENT signals sharing the same `(file_path, line_number, line_content)` into `CompositeIntegrationSignal` objects via a `SignalLike` protocol. All downstream consumers (symbol resolution, concretisers) receive uniformly deduplicated signals with no redundant processing
- **Symbol resolution**: Resolves each integration signal to its containing code symbol via CTags line ranges, producing per-symbol integration profiles
- **CFG construction**: Builds control flow graphs from tree-sitter parse trees using a language-independent role schema, supporting all 16 languages (except PL/I) with a single algorithm
- **Call-flow extraction**: Traces method call trees within a file using [Mojo-LSP](https://github.com/avishek-sen-gupta/mojo-lsp) go-to-definition
- **Training data generation**: Generates labelled training data for a fine-tuned integration classifier by prompting Claude to produce realistic code snippets per (language, integration_type, label) triple across the three active training labels (`DEFINITE_INWARD`, `DEFINITE_OUTWARD`, `NOT_DEFINITE`); a fourth label `REJECTED` exists in the enum for future use in validation/filtering workflows but is excluded from generation and training. Validated against the existing pattern registry and exported as stratified JSONL splits. Supports a `--batch` mode using the Anthropic Batches API for asynchronous processing at 50% cost savings, with checkpoint/resume support and `--batch-status` to check progress of a running batch
- **Signal classifier training**: Trains a lightweight TF-IDF (word + char n-grams) + Logistic Regression classifier that predicts `DEFINITE_INWARD`, `DEFINITE_OUTWARD`, or `NOT_DEFINITE` from a single `signal_line_content` string with no API dependency at inference time. Serialised with joblib for fast loading. Run with `poetry run python scripts/train_signal_classifier.py --train data/training/train.jsonl --val data/training/val.jsonl --test data/training/test.jsonl --output data/training/signal_classifier.joblib`
- **ML-based signal concretisation**: Groups FILE_CONTENT integration signals by their enclosing AST node (function/method/class body) via tree-sitter walk-up, then classifies each signal line with the trained `SignalClassifier` to produce `ConcretisedSignal` objects with two-stage classification: `validity` (`SIGNAL` or `NOISE`) gates whether the signal is a real integration, and `direction` (`INWARD`, `OUTWARD`, or `AMBIGUOUS`) indicates the integration direction. The AST grouper uses **batched AST extraction** (`batch_extract_ast_contexts`) to parse each file once and walk the AST once for all signals in that file, eliminating O(N) redundant parses and traversals per file. No LLM or API calls required at inference time. Wired into the main `survey()` pipeline as the fifth stage; pass a loaded `SignalClassifier` via the `classifier` parameter (defaults to `NullSignalClassifier` which classifies all signals as `NOISE`).
- **GitHub training-data harvester**: `scripts/harvest_github.py` mines real code from GitHub code search using the pattern registry itself as queries. Each HIGH-confidence, non-AMBIGUOUS pattern becomes a search query; the pattern's `SignalDirection` annotation provides the label (INWARD → `DEFINITE_INWARD`, OUTWARD → `DEFINITE_OUTWARD`) with no LLM required. Pass `--not-definite` to additionally harvest `NOT_DEFINITE` examples: non-integration search terms (e.g. `"implements Serializable"`) are used to find files that coincidentally match integration patterns, providing false-positive cases the model must learn to reject. Harvested examples are validated and exported with the same stratified train/val/test split as the synthetic generator. Supports `--resume` (checkpoint-based), `--languages`, `--integration-types`, and `--dry-run`.
### Classification Pipelines

All pipelines share the same Phase 1 (tech stack detection, integration signal discovery, symbol resolution) and Phase 3 (JSONL + timings output). They differ in Phase 2: how detected signals are classified into `SIGNAL`/`NOISE` and `INWARD`/`OUTWARD`/`AMBIGUOUS`.

| Pipeline | Technique | Validity Gate | Direction | API Required | Key Parameters | Script |
|----------|-----------|--------------|-----------|-------------|----------------|--------|
| TF-IDF + LogReg | ML classifier on signal line text | TF-IDF confidence | TF-IDF confidence | None | `--model` (joblib path) | `survey_classify_by_tfidf_logistic_regression.py` |
| Generic Embedding | Cosine similarity vs 26 directional descriptions | Score threshold (0.56) | Score ratio (1.2x) | Depends on backend | `--backend`, `--threshold` | `survey_classify_by_embedding_cosine_similarity.py` |
| Pattern Embedding KNN | Cosine similarity vs ~3,430 per-pattern descriptions, distance-weighted KNN | Average K-neighbour score + evidence checks | Weighted vote across K neighbours | Depends on backend | `--backend`, `--threshold`, `--k` | `survey_classify_by_embedding_similarity_weighted_knn.py` |
| Ollama LLM | Local LLM classifies each file's signals | LLM judgement | LLM judgement | None (local) | `--model`, `--ollama-url` | `survey_classify_by_ollama_llm.py` |
| Gemini Flash LLM | Cloud LLM classifies each file's signals | LLM judgement | LLM judgement | `GEMINI_001_EMBEDDING_API_KEY` | `--model` | `survey_classify_by_gemini_flash_llm.py` |
| Hybrid: Generic Gate + Gemini | Embedding gate for SIGNAL/NOISE, Gemini for direction | Embedding threshold | Gemini LLM | `GEMINI_001_EMBEDDING_API_KEY` | `--backend`, `--threshold` | `survey_classify_by_embedding_gate_nearest_neighbour_gemini_flash.py` |
| Hybrid: Pattern Gate + Gemini | Pattern-embedding KNN gate for SIGNAL/NOISE, Gemini for direction | KNN avg score + evidence | Gemini LLM | `GEMINI_001_EMBEDDING_API_KEY` | `--backend`, `--threshold`, `--k` | `survey_classify_by_framework_specific_pattern_embedding_gate_gemini_flash.py` |

**Embedding backends** (for all embedding-based pipelines): `huggingface` (nomic-embed-code), `gemini` (gemini-embedding-001), `ollama` (local, jina-embeddings-v2-base-code), `hf-local` (local, Salesforce/codet5p-110m-embedding), `coderank` (local, nomic-ai/CodeRankEmbed), `bge` (local, BAAI/bge-base-en-v1.5 — **recommended**, see [EXPERIMENTS.md](EXPERIMENTS.md)). The `ollama`, `hf-local`, `coderank`, and `bge` backends run entirely locally with no API keys.

**Shared pipeline infrastructure**:
- All pipelines produce two-stage classification: `validity` (`SIGNAL` or `NOISE`) gates whether the signal is a real integration, and `direction` (`INWARD`, `OUTWARD`, `AMBIGUOUS`, or `NOT_INTEGRATION`) indicates the integration direction
- AST context is extracted via tree-sitter walk-up (batched per file) and attached to every `ConcretisedSignal`, regardless of pipeline
- LLM pipelines (Ollama, Gemini) include AST statement context in prompts and use multi-file batching to reduce API round-trips
- Embedding pipelines use vectorized cosine similarity via numpy for fast KNN classification (~0.3s for 758 signals vs 3,478 descriptions)
- Pattern description embeddings are cached to `data/embeddings/` with SHA-256 content-hash auto-invalidation; pre-build with `poetry run python pipeline/survey_build_pattern_embedding_cache.py --backend <name>`
- The Pattern Embedding KNN pipeline applies seven universal evidence suppression checks (test files, vendor dirs, generated code, config dirs, string literals, log statements, constant declarations) to adjust cosine scores before thresholding
- Hybrid pipelines run embedding for SIGNAL/NOISE gating, then send only SIGNALs to Gemini for direction; embedding validity is authoritative, Gemini direction is authoritative
- All pipelines write `timings.json` to the output directory with per-stage durations (including Phase 2 substages: AST extraction, signal embedding, similarity calculation)
- **Claude rule-based ground truth**: `data/build_claude_ground_truth.py` produces an independent signal classification of all 758 smojol signals using a 12-rule priority classifier derived from manual code review. Rules cover generated parser code (ANTLR), COBOL string literals in Java, logging/assertion references, DI/config bindings, file metadata operations, and actual I/O operations (file, HTTP, database, socket, cache). Falls through to Gemini's classification as a tiebreaker for unclear signals. Output: `data/survey_output_claude_gt/*.jsonl`.
- **Three-way classification comparison**: `data/three_way_comparison.py` loads Gemini LLM, BGE embedding, and Claude rule-based classifications for the same 758 signals and produces pairwise confusion matrices, three-way agreement analysis (Gemini-BGE-Claude pattern counts), per-integration-type breakdown, and a full TSV (`data/three_way_comparison.tsv`) for manual inspection. Output: `data/three_way_comparison_report.txt`.
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
| [ADRs](docs/architectural-design-decisions.md) | Architectural Decision Records — rationale behind key design choices |
| [CFG Constructor](docs/cfg-constructor.md) | Control flow graph construction — role schema, graph types, builder algorithm, and per-language config |
| [Testing & CI](docs/testing.md) | Running tests, coverage, local-repo tests, and CI pipeline |
| [Presentation](presentation/index.html) | Reveal.js slide deck covering the pipeline, techniques, and architecture |

## See Also

- **[Red Dragon](https://github.com/avishek-sen-gupta/red-dragon)** — Multi-language symbolic code analysis toolkit (IR lowering, CFG generation, dataflow analysis, symbolic execution)
- **[Rev-Eng TUI](https://github.com/avishek-sen-gupta/reddragon-codescry-tui)** — Terminal UI that integrates Codescry and Red Dragon for interactive top-down reverse engineering of codebases

## License

This project is licensed under the MIT License — see [LICENSE.md](LICENSE.md) for details.
