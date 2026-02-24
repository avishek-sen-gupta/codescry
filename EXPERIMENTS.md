# Experiments

## HuggingFace Local Embedding Client: CodeT5p-110m-embedding

### Background

We added a fourth embedding backend (`--backend hf-local`) using `Salesforce/codet5p-110m-embedding`, a 110M-parameter code embedding model that produces 256-dimensional L2-normalised vectors. Unlike the other backends (HuggingFace Inference Endpoint, Gemini, Ollama), this runs locally via the `transformers` library with no API server.

### Compatibility

The model's custom code requires `transformers<5.0.0`. Version 5.x introduces a breaking change in the T5 config (`is_decoder` attribute missing from `CodeT5pEmbeddingConfig`). Pin to `transformers>=4.30.0,<5.0.0`.

### Token Length Limit

The model has a 512-token maximum sequence length. On the full smojol repo (758 Java signals), some AST statement contexts exceeded this limit, causing quadratic memory growth and OOM kills. Fix: pass `truncation=True, max_length=512` to the tokenizer. This truncates long inputs but may degrade embedding quality for large code blocks.

### Results: smojol-api (22 signals, Java)

Threshold: 0.58

| Backend | Classified | Inward | Outward | Ambiguous | Noise |
|---------|-----------|--------|---------|-----------|-------|
| **hf-local (CodeT5)** | 11 / 22 (50%) | 10 | 1 | 0 | 11 |
| **Gemini** (0.62 threshold) | 20 / 22 (91%) | 17 | 3 | 0 | 2 |

The 10 inward signals are the Javalin `.get(...)` route registrations (score 0.594 each). The single outward signal is `DriverManager.getConnection` (score 0.656). The `ctx.json()` callback lines (0.486-0.490) and database result-fetching lines (0.419-0.436) fall below threshold.

### Results: full smojol repo (758 signals, Java only)

Threshold: 0.58

| Label | Count |
|-------|-------|
| Inward | 10 |
| Outward | 10 |
| Ambiguous | 34 |
| Noise | 704 |
| **Total** | **758** |

Classification rate: 54 / 758 (7.1%). The model gates 92.9% of signals as noise at this threshold.

### Score Distribution Comparison

| Metric | hf-local (CodeT5) | Gemini |
|--------|-------------------|--------|
| Score range | 0.31 - 0.66 | 0.59 - 0.74 |
| Embedding dim | 256 | 768 |
| Embedding time (3,430 descs) | ~100s (CPU) | ~5s (API) |
| Signal embedding (22 texts) | ~2.8s | ~1.2s |

CodeT5 produces compressed similarity scores with a narrower signal/noise gap, making thresholding fragile.

### Verdict

CodeT5p-110m-embedding is **not competitive with Gemini at equivalent thresholds**. The model does demonstrate semantic understanding of code (Javalin routes match `http_rest/inward`, database connections match `database/outward`), but the compressed score range and 512-token limit make it unsuitable as a drop-in replacement. It could serve as a fast local pre-filter or be used with a substantially lower threshold (0.45-0.50) at the cost of more false positives.

---

## Writing Effective Pattern Descriptions for CodeT5 Embeddings

### Experiment Design

Given a code fragment, we iteratively invented natural-language descriptions and measured cosine similarity in the CodeT5 embedding space, seeking the description closest to the code without explicitly mirroring the API names or code structure.

### Case Study: Iterative Convergence

Code: `String text = new String(Files.readAllBytes(src.toPath()));`

| Round | Score | Description | Insight |
|-------|-------|-------------|---------|
| 1 | 0.448 | "Ingests a file from disk, producing a text representation of its contents" | Verbose, generic |
| 2 | 0.541 | "Reads a file from a given source and converts its full content into text" | "source" maps to `src` |
| 4 | 0.626 | "Reads from the given source, converting file content into text" | Shorter is better |
| 7 | 0.651 | "Reads file content from source, creating text" | "creating" maps to `new String()` |
| 8 | 0.673 | "Reads source file, creating text" | Compound noun "source file" |
| 10 | 0.684 | "Reads all source file, creating text" | "all" maps to `readAllBytes` |
| 12 | 0.707 | "Text created from reading all source file" | Noun-first structure |
| 13 | 0.730 | "Text created by reading all source file" | "by" beats "from" |
| 18 | 0.757 | "Text is created by reading all file content from source" | Full passive with "is" |
| **20** | **0.773** | **"Text is created by reading all source file"** | Brevity + passive |

For comparison, the literal transliteration "String text equals new String of Files readAllBytes of source toPath" scored 0.688 — the semantic description at 0.773 **surpassed** the literal one.

### Noise Signal Recovery

We selected 7 genuine I/O operations from the 704 noise signals (full smojol repo run) and wrote optimised descriptions:

| Case | Score | Best Description |
|------|-------|------------------|
| BufferedReader from stream | 0.695 | "Buffered reader is created over input stream with charset for text" |
| CacheBuilder | 0.684 | "Cache is created by building with expiry size and loader" |
| writeToGraphML | 0.668 | "Graph export is written to file at output path" |
| File.createTempFile | 0.660 | "Temp file is created for source input configuration" |
| getCanonicalFile + analysis | 0.649 | "Analysis is run on canonical file with language context" |
| JDBC connection | 0.621 | "Database is accessed by opening connection with user and password" |
| ServerSocket | 0.584 | "Server is started by creating socket on port" |

All 7 descriptions achieved scores above the 0.58 threshold, meaning these signals would be correctly classified with the right description in the pattern registry.

### Rules for Writing CodeT5-Effective Descriptions

**1. Use subject-first passive voice: "X is Y-ed by Z"**

The subject noun should be the *result* or *primary object* of the code operation. This mirrors assignment semantics (`result = operation(...)`) which is how most code is structured.

- Good: "Database is accessed by opening connection with credentials"
- Bad: "Opens a connection to access the database with credentials"

**2. Name the primary domain concept, not the API class**

Use the concept-level noun that the code is *about*, not the specific class name. The model understands that `DriverManager.getConnection` is about "database", `CacheBuilder.newBuilder()` is about "cache", and `new BufferedReader(new InputStreamReader(...))` is about "buffered reader".

- Good: "Cache is built with expiry and size"
- Bad: "CacheBuilder.newBuilder is called with expireAfterWrite and maximumSize"

**3. Include key operational parameters as bare nouns**

Echo the *semantic role* of the code's parameters without naming the variables. These parameter echoes are the strongest score boosters after the subject noun.

- Good: "...with user and password" (from `url, user, password`)
- Good: "...with charset for text" (from `charset` parameter)
- Good: "...with expiry size and loader" (from `.expireAfterWrite`, `.maximumSize`, `CacheLoader`)

**4. Brevity is critical**

Every word must earn its place. Articles ("a", "the"), qualifiers ("specific", "designated"), and elaboration ("in order to", "which allows") consistently reduce scores.

- Good: "Temp file is created for source input" (0.660)
- Bad: "A temporary file is allocated on the local disk for intermediate storage" (0.411)

**5. Match the code's action verb semantics**

Use the verb that maps to the code's actual operation, not a synonym:

| Code pattern | Best verb |
|-------------|-----------|
| `new X()` | "is created" |
| `.getConnection()` | "is accessed" / "is opened" |
| `write*()` | "is written" |
| Builder pattern | "is built" |
| `run*()` | "is run" |

Generic verbs like "performed", "executed", "utilised" score poorly.

**6. The model struggles with multi-step builder chains**

Long chained method calls (`.endpoint().credential().buildClient()`) are hard to capture in a single description. The model works best with clear single-action-with-parameters structures. Builder patterns may need to be described by their *outcome* rather than their *steps*.

**7. Description template**

> `[Primary result noun] is [action verb past participle] [preposition] [key parameters]`

Examples following the template:
- "Connection is opened to database with credentials"
- "Cache is built with expiry duration and size"
- "File is written with exported data at output path"
- "Text is created by reading all source file"
- "Server is started by creating socket on port"

---

## Bulk Description Rewrite: Passive-Voice Template

### Motivation

The experiments above showed that subject-first passive-voice descriptions ("HTTP route is registered with path handler") produce significantly higher cosine similarity scores than verbose active-voice descriptions ("Flask @app.route decorator defining an inbound HTTP REST endpoint") in the CodeT5 embedding space. The pattern registry contained 3,430 descriptions across 78 files, all written in the old verbose style. Rewriting them universally should improve classification quality across all embedding backends.

### Method

We built a one-off LLM rewrite script (`scripts/rewrite_pattern_descriptions.py`) that:

1. Extracted all 3,017 unique description strings from 77 pattern files via regex
2. Sent them to Claude (Sonnet) in batches of 60 with the 7 rewriting rules as system prompt
3. Performed exact string replacement in each source file
4. Verified the description count remained at 3,430 and all 1,351 tests passed

The LLM system prompt encoded the template `[Result noun] is [verb past participle] [preposition] [key parameters]` with before/after examples drawn from the experiments above.

### Example Rewrites

| Before (verbose active voice) | After (passive-voice template) |
|-------------------------------|-------------------------------|
| Flask @app.route decorator defining an inbound HTTP REST endpoint | HTTP route is registered with path handler |
| This code uses Javalin to handle incoming HTTP GET requests | HTTP GET requests are handled for inbound requests |
| JDBC DriverManager.getConnection for connecting to a relational database | Database connection is opened with credentials |
| Spring @RestController annotation defining a REST API controller | REST controller is annotated for inbound API |
| Generic HTTP keyword indicating web communication | HTTP communication is indicated |
| PL/I CICS WRITEQ TD command for transient data queue writing | Transient data queue is written by WRITEQ TD |
| Go grpc package import providing core gRPC client and server primitives | gRPC primitives are imported |

### Results: full smojol repo (758 signals, Java only, CodeT5 hf-local)

Threshold: 0.58

| Metric | Before rewrite | After rewrite | Change |
|--------|---------------|---------------|--------|
| Classified | 54 (7.1%) | 33 (4.4%) | -21 |
| Inward | 10 | 14 | +4 |
| Outward | 10 | 9 | -1 |
| Ambiguous | 34 | 10 | **-24** |
| Noise | 704 | 725 | +21 |

### Analysis

The rewrite produced a qualitative shift in classification behaviour:

1. **Ambiguous signals dropped 71%** (34 → 10). The clearer, more specific descriptions produce more decisive nearest-neighbor matches — signals now land firmly on either a directional description or fall below threshold entirely, rather than matching multiple descriptions at similar scores.

2. **Inward signals increased 40%** (10 → 14). The passive-voice descriptions for route handlers and endpoint registrations ("GET route is registered for HTTP requests") are closer in the embedding space to actual route registration code than the old verbose descriptions were.

3. **Outward signals held steady** (10 → 9). Database connections, HTTP clients, and other outbound patterns remain well-matched.

4. **Total classified decreased** (54 → 33). This is expected and desirable: the 24 formerly-ambiguous signals were borderline matches that produced low-confidence, uninformative classifications. They now correctly fall into noise rather than polluting the ambiguous bucket. The remaining 33 classified signals are higher-quality, more directionally specific matches.

5. **Classification rate vs quality trade-off**: The raw classification rate dropped from 7.1% to 4.4%, but the *useful* classification rate (signals with a definite direction) improved from 20/758 (2.6%) to 23/758 (3.0%). The rewrite traded away noisy ambiguous matches for sharper directional ones.

### Verdict

The bulk description rewrite validates the findings from the hand-crafted experiments: passive-voice, domain-concept descriptions consistently produce better embedding matches than verbose, API-name-heavy descriptions. The improvement is most visible in the dramatic reduction of ambiguous classifications, confirming that the template forces descriptions into a semantic space that aligns better with code semantics. The CodeT5 backend remains significantly less capable than Gemini (4.4% vs ~91% classification rate), but the rewrite narrows the quality gap for signals that *are* classified.

---

## Warp Pattern Expansion & Rust AST Walker Fix

### Background

Running the pattern-embedding pipeline on `restful-rust` (a Warp-based REST API) initially detected only 2 signals, with 1 classified. Investigation revealed two compounding issues:

1. **Sparse Warp patterns**: The Warp framework module had only 1 pattern (`warp::path!`), missing all HTTP method filters, filter composition, JSON body handling, SSE, and WebSocket patterns.
2. **Broken Rust AST walk-up**: Rust's tree-sitter grammar uses `*_item` suffix for top-level definitions (`function_item`, `struct_item`, `impl_item`, etc.) instead of the `*_definition`/`*_declaration` convention used by most other languages. The AST walker's suffix heuristics missed these entirely, so signals inside Warp method chains (`.and(warp::get())`) resolved to the raw `arguments` node with text `()` — a meaningless context that produced near-random embedding matches.

### Fix 1: Warp Pattern Expansion

Expanded from 1 to 25 patterns (50 descriptions) across three integration types:

| Integration Type | Patterns Added |
|------------------|---------------|
| `http_rest` | `warp::serve`, `warp::path!`, `warp::path(`, `warp::get()` through `warp::head()`, `.and(`, `.or(`, `.and_then(`, `warp::body::json()`, `warp::body::content_length_limit(`, `warp::query::`, `warp::reply::json`, `warp::reply::with_status`, `warp::log(`, `warp::any()`, `warp::Filter`, `warp::test::request()` |
| `sse_streaming` | `warp::sse` |
| `socket` | `warp::ws()`, `warp::ws::Ws` |

Signal detection jumped from 2 to 44.

### Fix 2: Rust `*_item` Definition Nodes

Added 8 Rust-specific node types to `_DefinitionNodes.EXACT` in the AST walker:

```
function_item, struct_item, enum_item, impl_item,
trait_item, mod_item, const_item, static_item
```

This lets the walk-up algorithm correctly resolve Warp chain lines to their enclosing `function_item` or `let_declaration`, providing meaningful AST context for embeddings.

#### Failed intermediate approach: `block` as statement node

We first tried adding `block` to Rust's `_LanguageStatementNodes` to capture function bodies without signatures. This broke the test `test_if_expression_detected_as_statement`: on a line like `if req.is_valid() {`, the if body's `block` node starts on the same line as the `{`, and since tree-sitter's depth-first traversal visits the block *after* the condition nodes, the `block` becomes the deepest named node — which the walker immediately returned instead of the more meaningful `if_expression`. Removing `block` and relying solely on `*_item` definition nodes resolved the issue: `let_declaration` (via suffix) and `expression_statement` (via suffix) already catch all Rust statement-level constructs, so `block` is unnecessary.

### Results: restful-rust (44 signals, Rust, CodeT5 hf-local)

Threshold: 0.50

| Stage | Signals | Classified | Inward | Outward | Ambiguous | Noise |
|-------|---------|-----------|--------|---------|-----------|-------|
| Before (1 Warp pattern, no `*_item`) | 2 | 1 | 0 | 0 | 1 | 1 |
| After Warp expansion only (no `*_item`) | 44 | 6 | 0 | 1 | 1 | 38 |
| After both fixes | 44 | 17 (38.6%) | 9 | 7 | 1 | 27 |

### AST Context Quality

The `*_item` fix produces meaningful AST contexts that capture the full Warp filter chain:

| Signal | AST Node Type | AST Context |
|--------|--------------|-------------|
| `warp::path!("games")` (route def) | `function_item` | Full `games_list(db: Db)` function with `.and()` / `.and_then()` chain |
| `warp::test::request()` (test) | `let_declaration` | Full `let res = warp::test::request().method("GET").path(...).reply(&filter).await;` |
| `warp::serve(routes)` (server start) | `expression_statement` | `warp::serve(routes).run(([127, 0, 0, 1], 8080)).await;` |
| `warp::body::json()` (filter) | `function_item` | Full `json_body()` function |

### Direction Misclassification: `db: Db` Parameter Pollution

An interesting misclassification: 5 of the 7 outward signals are actually Warp route definitions (lines 24-28 of `routes.rs`) that *should* be `http_rest/inward`, but are classified as `database/outward` (score 0.501). The root cause is that the `function_item` AST context includes the function signature `pub fn games_list(db: Db) -> impl Filter<...>`, and the `Db` parameter in the signature pulls the embedding toward database descriptions. The nearest match is "Database results are queried and fetched" from jOOQ.

This reveals a fundamental tension in statement-level context extraction: wider context (full function) captures the filter chain structure needed for route detection, but also includes parameters that can dominate the embedding. A potential mitigation would be to extract only the function *body* (the block inside the function) without the signature, but as documented above, adding `block` as a statement node introduces its own problems with control-flow constructs.

### Score Distribution

| Signal Category | Score Range | Nearest Source |
|----------------|------------|----------------|
| `warp::serve` (server start) | 0.576 | Warp |
| `let routes = api.with(warp::log(...))` | 0.638 | Rails |
| `warp::test::request()` (test assertions) | 0.493 - 0.528 | Warp |
| Route definitions (`function_item` context) | 0.475 - 0.501 | Play, jOOQ, ServiceStack |
| `warp::body::json()` (filter) | 0.547 | Rust |

The highest-scoring signal (0.638) is `let routes = api.with(warp::log("restful_rust"))` matching "RESTful endpoints are exposed via Rails routing" — the word "routes" in the variable name and "RESTful" in the log string strongly align with the Rails description despite being Warp code.

### Verdict

The combined Warp expansion + Rust AST walker fix transforms detection from nearly blind (1/2 classified) to broadly functional (17/44 classified). The remaining challenges are:

1. **Score compression**: CodeT5 scores cluster in the 0.47-0.53 range, making threshold selection fragile. Many genuine route signals sit just above/below the 0.50 threshold.
2. **Signature pollution**: Function-level AST context includes parameter types that can override the embedding's code-structure signal, causing route definitions with database parameters to misclassify as `database/outward`.
3. **Cross-framework matching**: Signals often match descriptions from unrelated frameworks (Rails, jOOQ, Play) rather than Warp, because CodeT5 captures domain semantics ("routes", "database") rather than framework-specific API structure.
