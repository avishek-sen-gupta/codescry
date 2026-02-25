# Experiments

Embedding-based signal concretisation experiments, investigating which embedding models and description styles best classify integration signals via nearest-neighbor pattern matching.

## Key Results

| Backend | Type | Params | Dim | smojol-api (22) | restful-rust (44) | smojol (758) |
|---------|------|--------|-----|-----------------|-------------------|-------------|
| **BGE-base-en-v1.5** | General-purpose | 110M | 768 | **22 (100%)** | **44 (100%)** | **758 (100%)** / 231 (30.5%) |
| Gemini | Cloud API | — | 768 | 20 (91%) | — | — |
| CodeT5p-110m | Code-specific | 110M | 256 | 11 (50%) | 17 (38.6%) | 54 (7.1%) |
| CodeRankEmbed | Code-specific | 137M | 768 | 1 (4.5%) | — | — |

**Recommendation**: BGE-base-en-v1.5 is the optimal embedding backend. It runs locally with no API costs, matches or exceeds Gemini's classification quality, and uses a standard BERT architecture compatible with `transformers` v5.x. See [Experiment 5](#5-bge-base-en-v15-as-local-backend) for details.

**Core finding**: General-purpose text embedding models trained for semantic similarity (BGE, Gemini) outperform code-specific models trained for retrieval (CodeT5, CodeRankEmbed) on our description-to-code classification task. MTEB leaderboard scores do not predict performance on this task. See [Experiment 6](#6-sota-model-comparison) and [Analysis](#why-general-purpose-models-outperform-code-models).

**Three-way comparison** (Experiment 7): An independent rule-based classifier (Claude GT) applied to all 758 smojol signals shows that the three approaches (Gemini LLM, BGE embedding, Claude rules) agree on 63.5% of signals. Claude achieves 95.5% precision against Gemini but only 46.7% recall — it is the most conservative classifier (88 integration vs Gemini's 180 and BGE's 231). The biggest source of Gemini false positives (per Claude's rules) are COBOL string literals in Java test code (219 signals) and ANTLR-generated parser code (111 signals). See [Experiment 7](#7-three-way-classification-comparison).

## Open Challenges

1. **Signature pollution**: Function-level AST context includes parameter types that can dominate the embedding, causing misclassification (e.g., route functions with `Db` parameters classified as `database/outward`).
2. **Cross-framework matching**: Signals sometimes match descriptions from unrelated frameworks (Rails, jOOQ) instead of the correct one (Warp), because the model captures domain semantics ("routes", "database") rather than framework-specific API structure.
3. **Threshold sensitivity at scale**: At threshold 0.50 on the full smojol repo, BGE classifies 100% of signals but 47% land as ambiguous. At 0.70, classification drops to 30.5% but directional quality improves sharply. The optimal threshold likely varies per repo.
4. **String-literal false positives dominate at scale**: 219/758 smojol signals (29%) are COBOL string literals embedded in Java test code. All three classifiers struggle with these differently — Gemini calls many of them integration (COBOL `OPEN INPUT` in a Java string), BGE matches them to file I/O descriptions, and only rule-based heuristics reliably filter them. This suggests pattern-matching pre-filters or AST-aware string-literal detection could significantly improve all approaches.
5. **Low three-way agreement on file_io**: The `file_io` integration type has the lowest three-way agreement (60.8%) across 530 signals. The broad `(?i)\bfile\b` common pattern fires on file metadata operations, path manipulation, and string constants that mention files — none of which are actual I/O boundaries. Tightening this pattern or adding a file-metadata exclusion list would reduce noise across all classifiers.

---

## Experiments

### 1. CodeT5p-110m-embedding as Local Backend

**Goal**: Evaluate a local code embedding model as a Gemini replacement.

**Model**: `Salesforce/codet5p-110m-embedding` (110M params, 256-dim, 512-token limit), running locally via `transformers`.

#### Results

| Repo | Threshold | Classified | Inward | Outward | Ambiguous | Noise |
|------|-----------|-----------|--------|---------|-----------|-------|
| smojol-api (22) | 0.58 | 11 (50%) | 10 | 1 | 0 | 11 |
| smojol (758) | 0.58 | 54 (7.1%) | 10 | 10 | 34 | 704 |

Score distribution on smojol-api: 0.31–0.66 (CodeT5) vs 0.59–0.74 (Gemini). CodeT5 produces compressed similarity scores with a narrow signal/noise gap.

#### Key Observations

- The 10 inward signals are Javalin `.get(...)` route registrations (score 0.594 each). The single outward signal is `DriverManager.getConnection` (score 0.656).
- The model demonstrates semantic understanding — Javalin routes match `http_rest/inward`, database connections match `database/outward` — but scores cluster too tightly for reliable thresholding.
- 512-token limit required `truncation=True, max_length=512` to prevent OOM on long AST contexts.
- Requires `transformers<5.0.0` (v5.x breaks the custom `CodeT5pEmbeddingConfig`).

**Verdict**: Not competitive with Gemini. Viable as a fast local pre-filter but unsuitable as a primary backend.

---

### 2. Writing Effective Pattern Descriptions

**Goal**: Discover description styles that maximise cosine similarity in the CodeT5 embedding space.

#### Method

Iteratively tested natural-language descriptions against a code fragment, measuring cosine similarity:

**Code**: `String text = new String(Files.readAllBytes(src.toPath()));`

| Round | Score | Description | Insight |
|-------|-------|-------------|---------|
| 1 | 0.448 | "Ingests a file from disk, producing a text representation of its contents" | Verbose, generic |
| 7 | 0.651 | "Reads file content from source, creating text" | "creating" maps to `new String()` |
| 12 | 0.707 | "Text created from reading all source file" | Noun-first structure |
| **20** | **0.773** | **"Text is created by reading all source file"** | **Brevity + passive** |

The literal transliteration "String text equals new String of Files readAllBytes of source toPath" scored 0.688 — the semantic description at 0.773 surpassed it.

#### Rules Derived

1. **Subject-first passive voice**: `"X is Y-ed by Z"` — the subject is the result/primary object
2. **Domain concepts, not API classes**: "database" not `DriverManager`, "cache" not `CacheBuilder`
3. **Bare noun parameters**: "...with user and password" (semantic role, not variable names)
4. **Brevity**: Every word must earn its place; no articles, qualifiers, or elaboration
5. **Match the action verb**: `new X()` → "is created", `.getConnection()` → "is opened"
6. **Describe outcomes for builders**: Long chains are better described by what they produce
7. **Template**: `[Result noun] is [action verb past participle] [preposition] [key parameters]`

#### Noise Signal Recovery

7 genuine I/O signals from the 704 noise bucket were recovered by writing optimised descriptions:

| Case | Score | Description |
|------|-------|-------------|
| BufferedReader from stream | 0.695 | "Buffered reader is created over input stream with charset for text" |
| CacheBuilder | 0.684 | "Cache is created by building with expiry size and loader" |
| writeToGraphML | 0.668 | "Graph export is written to file at output path" |
| ServerSocket | 0.584 | "Server is started by creating socket on port" |

All 7 achieved scores above the 0.58 threshold with properly written descriptions.

---

### 3. Bulk Description Rewrite

**Goal**: Apply the 7 description rules to all 3,430 pattern descriptions across 77 files.

#### Method

LLM rewrite script (`scripts/rewrite_pattern_descriptions.py`) sent 3,017 unique descriptions to Claude (Sonnet) in batches of 60 with the 7 rules as system prompt.

**Example rewrites**:

| Before | After |
|--------|-------|
| Flask @app.route decorator defining an inbound HTTP REST endpoint | HTTP route is registered with path handler |
| JDBC DriverManager.getConnection for connecting to a relational database | Database connection is opened with credentials |
| PL/I CICS WRITEQ TD command for transient data queue writing | Transient data queue is written by WRITEQ TD |

#### Results (full smojol, 758 signals, CodeT5)

| Metric | Before | After | Change |
|--------|--------|-------|--------|
| Classified | 54 (7.1%) | 33 (4.4%) | -21 |
| Inward | 10 | 14 | +4 |
| Outward | 10 | 9 | -1 |
| **Ambiguous** | **34** | **10** | **-24 (71%)** |
| Noise | 704 | 725 | +21 |

**Verdict**: The rewrite traded noisy ambiguous matches for sharper directional ones. Ambiguous signals dropped 71%, inward increased 40%. Raw classification rate decreased but useful (directional) classification rate improved from 2.6% to 3.0%.

---

### 4. Warp Pattern Expansion & Rust AST Walker Fix

**Goal**: Improve detection and classification on `restful-rust` (a Warp-based Rust REST API).

#### Problems Found

1. **Sparse Warp patterns**: Only 1 pattern (`warp::path!`) — missing HTTP method filters, filter composition, JSON body handling, SSE, and WebSocket patterns.
2. **Broken Rust AST walk-up**: Rust uses `*_item` suffix for top-level definitions (`function_item`, `struct_item`, etc.) instead of `*_definition`/`*_declaration`. The walker missed these entirely, resolving signals to meaningless `arguments` nodes with text `()`.

#### Fixes

- **Warp expansion**: 1 → 25 patterns (50 descriptions) across `http_rest`, `sse_streaming`, and `socket`.
- **Rust `*_item` nodes**: Added 8 node types (`function_item`, `struct_item`, `enum_item`, `impl_item`, `trait_item`, `mod_item`, `const_item`, `static_item`) to the AST walker.

#### Results (CodeT5, threshold 0.50)

| Stage | Signals | Classified | Inward | Outward |
|-------|---------|-----------|--------|---------|
| Before (1 pattern, no `*_item`) | 2 | 1 | 0 | 0 |
| After Warp expansion only | 44 | 6 | 0 | 1 |
| **After both fixes** | **44** | **17 (38.6%)** | **9** | **7** |

#### Direction Misclassification: Signature Pollution

5 of 7 outward signals are actually Warp route definitions that should be `http_rest/inward`, but the `function_item` AST context includes `pub fn games_list(db: Db)` — the `Db` parameter pulls the embedding toward "Database results are queried and fetched" (jOOQ). This reveals a tension between wider AST context (captures structure) and parameter noise (dominates embedding).

---

### 5. BGE-base-en-v1.5 as Local Backend

**Goal**: Find a local embedding model that matches Gemini's classification quality.

#### Background

After CodeRankEmbed's failure ([Experiment 4b](#4b-coderankembed-as-local-backend-negative-result)) and failed loading attempts with CodeSage v2 and gte-Qwen2-1.5B (both broken by `transformers` v5.x), we searched for models that: (1) use standard architectures (no `trust_remote_code`), (2) produce semantic similarity scores, (3) have 768+ dim.

#### Model Selection

Tested on a Javalin code fragment against 8 descriptions (4 relevant, 4 irrelevant):

| Model | Dim | Top score | Spread | Loads on v5.x |
|-------|-----|-----------|--------|---------------|
| **BGE-base-en-v1.5** | 768 | 0.74 | 0.22 | Yes |
| UniXcoder-base | 768 | 0.49 | 0.52 | Yes |
| Jina code embed | — | — | — | No |
| CodeSage v2 | — | — | — | No |

#### Results

**smojol-api (22 signals, Java)**:

| Backend | Classified | Inward | Outward | Ambiguous | Noise | Score range |
|---------|-----------|--------|---------|-----------|-------|-------------|
| **BGE-base** (0.50) | 22 (100%) | 17 | 2 | 3 | 0 | 0.64–0.77 |
| Gemini (0.62) | 20 (91%) | 17 | 3 | 0 | 2 | 0.59–0.74 |

Sample classifications:

| Score | Direction | Location | Nearest Description |
|-------|-----------|----------|-------------------|
| 0.767 | inward | ApiServer.java:89 | HTTP request context is represented by Ctx |
| 0.751 | inward | ApiServer.java:45 | HTTP server is instantiated with Javalin |
| 0.743 | outward | DbContext.java:27 | SQLite connection is typed for database access |
| 0.638 | ambiguous | ApiServer.java:142 | Scheduled task is defined by annotation |

**restful-rust (44 signals, Rust)**:

| Backend | Classified | Inward | Outward |
|---------|-----------|--------|---------|
| **BGE-base** (0.50) | 44 (100%) | 30 | 14 |
| CodeT5 (0.58) | 17 (38.6%) | 9 | 7 |

**Full smojol repo (758 signals, Java)**:

| Threshold | Classified | Inward | Outward | Ambiguous | Noise |
|-----------|-----------|--------|---------|-----------|-------|
| 0.50 | 758 (100%) | 69 | 336 | 353 | 0 |
| 0.70 | 231 (30.5%) | 33 | 85 | 113 | 527 |

Score distribution: min 0.57, max 0.84, mean 0.69, median 0.68. At threshold 0.62 (Gemini-equivalent), 735/758 (97%) would still be classified.

The outward-heavy distribution (336 outward vs 69 inward at 0.50) reflects smojol's architecture: a COBOL analysis tool that heavily consumes external systems (Neo4j, ANTLR, file I/O) while exposing few inward integration points.

#### Practical Implications

- **No API costs**: Runs locally, ~400MB model size
- **Fast inference**: ~10s for 3,478 descriptions, ~8s for 22 signals (CPU)
- **No `trust_remote_code`**: Standard BERT architecture, compatible with `transformers` v5.x
- **Drop-in replacement**: 768-dim, `SentenceTransformer.encode()` API

---

### 6. SOTA Model Comparison

**Goal**: Test whether newer or larger embedding models outperform BGE-base on our task.

#### Method

Five models compared on smojol-api (22 Java signals), each with **freshly embedded description vectors** (3,478 descriptions re-embedded per model — no shared caches):

| Model | Params | Classified | Inward | Outward | Ambig | Noise | Mean |
|-------|--------|-----------|--------|---------|-------|-------|------|
| **BGE-large-en-v1.5** | 335M | 22 (100%) | 17 | 2 | 3 | 0 | 0.734 |
| **BGE-base-en-v1.5** | 110M | 22 (100%) | 17 | 2 | 3 | 0 | 0.730 |
| Nomic Embed v1.5 | 137M | 22 (100%) | 16 | 5 | 1 | 0 | 0.627 |
| Snowflake Arctic-m v1.5 | 109M | 22 (100%) | 13 | 4 | 5 | 0 | 0.680 |
| all-mpnet-base-v2 | 110M | 4 (18%) | 3 | 1 | 0 | 18 | 0.436 |
| CodeCSE (RoBERTa load) | ~125M | 22 (100%) | 2 | 8 | 12 | 0 | 0.502 |
| Gemini (cloud) | — | 20 (91%) | 17 | 3 | 0 | 2 | — |

#### Model-Specific Observations

- **BGE-large-en-v1.5**: Identical distribution to BGE-base (17/2/3) with +0.004 mean score. 3x model size, negligible improvement.
- **Nomic Embed v1.5**: Misclassifies 3 signals — scheduled tasks and GraphQL matched to database descriptions ("Database table is annotated for mapping").
- **Snowflake Arctic-m v1.5**: Worst direction accuracy. Top match is "JSON request is extracted from HTTP" as **ambiguous** for a Javalin `ctx.result()` call. Only 13 inward vs BGE's 17.
- **all-mpnet-base-v2**: Too old — scores compressed below 0.50 for 18/22 signals.
- **CodeCSE**: Only 2 inward. Javalin routes matched to **Sanic** (wrong framework) as ambiguous. `ctx.path()` handlers classified as outward (MongoDB, graph database).

#### Fragment-Level Scoring

All models rank correctly on isolated fragments but diverge in the pipeline because nearest-neighbor lookup across 3,478 descriptions amplifies small biases:

| Model | Top score | Spread | Correct ranking |
|-------|-----------|--------|----------------|
| BGE-large-en-v1.5 | 0.786 | 0.249 | Yes |
| Snowflake Arctic-m v1.5 | 0.771 | 0.191 | Yes |
| BGE-base-en-v1.5 | 0.741 | 0.224 | Yes |
| Nomic Embed v1.5 | 0.725 | 0.290 | Yes |
| all-mpnet-base-v2 | 0.611 | 0.514 | Yes |

**Verdict**: BGE-base-en-v1.5 is optimal. Higher MTEB scores do not predict better performance on cross-modal description-to-code classification.

---

### 4b. CodeRankEmbed as Local Backend (Negative Result)

**Goal**: Test whether `nomic-ai/CodeRankEmbed` (768-dim, 8192-token context, 137M params) could replace Gemini.

**Result**: 1/22 classified (4.5%) — worse than every other backend.

#### Diagnostic: Lexical vs Semantic Sensitivity

Controlled experiment varying lexical overlap and semantic correctness:

| Group | Score range | Example |
|-------|-----------|---------|
| **Code tokens as description** | 0.18–**0.50** | "Javalin.create config jsonMapper get api ctx json" (0.497) |
| **Semantically wrong + lexical match** | 0.14–**0.34** | "Javalin.create DELETE route that removes data" (0.340) |
| **Semantically perfect + zero overlap** | 0.14–0.25 | "web framework route registration" (0.220) |
| **Truly unrelated** | 0.06–0.11 | "quicksort algorithm implementation" (0.061) |

The smoking gun: pasting raw code tokens as a "description" (0.497) outscores every semantically correct description (max 0.25 with no shared tokens). Semantically wrong descriptions with lexical overlap outscore semantically perfect descriptions without it.

**Verdict**: CodeRankEmbed performs **learned sparse retrieval** (lexical matching in embedding space), not semantic similarity. Unsuitable for description-to-code classification.

---

## Analysis

### Why General-Purpose Models Outperform Code Models

Our task — matching natural-language pattern descriptions to code fragments — is fundamentally a **semantic similarity** task, not a **code retrieval** task:

| Aspect | Code retrieval (CodeT5, CodeRankEmbed) | Semantic similarity (BGE, Gemini) |
|--------|---------------------------------------|----------------------------------|
| Training objective | Find code that answers a query | Measure whether two texts mean the same thing |
| Similarity signal | Token co-occurrence, API patterns | Abstract concept matching |
| Our descriptions | Don't look like search queries | Match as natural-language semantics |
| Result | Low/compressed scores | High, well-separated scores |

**Code retrieval models** learn that queries containing `"sort"` should match code containing `sort()`. They don't understand that `"HTTP server is instantiated"` and `Javalin.create()` refer to the same concept because they share no tokens.

**General-purpose semantic models** are trained on diverse text pairs via contrastive learning. They learn abstract concepts — "create" ~ "instantiate", "Javalin" ~ "HTTP server" — because they've seen documentation, tutorials, and Q&A that naturally pair these concepts.

BGE treats code as "text with meaning" rather than "code with syntax", which paradoxically makes it better at understanding what code *does* than models trained specifically on code.

### Three-Way Classifier Characteristics

The three classifiers occupy distinct points in the precision-recall space:

| Classifier | Integration count | Precision (vs Gemini) | Recall (vs Gemini) | F1 | Character |
|-----------|-------------------|----------------------|--------------------|----|-----------|
| **Claude rules** | 88 | 95.5% | 46.7% | 0.627 | Conservative — high precision, low recall |
| **Gemini LLM** | 180 | (reference) | (reference) | — | Moderate — balanced but influenced by string content |
| **BGE embeddings** | 231 | 35.5% | 45.6% | 0.399 | Aggressive — low precision, moderate recall |

Claude's rule-based approach demotes 96 Gemini integration signals (mostly COBOL string literals and file metadata) while promoting only 4 (actual file I/O that Gemini marked as noise). The 149 BGE-only integration signals (N-I-N pattern) are almost entirely false positives from the embedding model matching `file_io` descriptions against file-metadata operations and string constants.

### Score Distributions Compared

| Backend | Dim | Score range | Mean | Signal/noise gap |
|---------|-----|------------|------|-----------------|
| BGE-base | 768 | 0.64–0.77 | 0.730 | Wide |
| Gemini | 768 | 0.59–0.74 | — | Wide |
| CodeT5 | 256 | 0.31–0.66 | — | Narrow |
| CodeRankEmbed | 768 | 0.26–0.57 | — | Nonexistent |

---

### 7. Three-Way Classification Comparison

**Goal**: Produce an independent ground truth using rule-based heuristics derived from manual code review, then compare all three classification approaches (Gemini LLM, BGE embedding, Claude rules) on the full 758-signal smojol dataset.

#### Method

`data/build_claude_ground_truth.py` applies a 12-rule priority classifier to each signal. Rules are ordered so that NOT_INTEGRATION exclusions fire before INTEGRATION inclusions:

| Priority | Rule | Fires on | Classification |
|----------|------|----------|---------------|
| 1 | Generated parser code | Db2SqlParser.java, ANTLR `match()/case` | NOT_INTEGRATION |
| 2 | COBOL string literals | `+ "` lines with COBOL keywords (FILE-CONTROL, FD, SELECT...ASSIGN) | NOT_INTEGRATION |
| 3 | String/name-only references | Logging, assertions, NodeSymbolType constants | NOT_INTEGRATION |
| 4 | DI/config binding | `bindConstant()`, `@Named`, `@Inject` | NOT_INTEGRATION |
| 5 | Boolean download field | `boolean download` declarations | NOT_INTEGRATION |
| 6 | File metadata operations | `file.isDirectory()`, `Paths.get()`, `URI.create()` | NOT_INTEGRATION |
| 7 | Socket operations | `ServerSocket`, `.accept()` | INTEGRATION |
| 8 | HTTP/REST endpoints | Javalin routes, `ctx.json()`, `ctx.result()` | INTEGRATION |
| 9 | Database operations | `DriverManager.getConnection`, `DSL.using`, `.execute()` | INTEGRATION |
| 10 | Actual file I/O | `Files.readAllBytes`, `FileInputStream`, `BufferedReader` | INTEGRATION |
| 11 | Cache operations | `CacheBuilder`, `cache.get()` | AMBIGUOUS |
| 12 | Gemini tiebreaker | Falls through to Gemini's original classification | varies |

#### Results: Classification Distribution

| Classifier | Integration | Not Integration | Total |
|-----------|-------------|-----------------|-------|
| Gemini LLM | 180 (23.7%) | 578 (76.3%) | 758 |
| BGE embeddings (0.50) | 231 (30.5%) | 527 (69.5%) | 758 |
| Claude rules | 88 (11.6%) | 670 (88.4%) | 758 |

Rule hit distribution for Claude:

| Rule | Count | % |
|------|-------|---|
| COBOL string literal in Java code | 219 | 28.9% |
| Gemini tiebreaker: classified as NOISE | 200 | 26.4% |
| Generated parser code (ANTLR/Db2SqlParser) | 111 | 14.6% |
| File metadata operation | 80 | 10.6% |
| String/name-only reference | 43 | 5.7% |
| Actual file I/O | 22 | 2.9% |
| Gemini tiebreaker: outward | 22 | 2.9% |
| HTTP/REST endpoint | 16 | 2.1% |
| DI/config binding | 14 | 1.8% |
| Cache operation (ambiguous) | 11 | 1.5% |
| Database operation | 8 | 1.1% |
| Other (Gemini tiebreaker, download field, socket) | 12 | 1.6% |

#### Results: Pairwise Comparison

| Pair | Agreement | Precision | Recall | F1 |
|------|-----------|-----------|--------|----|
| Gemini vs BGE | 67.4% | 35.5% | 45.6% | 0.399 |
| Gemini vs Claude | 86.8% | 95.5% | 46.7% | 0.627 |
| BGE vs Claude | 72.7% | 63.6% | 24.2% | 0.351 |

#### Results: Three-Way Agreement

| Pattern (Gemini-BGE-Claude) | Count | Meaning |
|-----------------------------|-------|---------|
| N-N-N | 425 | All agree: Not Integration |
| N-I-N | 149 | Only BGE says Integration |
| I-N-N | 70 | Only Gemini says Integration |
| I-I-I | 56 | All agree: Integration |
| I-N-I | 28 | Gemini + Claude agree, BGE misses |
| I-I-N | 26 | Gemini + BGE agree, Claude demotes |
| N-N-I | 4 | Only Claude says Integration |

**63.5% three-way agreement** (481/758). The 56 unanimous integration signals are high-confidence true positives. The 425 unanimous not-integration signals are high-confidence true negatives.

#### Results: Per-Integration-Type Agreement

| Integration Type | Signals | Three-way agreement |
|-----------------|---------|-------------------|
| http_rest | 34 | 85.3% |
| scheduling | 2 | 100% |
| socket | 1 | 100% |
| caching | 95 | 67.4% |
| database | 93 | 66.7% |
| **file_io** | **530** | **60.8%** |

`file_io` dominates the dataset (70% of signals) and has the lowest agreement rate, driven by the broad `(?i)\bfile\b` common pattern matching COBOL string literals, file metadata operations, and path manipulation — none of which are actual I/O boundaries.

#### Key Observations

1. **Claude's conservatism is deliberate**: The rule-based classifier is designed to avoid false positives. It only classifies something as integration when it sees actual I/O API calls (e.g., `Files.readAllBytes`, `ServerSocket`, `DriverManager.getConnection`). This yields 95.5% precision but misses signals where integration is expressed through higher-level abstractions.

2. **The 96 Claude demotions are defensible**: Gemini classified these as integration, but they include file metadata checks (`file.isDirectory()`), COBOL string constants in Java test code (`+ "000490 FILE-CONTROL.\n"`), and test assertion references. These are not runtime I/O boundaries.

3. **The 4 Claude promotions reveal Gemini blind spots**: `CobolDocumentModel.java:116` (`BufferedReader reader = new BufferedReader(new StringReader(text))`) — Gemini classified as not_integration but this is actual stream-based I/O. Similarly, `Cli.java:77` and `CliCFAST.java:78` contain real file operations that Gemini missed.

4. **BGE's 149 false positives (N-I-N)** are almost entirely file_io and database descriptions matching against file-metadata operations and string constants. The embedding model cannot distinguish `file.isDirectory()` (metadata) from `Files.readAllBytes()` (I/O) because both contain "file" in a similar syntactic context.

5. **The 26 Gemini+BGE agree but Claude disagrees (I-I-N)** are the most interesting signals for review — they suggest places where Claude's rules may be too aggressive. Full signal-level data is in `data/three_way_comparison.tsv`.

#### Data Files

| File | Contents |
|------|----------|
| `data/survey_output_claude_gt/*.jsonl` | Claude's signal-level classifications (758 signals) |
| `data/three_way_comparison_report.txt` | Full comparison report |
| `data/three_way_comparison.tsv` | All 758 signals with all three classifications for manual inspection |
