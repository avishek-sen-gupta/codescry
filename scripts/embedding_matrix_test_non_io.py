"""Test that non-I/O code fragments do NOT classify as any integration type.

Uses the same nomic-embed-code + cosine similarity approach as embedding_matrix_test.py,
but with code fragments that are purely computational (no network, no database, no file I/O,
no messaging, etc.). The expectation is that the max cosine similarity across all 13
integration type descriptions stays below a threshold, indicating the model can distinguish
"not an integration" from actual integration code.
"""

import math
import os
import sys

import requests

ENDPOINT = os.environ["HUGGING_FACE_URL"]
TOKEN = os.environ["HUGGING_FACE_API_TOKEN"]

DESCRIPTIONS = {
    "http_rest": "Makes an HTTP or REST request to a URL, such as GET, POST, or PUT to a web API endpoint.",
    "soap": "Calls a SOAP web service using a WSDL-generated client proxy with XML envelope serialization.",
    "messaging": "Sends a message to a queue or topic via a message broker.",
    "socket": "Opens a TCP, UDP, or WebSocket connection by binding or connecting to a network port to accept or send data.",
    "database": "Executes a SQL query or ORM operation against a relational database table or document store.",
    "file_io": "Opens, reads, or writes a file on the local filesystem using file handles, streams, or path-based I/O.",
    "grpc": "Makes a gRPC remote procedure call over an HTTP/2 channel to a protobuf service.",
    "graphql": "Sends a GraphQL query or mutation using a schema with types, fields, and resolvers.",
    "email": "Sends an email message via an SMTP server.",
    "caching": "Reads or writes data to a cache store like Redis.",
    "sse_streaming": "Pushes server-sent events to a client using text/event-stream content type with data: prefix lines.",
    "scheduling": "Schedules a task to run at a specified time or interval.",
    "ftp_sftp": "Transfers a file to a remote server via FTP or SFTP.",
}

# Non-I/O code fragments: pure computation, data structures, string manipulation,
# math, sorting, validation, parsing, etc.
FRAGMENTS = {
    # ── Java ──
    (
        "Java",
        "sorting",
    ): "Collections.sort(users, Comparator.comparing(User::getLastName));",
    (
        "Java",
        "string_manipulation",
    ): "String reversed = new StringBuilder(input).reverse().toString();",
    (
        "Java",
        "math",
    ): "double distance = Math.sqrt(Math.pow(x2 - x1, 2) + Math.pow(y2 - y1, 2));",
    (
        "Java",
        "validation",
    ): 'if (email == null || !email.contains("@")) {\n    throw new IllegalArgumentException("Invalid email");\n}',
    (
        "Java",
        "data_structure",
    ): "Map<String, List<Order>> grouped = orders.stream()\n    .collect(Collectors.groupingBy(Order::getCategory));",
    ("Java", "algorithm"): "int idx = Collections.binarySearch(sortedList, target);",
    # ── Python ──
    ("Python", "sorting"): "sorted_users = sorted(users, key=lambda u: u.last_name)",
    (
        "Python",
        "string_manipulation",
    ): 'cleaned = re.sub(r"[^a-zA-Z0-9]", "", raw_input.strip().lower())',
    ("Python", "math"): "distance = math.sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2)",
    (
        "Python",
        "validation",
    ): 'assert len(password) >= 8, "Password must be at least 8 characters"',
    (
        "Python",
        "data_structure",
    ): "grouped = defaultdict(list)\nfor order in orders:\n    grouped[order.category].append(order)",
    (
        "Python",
        "algorithm",
    ): "pivot = arr[len(arr) // 2]\nleft = [x for x in arr if x < pivot]",
    # ── TypeScript ──
    (
        "TypeScript",
        "sorting",
    ): "const sorted = users.sort((a, b) => a.lastName.localeCompare(b.lastName));",
    (
        "TypeScript",
        "string_manipulation",
    ): "const slug = title.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '');",
    ("TypeScript", "math"): "const area = Math.PI * radius * radius;",
    (
        "TypeScript",
        "validation",
    ): "function isValidAge(age: number): boolean {\n    return Number.isInteger(age) && age >= 0 && age <= 150;\n}",
    (
        "TypeScript",
        "data_structure",
    ): "const map = new Map<string, number>();\nfor (const word of words) {\n    map.set(word, (map.get(word) ?? 0) + 1);\n}",
    (
        "TypeScript",
        "algorithm",
    ): "function fibonacci(n: number): number {\n    return n <= 1 ? n : fibonacci(n - 1) + fibonacci(n - 2);\n}",
    # ── JavaScript ──
    (
        "JavaScript",
        "sorting",
    ): "const sorted = [...items].sort((a, b) => a.price - b.price);",
    (
        "JavaScript",
        "string_manipulation",
    ): "const capitalized = str.charAt(0).toUpperCase() + str.slice(1);",
    (
        "JavaScript",
        "math",
    ): "const factorial = (n) => n <= 1 ? 1 : n * factorial(n - 1);",
    (
        "JavaScript",
        "validation",
    ): "const isEmail = (s) => /^[^\\s@]+@[^\\s@]+\\.[^\\s@]+$/.test(s);",
    (
        "JavaScript",
        "data_structure",
    ): "const unique = [...new Set(items.map(i => i.id))];",
    (
        "JavaScript",
        "algorithm",
    ): "function mergeSort(arr) {\n    if (arr.length <= 1) return arr;\n    const mid = Math.floor(arr.length / 2);\n    return merge(mergeSort(arr.slice(0, mid)), mergeSort(arr.slice(mid)));\n}",
    # ── Go ──
    (
        "Go",
        "sorting",
    ): "sort.Slice(users, func(i, j int) bool {\n    return users[i].LastName < users[j].LastName\n})",
    (
        "Go",
        "string_manipulation",
    ): "cleaned := strings.TrimSpace(strings.ToLower(input))",
    ("Go", "math"): "distance := math.Sqrt(math.Pow(x2-x1, 2) + math.Pow(y2-y1, 2))",
    (
        "Go",
        "validation",
    ): 'if len(password) < 8 {\n    return fmt.Errorf("password must be at least 8 characters")\n}',
    (
        "Go",
        "data_structure",
    ): "grouped := make(map[string][]Order)\nfor _, o := range orders {\n    grouped[o.Category] = append(grouped[o.Category], o)\n}",
    (
        "Go",
        "algorithm",
    ): "sort.Search(len(sorted), func(i int) bool { return sorted[i] >= target })",
    # ── Rust ──
    ("Rust", "sorting"): "users.sort_by(|a, b| a.last_name.cmp(&b.last_name));",
    (
        "Rust",
        "string_manipulation",
    ): "let cleaned: String = input.chars().filter(|c| c.is_alphanumeric()).collect();",
    ("Rust", "math"): "let distance = ((x2 - x1).powi(2) + (y2 - y1).powi(2)).sqrt();",
    ("Rust", "validation"): 'ensure!(age <= 150, "Age must be at most 150");',
    (
        "Rust",
        "data_structure",
    ): "let mut freq: HashMap<&str, usize> = HashMap::new();\nfor word in words {\n    *freq.entry(word).or_insert(0) += 1;\n}",
    (
        "Rust",
        "algorithm",
    ): "let idx = sorted.binary_search(&target).unwrap_or_else(|x| x);",
    # ── C# ──
    (
        "C#",
        "sorting",
    ): "var sorted = users.OrderBy(u => u.LastName).ThenBy(u => u.FirstName).ToList();",
    (
        "C#",
        "string_manipulation",
    ): 'var slug = Regex.Replace(title.ToLower(), @"[^a-z0-9]+", "-").Trim(\'-\');',
    (
        "C#",
        "math",
    ): "var distance = Math.Sqrt(Math.Pow(x2 - x1, 2) + Math.Pow(y2 - y1, 2));",
    (
        "C#",
        "validation",
    ): 'if (string.IsNullOrWhiteSpace(name))\n    throw new ArgumentException("Name cannot be empty");',
    (
        "C#",
        "data_structure",
    ): "var grouped = orders.GroupBy(o => o.Category).ToDictionary(g => g.Key, g => g.ToList());",
    ("C#", "algorithm"): "int idx = Array.BinarySearch(sortedArray, target);",
    # ── Kotlin ──
    ("Kotlin", "sorting"): "val sorted = users.sortedBy { it.lastName }",
    (
        "Kotlin",
        "string_manipulation",
    ): 'val slug = title.lowercase().replace(Regex("[^a-z0-9]+"), "-").trim(\'-\')',
    ("Kotlin", "math"): "val distance = sqrt((x2 - x1).pow(2) + (y2 - y1).pow(2))",
    (
        "Kotlin",
        "validation",
    ): 'require(age in 0..150) { "Age must be between 0 and 150" }',
    ("Kotlin", "data_structure"): "val grouped = orders.groupBy { it.category }",
    ("Kotlin", "algorithm"): "val idx = sortedList.binarySearch(target)",
    # ── Scala ──
    ("Scala", "sorting"): "val sorted = users.sortBy(_.lastName)",
    (
        "Scala",
        "string_manipulation",
    ): 'val cleaned = input.trim.toLowerCase.replaceAll("[^a-z0-9]", "")',
    (
        "Scala",
        "math",
    ): "val distance = math.sqrt(math.pow(x2 - x1, 2) + math.pow(y2 - y1, 2))",
    ("Scala", "validation"): 'require(age >= 0 && age <= 150, s"Invalid age: $age")',
    ("Scala", "data_structure"): "val grouped = orders.groupBy(_.category)",
    ("Scala", "algorithm"): "val result = list.foldLeft(0)(_ + _)",
    # ── Ruby ──
    ("Ruby", "sorting"): "sorted = users.sort_by(&:last_name)",
    (
        "Ruby",
        "string_manipulation",
    ): "slug = title.downcase.gsub(/[^a-z0-9]+/, '-').chomp('-')",
    ("Ruby", "math"): "distance = Math.sqrt((x2 - x1)**2 + (y2 - y1)**2)",
    (
        "Ruby",
        "validation",
    ): "raise ArgumentError, 'Name cannot be blank' if name.nil? || name.strip.empty?",
    ("Ruby", "data_structure"): "grouped = orders.group_by(&:category)",
    (
        "Ruby",
        "algorithm",
    ): "result = array.each_with_object(Hash.new(0)) { |item, counts| counts[item] += 1 }",
    # ── PHP ──
    (
        "PHP",
        "sorting",
    ): "usort($users, fn($a, $b) => strcmp($a->lastName, $b->lastName));",
    (
        "PHP",
        "string_manipulation",
    ): "$slug = preg_replace('/[^a-z0-9]+/', '-', strtolower($title));",
    ("PHP", "math"): "$distance = sqrt(pow($x2 - $x1, 2) + pow($y2 - $y1, 2));",
    (
        "PHP",
        "validation",
    ): "if (empty($name) || strlen($name) < 2) {\n    throw new InvalidArgumentException('Name is too short');\n}",
    (
        "PHP",
        "data_structure",
    ): "$grouped = [];\nforeach ($orders as $order) {\n    $grouped[$order->category][] = $order;\n}",
    (
        "PHP",
        "algorithm",
    ): "function binarySearch(array $arr, int $target): int {\n    $lo = 0; $hi = count($arr) - 1;\n    while ($lo <= $hi) { $mid = intdiv($lo + $hi, 2); }\n}",
    # ── C ──
    ("C", "sorting"): "qsort(array, n, sizeof(int), compare_ints);",
    (
        "C",
        "string_manipulation",
    ): "for (int i = 0, j = len - 1; i < j; i++, j--) {\n    char tmp = str[i]; str[i] = str[j]; str[j] = tmp;\n}",
    ("C", "math"): "double distance = sqrt(pow(x2 - x1, 2) + pow(y2 - y1, 2));",
    (
        "C",
        "validation",
    ): 'if (age < 0 || age > 150) {\n    fprintf(stderr, "Invalid age: %d\\n", age);\n    return -1;\n}',
    (
        "C",
        "data_structure",
    ): "struct Node *new_node = malloc(sizeof(struct Node));\nnew_node->data = value;\nnew_node->next = head;\nhead = new_node;",
    (
        "C",
        "algorithm",
    ): "void *result = bsearch(&key, array, n, sizeof(int), compare_ints);",
    # ── C++ ──
    (
        "C++",
        "sorting",
    ): "std::sort(users.begin(), users.end(), [](const User& a, const User& b) {\n    return a.lastName < b.lastName;\n});",
    (
        "C++",
        "string_manipulation",
    ): "std::transform(str.begin(), str.end(), str.begin(), ::tolower);",
    ("C++", "math"): "double distance = std::hypot(x2 - x1, y2 - y1);",
    (
        "C++",
        "validation",
    ): 'if (name.empty()) {\n    throw std::invalid_argument("Name cannot be empty");\n}',
    (
        "C++",
        "data_structure",
    ): "std::unordered_map<std::string, std::vector<Order>> grouped;\nfor (const auto& order : orders) {\n    grouped[order.category].push_back(order);\n}",
    (
        "C++",
        "algorithm",
    ): "auto it = std::lower_bound(sorted.begin(), sorted.end(), target);",
    # ── COBOL ──
    (
        "COBOL",
        "sorting",
    ): "           SORT WORK-FILE ON ASCENDING KEY WS-LAST-NAME\n               USING INPUT-FILE GIVING OUTPUT-FILE.",
    (
        "COBOL",
        "string_manipulation",
    ): "           INSPECT WS-INPUT TALLYING WS-COUNT FOR ALL SPACES.\n           STRING WS-FIRST-NAME DELIMITED BY SPACE ' ' DELIMITED BY SIZE WS-LAST-NAME DELIMITED BY SPACE INTO WS-FULL-NAME.",
    (
        "COBOL",
        "math",
    ): "           COMPUTE WS-DISTANCE = FUNCTION SQRT((WS-X2 - WS-X1) ** 2 + (WS-Y2 - WS-Y1) ** 2).",
    (
        "COBOL",
        "validation",
    ): "           IF WS-AGE < 0 OR WS-AGE > 150\n               DISPLAY 'INVALID AGE'\n               MOVE 'N' TO WS-VALID-FLAG\n           END-IF.",
    (
        "COBOL",
        "data_structure",
    ): "           SEARCH ALL WS-TABLE-ENTRY\n               WHEN WS-TABLE-KEY(WS-IDX) = WS-SEARCH-KEY\n                   MOVE WS-TABLE-VALUE(WS-IDX) TO WS-RESULT.",
    (
        "COBOL",
        "algorithm",
    ): "           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-COUNT\n               IF WS-ARRAY(WS-I) > WS-MAX\n                   MOVE WS-ARRAY(WS-I) TO WS-MAX\n               END-IF\n           END-PERFORM.",
    # ── PL/I ──
    (
        "PL/I",
        "sorting",
    ): "DO I = 1 TO N - 1;\n    DO J = I + 1 TO N;\n        IF NAMES(J) < NAMES(I) THEN DO;\n            TEMP = NAMES(I); NAMES(I) = NAMES(J); NAMES(J) = TEMP;\n        END;\n    END;\nEND;",
    (
        "PL/I",
        "string_manipulation",
    ): "FULL_NAME = TRIM(FIRST_NAME) || ' ' || TRIM(LAST_NAME);",
    ("PL/I", "math"): "DISTANCE = SQRT((X2 - X1) ** 2 + (Y2 - Y1) ** 2);",
    (
        "PL/I",
        "validation",
    ): "IF AGE < 0 | AGE > 150 THEN\n    PUT SKIP LIST('INVALID AGE:', AGE);",
    (
        "PL/I",
        "data_structure",
    ): "ALLOCATE LINKED_NODE;\nLINKED_NODE.VALUE = NEW_VAL;\nLINKED_NODE.NEXT = HEAD;\nHEAD = ADDR(LINKED_NODE);",
    (
        "PL/I",
        "algorithm",
    ): "MAX_VAL = ARRAY(1);\nDO I = 2 TO N;\n    IF ARRAY(I) > MAX_VAL THEN MAX_VAL = ARRAY(I);\nEND;",
    # ── Pascal ──
    (
        "Pascal",
        "sorting",
    ): "procedure QuickSort(var A: array of Integer; Lo, Hi: Integer);\nbegin\n  if Lo < Hi then begin Pivot := Partition(A, Lo, Hi); end;\nend;",
    ("Pascal", "string_manipulation"): "Result := LowerCase(Trim(Input));",
    ("Pascal", "math"): "Distance := Sqrt(Sqr(X2 - X1) + Sqr(Y2 - Y1));",
    (
        "Pascal",
        "validation",
    ): "if (Age < 0) or (Age > 150) then\n  raise Exception.Create('Invalid age');",
    (
        "Pascal",
        "data_structure",
    ): "NewNode := TNode.Create;\nNewNode.Data := Value;\nNewNode.Next := Head;\nHead := NewNode;",
    (
        "Pascal",
        "algorithm",
    ): "Result := 0;\nfor I := Low(A) to High(A) do\n  if A[I] > Result then Result := A[I];",
}


def cosine(a, b):
    dot = sum(x * y for x, y in zip(a, b))
    ma = math.sqrt(sum(x * x for x in a))
    mb = math.sqrt(sum(y * y for y in b))
    return dot / (ma * mb) if ma and mb else 0.0


def embed_batch(inputs):
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {TOKEN}",
    }
    resp = requests.post(ENDPOINT, json={"inputs": inputs}, headers=headers)
    resp.raise_for_status()
    return resp.json()


def main():
    # Embed descriptions once
    desc_keys = list(DESCRIPTIONS.keys())
    desc_texts = [DESCRIPTIONS[k] for k in desc_keys]
    print("Embedding descriptions...", file=sys.stderr)
    desc_embeddings = embed_batch(desc_texts)

    # Embed code fragments in batches
    frag_keys = list(FRAGMENTS.keys())
    frag_texts = [FRAGMENTS[k] for k in frag_keys]

    BATCH_SIZE = 32
    frag_embeddings = []
    for i in range(0, len(frag_texts), BATCH_SIZE):
        batch = frag_texts[i : i + BATCH_SIZE]
        print(
            f"Embedding fragments {i+1}-{i+len(batch)} of {len(frag_texts)}...",
            file=sys.stderr,
        )
        frag_embeddings.extend(embed_batch(batch))

    # Score each fragment against all integration types
    results_by_lang = {}
    all_max_scores = []

    for fi, (lang, frag_type) in enumerate(frag_keys):
        vec_code = frag_embeddings[fi]
        scores = {
            desc_keys[di]: cosine(vec_code, desc_embeddings[di])
            for di in range(len(desc_keys))
        }
        best_type = max(scores, key=scores.get)
        best_score = scores[best_type]
        all_max_scores.append(best_score)

        if lang not in results_by_lang:
            results_by_lang[lang] = []
        results_by_lang[lang].append(
            {
                "fragment_type": frag_type,
                "best_match": best_type,
                "best_score": best_score,
                "all_scores": scores,
            }
        )

    # Print per-language results
    print()
    print(
        f"{'Language':<14} {'Fragment':<22} {'Best Match':<16} {'Score':>6}  {'Status'}"
    )
    print("-" * 75)

    high_confidence_count = 0
    total = 0
    for lang in [
        "Java",
        "Python",
        "TypeScript",
        "JavaScript",
        "Go",
        "Rust",
        "C#",
        "C++",
        "C",
        "Kotlin",
        "Scala",
        "PHP",
        "Ruby",
        "COBOL",
        "PL/I",
        "Pascal",
    ]:
        if lang not in results_by_lang:
            continue
        for r in results_by_lang[lang]:
            total += 1
            status = "OK" if r["best_score"] < 0.45 else "HIGH"
            if r["best_score"] >= 0.45:
                high_confidence_count += 1
            print(
                f"{lang:<14} {r['fragment_type']:<22} {r['best_match']:<16} {r['best_score']:>6.3f}  {status}"
            )

    # Summary statistics
    print("-" * 75)
    avg_max = sum(all_max_scores) / len(all_max_scores) if all_max_scores else 0
    min_max = min(all_max_scores) if all_max_scores else 0
    max_max = max(all_max_scores) if all_max_scores else 0
    print(f"\nTotal fragments: {total}")
    print(
        f"Max similarity stats:  avg={avg_max:.3f}  min={min_max:.3f}  max={max_max:.3f}"
    )
    print(f"Fragments with max score >= 0.45: {high_confidence_count}/{total}")

    # Compare against I/O fragment scores (reference from embedding_matrix_test.py)
    print(
        "\nInterpretation: In the I/O test, correct integration matches typically score"
    )
    print(
        "0.45-0.85. Non-I/O fragments should ideally stay below 0.45 (the lower the better)."
    )


if __name__ == "__main__":
    main()
