"""Extract a method call flow from a Java file using JDTLS via the mojo-lsp REST bridge.

Prerequisites:
    1. Start the mojo-lsp bridge in a separate terminal:
       cd /Users/asgupta/code/mojo-lsp && npx tsx src/bridge/bridge-cli.ts --port 3000

    2. Run this script:
       poetry run python examples/extract_java_flow.py

Target: DataLayoutBuilder.layout() in smojol-core — a self-contained class where
layout() → parseSpec(), typeSpec(), build(); typeSpec() → alphanumericLayout(), numeric();
and those methods call various numChars() overloads.
"""

import sys
import time
from pathlib import Path

import requests
from tree_sitter import Query, QueryCursor
from tree_sitter_language_pack import get_language, get_parser

BRIDGE_URL = "http://localhost:3000"
ROOT_PATH = Path("/Users/asgupta/code/smojol")
TARGET_FILE = ROOT_PATH / "smojol-core/src/main/java/org/smojol/common/vm/memory/DataLayoutBuilder.java"
FILE_URI = "file://" + str(TARGET_FILE)
ENTRY_METHOD = "layout"
INDEX_WAIT_SECONDS = 30

# LSP SymbolKind: 6 = Method, 9 = Constructor, 12 = Function
METHOD_KINDS = {6, 9, 12}

JAVA_LANGUAGE = get_language("java")
JAVA_PARSER = get_parser("java")
METHOD_INVOCATION_QUERY = Query(JAVA_LANGUAGE, "(method_invocation name: (identifier) @name)")


def start_server():
    resp = requests.post(f"{BRIDGE_URL}/start", json={
        "language": "java",
        "rootUri": f"file://{ROOT_PATH}",
    })
    resp.raise_for_status()
    print("JDTLS started.")
    return resp.json()


def open_document():
    text = TARGET_FILE.read_text()
    resp = requests.post(f"{BRIDGE_URL}/document/open", json={
        "uri": FILE_URI,
        "languageId": "java",
        "text": text,
    })
    resp.raise_for_status()
    print(f"Opened {TARGET_FILE.name}")
    return text


def get_symbols():
    resp = requests.post(f"{BRIDGE_URL}/symbols", json={"uri": FILE_URI})
    resp.raise_for_status()
    return resp.json().get("symbols") or []


def get_definition(line, character):
    resp = requests.post(f"{BRIDGE_URL}/definition", json={
        "uri": FILE_URI,
        "line": line,
        "character": character,
    })
    resp.raise_for_status()
    locations = resp.json().get("locations")
    return locations or []


def close_document():
    requests.post(f"{BRIDGE_URL}/document/close", json={"uri": FILE_URI})


def stop_server():
    requests.post(f"{BRIDGE_URL}/stop", json={})


def _base_name(symbol_name):
    """Strip parameter signature: 'layout(String)' → 'layout'."""
    paren = symbol_name.find("(")
    return symbol_name[:paren] if paren != -1 else symbol_name


def _collect_methods(symbols, out):
    """Recursively collect method symbols from a DocumentSymbol tree."""
    for sym in symbols:
        if sym["kind"] in METHOD_KINDS:
            name = _base_name(sym["name"])
            rng = sym["range"]
            out.setdefault(name, []).append({
                "start": rng["start"]["line"],
                "end": rng["end"]["line"],
            })
        for child in sym.get("children", []):
            _collect_methods([child], out)


def build_method_map(symbols):
    """Build a map of method name → list of {start, end} line ranges.

    Handles both DocumentSymbol (hierarchical, JDTLS) and SymbolInformation (flat) formats.
    Multiple entries per name handle overloaded methods (e.g. numChars).
    """
    methods = {}
    _collect_methods(symbols, methods)
    return methods


def find_method_containing(method_map, line):
    """Return the method name whose range contains the given line, or None."""
    for name, ranges in method_map.items():
        for r in ranges:
            if r["start"] <= line <= r["end"]:
                return name
    return None


def normalise_uri(uri):
    """Strip the file:// prefix for path comparison."""
    return uri.removeprefix("file://")


def extract_call_identifiers(tree, start_line, end_line):
    """Find method invocations within a line range using tree-sitter.

    Returns a list of (line, character, identifier) tuples.
    """
    cursor = QueryCursor(METHOD_INVOCATION_QUERY)
    cursor.set_point_range((start_line, 0), (end_line + 1, 0))
    captures = cursor.captures(tree.root_node)
    return [
        (node.start_point[0], node.start_point[1], node.text.decode())
        for node in captures.get("name", [])
    ]


def extract_call_tree(entry_method, method_map, tree):
    """Walk the call tree starting from entry_method using go-to-definition.

    Returns a dict: caller → set of callee method names (within the same file).
    """
    edges = {}
    visited = set()

    def walk(method_name):
        if method_name in visited:
            return
        visited.add(method_name)

        callees = set()
        for r in method_map[method_name]:
            candidates = extract_call_identifiers(tree, r["start"], r["end"])
            for line, char, ident in candidates:
                locations = get_definition(line, char)
                for loc in locations:
                    target_uri = normalise_uri(loc["uri"])
                    target_line = loc["range"]["start"]["line"]
                    if target_uri != str(TARGET_FILE):
                        continue
                    callee = find_method_containing(method_map, target_line)
                    if callee and callee != method_name:
                        callees.add(callee)

        edges[method_name] = callees
        for callee in callees:
            walk(callee)

    walk(entry_method)
    return edges


def print_call_tree(edges, root, indent=0, visited=None):
    if visited is None:
        visited = set()
    prefix = "  " * indent + ("-> " if indent > 0 else "")
    suffix = " (recursive)" if root in visited else ""
    print(f"{prefix}{root}(){suffix}")
    if root in visited:
        return
    visited.add(root)
    for callee in sorted(edges.get(root, [])):
        print_call_tree(edges, callee, indent + 1, visited)


def main():
    print("--- Java Call-Flow Extraction via JDTLS ---\n")

    print("Starting JDTLS...")
    start_server()

    source_text = open_document()
    tree = JAVA_PARSER.parse(source_text.encode("utf-8"))

    print(f"Waiting {INDEX_WAIT_SECONDS}s for JDTLS to index the project...")
    time.sleep(INDEX_WAIT_SECONDS)

    print("Fetching document symbols...")
    symbols = get_symbols()
    method_map = build_method_map(symbols)
    print(f"Found {sum(len(v) for v in method_map.values())} method symbols "
          f"({len(method_map)} unique names):\n  " +
          ", ".join(sorted(method_map.keys())))

    if ENTRY_METHOD not in method_map:
        print(f"\nEntry method '{ENTRY_METHOD}' not found in symbols. Available: {sorted(method_map.keys())}")
        close_document()
        stop_server()
        sys.exit(1)

    print(f"\nTracing call flow from {ENTRY_METHOD}()...\n")
    edges = extract_call_tree(ENTRY_METHOD, method_map, tree)

    print("=== Call Tree ===\n")
    print_call_tree(edges, ENTRY_METHOD)

    print("\n=== Edges ===\n")
    for caller, callees in sorted(edges.items()):
        for callee in sorted(callees):
            print(f"  {caller}() -> {callee}()")

    print("\nCleaning up...")
    close_document()
    stop_server()
    print("Done.")


if __name__ == "__main__":
    main()
