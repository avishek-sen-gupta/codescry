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

import time
from pathlib import Path

from repo_surveyor.call_flow import extract_call_tree, format_call_tree
from repo_surveyor.lsp_bridge import RequestsLspBridgeClient

BRIDGE_URL = "http://localhost:3000"
ROOT_PATH = Path("/Users/asgupta/code/smojol")
TARGET_FILE = ROOT_PATH / "smojol-core/src/main/java/org/smojol/common/vm/memory/DataLayoutBuilder.java"
FILE_URI = "file://" + str(TARGET_FILE)
ENTRY_METHOD = "layout"
INDEX_WAIT_SECONDS = 30


def main():
    print("--- Java Call-Flow Extraction via JDTLS ---\n")

    client = RequestsLspBridgeClient(BRIDGE_URL)

    print("Starting JDTLS...")
    client.start_server("java", f"file://{ROOT_PATH}")

    source_text = TARGET_FILE.read_text()
    client.open_document(FILE_URI, "java", source_text)
    print(f"Opened {TARGET_FILE.name}")

    print(f"Waiting {INDEX_WAIT_SECONDS}s for JDTLS to index the project...")
    time.sleep(INDEX_WAIT_SECONDS)

    print(f"\nTracing call flow from {ENTRY_METHOD}()...\n")
    call_tree = extract_call_tree(
        client, FILE_URI, str(TARGET_FILE), source_text, ENTRY_METHOD, "java"
    )

    print("=== Call Tree ===\n")
    print(format_call_tree(call_tree))

    print("\n=== Edges ===\n")
    for caller, callees in sorted(call_tree.edges.items()):
        for callee in sorted(callees):
            print(f"  {caller}() -> {callee}()")

    print("\nCleaning up...")
    client.close_document(FILE_URI)
    client.stop_server()
    print("Done.")


if __name__ == "__main__":
    main()
