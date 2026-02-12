"""Minimal example: jump to the definition of doSomething() via JDTLS.

Prerequisites:
    1. Start the mojo-lsp bridge:
       cd ~/code/mojo-lsp && npx tsx src/bridge/bridge-cli.ts --port 3000

    2. Have a Java project at ROOT_PATH containing the target file.

    3. Run:
       poetry run python examples/goto_definition.py
"""

import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from repo_surveyor.lsp_bridge import RequestsLspBridgeClient

BRIDGE_URL = "http://localhost:3000"
ROOT_PATH = Path("/Users/asgupta/code/smojol")
TARGET_FILE = ROOT_PATH / "smojol-core/src/main/java/org/smojol/common/navigation/DataStructureNavigator.java"
INDEX_WAIT_SECONDS = 30


def find_call_position(source: str, method_name: str) -> tuple[int, int]:
    """Find the line and character offset of a method call like .doSomething(."""
    for line_no, line in enumerate(source.splitlines()):
        col = line.find(f".{method_name}(")
        if col != -1:
            # Position the cursor on the method name itself (after the dot)
            return line_no, col + 1
    raise ValueError(f"Could not find .{method_name}( in source")


def main():
    client = RequestsLspBridgeClient(BRIDGE_URL)
    file_uri = "file://" + str(TARGET_FILE)

    # 1. Start JDTLS and open the document
    client.start_server("java", f"file://{ROOT_PATH}")
    source = TARGET_FILE.read_text()
    client.open_document(file_uri, "java", source)

    # 2. Wait for JDTLS to finish indexing
    print(f"Waiting {INDEX_WAIT_SECONDS}s for JDTLS to index...")
    time.sleep(INDEX_WAIT_SECONDS)

    # 3. Find where .doSomething( appears and jump to its definition
    line, character = find_call_position(source, "subStructures")
    print(f"Found subStructures() call at line {line}, char {character}")

    locations = client.get_definition(file_uri, line=line, character=character)

    if locations:
        for loc in locations:
            print(f"Definition: {loc.uri}:{loc.range_start_line + 1}")
    else:
        print("No definition found (JDTLS may still be indexing — try increasing the wait)")

    # 4. Clean up
    client.close_document(file_uri)
    client.stop_server()


if __name__ == "__main__":
    main()
