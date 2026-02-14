"""Example: classify code snippets using Qwen via Ollama."""

import tempfile
from pathlib import Path

from repo_surveyor.ml_classifier import classify_file
from repo_surveyor.ml_classifier.qwen_model import QwenClassifierModel

SAMPLES: dict[str, tuple[str, str]] = {
    "COBOL — IDMS DML (BIND/READY/OBTAIN/STORE/MODIFY)": (
        ".cbl",
        """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUST-UPDATE.
       ENVIRONMENT DIVISION.
       IDMS-CONTROL SECTION.
           PROTOCOL. MODE IS BATCH DEBUG
                     IDMS-RECORDS MANUAL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CUST-ID        PIC X(10).
       01  WS-CUST-NAME      PIC X(40).
       01  WS-ERROR-STATUS   PIC X(04).
       01  WS-COUNTER        PIC 9(05) VALUE ZERO.
       SCHEMA SECTION.
           DB  SUBSCHEMA-CTRL.
           DB  CUSTOMER-RECORD.
       PROCEDURE DIVISION.
       MAIN-PARA.
           BIND RUN-UNIT.
           READY CUST-AREA USAGE-MODE UPDATE.
           MOVE 'C00123' TO CUST-ID IN CUSTOMER-RECORD.
           OBTAIN CALC CUSTOMER-RECORD.
           IF DB-STATUS-OK
               MOVE 'JOHN DOE' TO CUST-NAME IN CUSTOMER-RECORD
               MODIFY CUSTOMER-RECORD
               DISPLAY 'RECORD UPDATED SUCCESSFULLY'
           ELSE
               DISPLAY 'RECORD NOT FOUND'
           END-IF.
           MOVE WS-COUNTER TO WS-ERROR-STATUS.
           FINISH.
           STOP RUN.
""",
    ),
    "COBOL — CICS & DB2 (EXEC SQL, EXEC CICS)": (
        ".cbl",
        """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCT-INQ.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ACCT-ID        PIC X(10).
       01  WS-BALANCE        PIC S9(9)V99 COMP-3.
       01  WS-RESPONSE       PIC S9(08) COMP.
       01  WS-SQLCODE         PIC S9(09) COMP.
       01  WS-TAX-RATE       PIC 9V99 VALUE 0.15.
       01  WS-NET-BALANCE    PIC S9(9)V99.
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE ACCTBL END-EXEC.
       PROCEDURE DIVISION.
       MAIN-PARA.
           EXEC CICS RECEIVE
               INTO(WS-ACCT-ID)
               LENGTH(10)
               RESP(WS-RESPONSE)
           END-EXEC.
           EXEC SQL
               SELECT BALANCE
               INTO :WS-BALANCE
               FROM ACCOUNTS
               WHERE ACCT_ID = :WS-ACCT-ID
           END-EXEC.
           IF SQLCODE = 0
               COMPUTE WS-NET-BALANCE =
                   WS-BALANCE - (WS-BALANCE * WS-TAX-RATE)
               EXEC CICS SEND
                   FROM(WS-NET-BALANCE)
                   LENGTH(11)
                   RESP(WS-RESPONSE)
               END-EXEC
           ELSE
               EXEC CICS SEND
                   FROM('ACCOUNT NOT FOUND')
                   LENGTH(17)
                   RESP(WS-RESPONSE)
               END-EXEC
           END-IF.
           EXEC CICS RETURN END-EXEC.
""",
    ),
    "COBOL — MQ Series & VSAM file I/O": (
        ".cbl",
        """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORDER-PROC.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ORDER-FILE ASSIGN TO ORDERFL
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ORD-KEY
               FILE STATUS IS WS-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  ORDER-FILE.
       01  ORDER-RECORD.
           05  ORD-KEY       PIC X(10).
           05  ORD-QTY       PIC 9(05).
           05  ORD-AMOUNT    PIC S9(7)V99 COMP-3.
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS    PIC XX.
       01  WS-MQ-HCONN      PIC S9(09) COMP.
       01  WS-MQ-HOBJ       PIC S9(09) COMP.
       01  WS-MQ-REASON     PIC S9(09) COMP.
       01  WS-MQ-COMPCODE   PIC S9(09) COMP.
       01  WS-MSG-BUFFER     PIC X(1024).
       01  WS-UNIT-PRICE    PIC 9(5)V99 VALUE 12.50.
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN I-O ORDER-FILE.
           READ ORDER-FILE INTO ORDER-RECORD
               KEY IS ORD-KEY.
           IF WS-FILE-STATUS = '00'
               COMPUTE ORD-AMOUNT = ORD-QTY * WS-UNIT-PRICE
               REWRITE ORDER-RECORD
           END-IF.
           CALL 'MQCONN' USING WS-MQ-HCONN
                                WS-MQ-COMPCODE
                                WS-MQ-REASON.
           CALL 'MQOPEN' USING WS-MQ-HCONN
                                WS-MQ-HOBJ
                                WS-MQ-COMPCODE
                                WS-MQ-REASON.
           CALL 'MQPUT' USING WS-MQ-HCONN
                               WS-MQ-HOBJ
                               WS-MSG-BUFFER
                               WS-MQ-COMPCODE
                               WS-MQ-REASON.
           CALL 'MQCLOSE' USING WS-MQ-HCONN
                                 WS-MQ-HOBJ
                                 WS-MQ-COMPCODE
                                 WS-MQ-REASON.
           CLOSE ORDER-FILE.
           STOP RUN.
""",
    ),
}


def classify_snippet(model: QwenClassifierModel, label: str, suffix: str, code: str) -> None:
    with tempfile.NamedTemporaryFile(suffix=suffix, mode="w", delete=False) as f:
        f.write(code)
        path = Path(f.name)

    result = classify_file(path, model)
    path.unlink()

    print(f"=== {label} ===")
    if result is None:
        print("  Could not classify.\n")
        return

    print(f"  Lines submitted: {result.lines_submitted}  |  Skipped: {result.lines_skipped}\n")
    for cl in result.classified_lines:
        tag = cl.integration_type.value
        print(f"  L{cl.line_number:>3}  [{tag:>20}]  {cl.line_content}")
    print()


def main() -> None:
    model = QwenClassifierModel(model="qwen2.5-coder:7b-instruct")
    print(f"Model: {model.model_id}\n")

    for label, (suffix, code) in SAMPLES.items():
        classify_snippet(model, label, suffix, code)


if __name__ == "__main__":
    main()
