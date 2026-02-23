"""PL/I base integration patterns."""

from repo_surveyor.integration_patterns.types import (
    BasePatternSpec,
    Confidence,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

BASE = BasePatternSpec(
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"EXEC\s+CICS\s+WEB\s+",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP communication is handled by WEB command",
                        "HTTP requests are handled via CICS inbound",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+INVOKE\s+WEBSERVICE",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Web service is invoked by INVOKE command",
                        "Web services are called via CICS outbound",
                    ),
                ),
                (
                    r"%INCLUDE\s+DFHWS",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Web service definitions are included via DFHWS",
                        "Web service structures are accessed via CICS",
                    ),
                ),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r"PLISAXA",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SAX XML parsing is performed by PLISAXA",
                        "XML is parsed via SAX",
                    ),
                ),
                (
                    r"PLISAXB",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SAX XML parsing is performed by PLISAXB",
                        "XML is parsed via buffered SAX",
                    ),
                ),
                (
                    r"PLISAXC",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SAX XML parsing is performed by PLISAXC",
                        "XML is parsed via callback SAX",
                    ),
                ),
                (
                    r"XMLCHAR",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "XML character data is handled by XMLCHAR",
                        "XML character data is processed",
                    ),
                ),
                (
                    r"DOMTREE",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "XML DOM tree is manipulated by DOMTREE",
                        "XML is processed via DOM tree",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"CALL\s+MQOPEN",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Queue handle is opened by MQOPEN call",
                        "Message queue is accessed via MQSeries",
                    ),
                ),
                (
                    r"CALL\s+MQPUT",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Message is sent to queue by MQPUT",
                        "Messages are sent to queue via MQSeries",
                    ),
                ),
                (
                    r"CALL\s+MQGET",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Message is received from queue by MQGET",
                        "Messages are received from queue via MQSeries",
                    ),
                ),
                (
                    r"CALL\s+MQCLOSE",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Queue handle is closed by MQCLOSE call",
                        "Message queue handles are accessed via MQSeries",
                    ),
                ),
                (
                    r"CALL\s+MQCONN",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Queue manager connection is opened by MQCONN",
                        "Queue manager is accessed via MQSeries",
                    ),
                ),
                (
                    r"CALL\s+MQDISC",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Queue manager is disconnected by MQDISC call",
                        "Queue manager connections are accessed via MQSeries",
                    ),
                ),
                (
                    r"%INCLUDE\s+CMQP",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "MQ constants are included via CMQP",
                        "Message queue structures are accessed via MQSeries",
                    ),
                ),
                (
                    r"%INCLUDE\s+CMQEPP",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "MQ extended constants are included via CMQEPP",
                        "Extended message structures are accessed via MQSeries",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+WRITEQ\s+TD",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Transient data queue is written by WRITEQ TD",
                        "Data is sent to transient data queue",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+READQ\s+TD",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Transient data queue is read by READQ TD",
                        "Data is received from transient data queue",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+WRITEQ\s+TS",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Temporary storage queue is written by WRITEQ TS",
                        "Data is sent to temporary storage queue",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+READQ\s+TS",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Temporary storage queue is read by READQ TS",
                        "Data is received from temporary storage queue",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"CALL\s+EZASOKET",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TCP/IP socket operations are performed by EZASOKET",
                        "TCP sockets are accessed",
                    ),
                ),
                (
                    r"CALL\s+SOCKET",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Socket is created by SOCKET call",
                        "Network socket is accessed for communication",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+EXTRACT\s+TCPIP",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TCP/IP attributes are extracted by EXTRACT command",
                        "TCP connection is handled via CICS inbound",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+SOCKET",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Socket operations are performed by SOCKET command",
                        "Network sockets are accessed via CICS",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"EXEC\s+SQL",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database operations are performed via embedded SQL",
                        "Database queries are executed via embedded SQL",
                    ),
                ),
                (
                    r"SQLCA",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL communication area is structured by SQLCA",
                        "Relational database is queried via embedded SQL",
                    ),
                ),
                (
                    r"SQLCODE",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL return code is handled by SQLCODE",
                        "Relational database is queried via embedded SQL",
                    ),
                ),
                (
                    r"%INCLUDE\s+SQLCA",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL communication area is included via SQLCA",
                        "Relational database is queried via embedded SQL",
                    ),
                ),
                (
                    r"%INCLUDE\s+SQLDA",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL descriptor area is included via SQLDA",
                        "Database is queried dynamically via embedded SQL",
                    ),
                ),
                (
                    r"DECLARE\s+.*\s+CURSOR",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Result set cursor is declared for iteration",
                        "Database cursor is queried via embedded SQL",
                    ),
                ),
                (
                    r"FETCH\s+.*\s+INTO",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cursor row is retrieved by FETCH INTO",
                        "Cursor rows are queried via embedded SQL",
                    ),
                ),
                (
                    r"SELECT\s+.*\s+INTO",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Single row is retrieved by SELECT INTO",
                        "Single database row is queried via SQL",
                    ),
                ),
                (
                    r"INSERT\s+INTO",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database row is inserted by INSERT INTO",
                        "Database rows are written via embedded SQL",
                    ),
                ),
                (
                    r"UPDATE\s+.*\s+SET",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database row is modified by UPDATE SET",
                        "Database records are updated via embedded SQL",
                    ),
                ),
                (
                    r"DELETE\s+FROM",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database row is deleted by DELETE FROM",
                        "Database records are deleted via embedded SQL",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+READ",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "VSAM record is retrieved by READ command",
                        "VSAM record is queried via CICS",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+WRITE",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "VSAM record is written by WRITE command",
                        "VSAM record is written via CICS",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+REWRITE",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "VSAM record is updated by REWRITE command",
                        "VSAM record is updated via CICS",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+DELETE",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "VSAM record is deleted by DELETE command",
                        "VSAM record is deleted via CICS",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+STARTBR",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "VSAM browse is started by STARTBR command",
                        "VSAM records are browsed via CICS",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+READNEXT",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "VSAM records are browsed forward by READNEXT",
                        "Next VSAM record is queried via CICS",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+READPREV",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "VSAM records are browsed backward by READPREV",
                        "Previous VSAM record is queried via CICS",
                    ),
                ),
                (
                    r"CALL\s+PLITDLI",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database operations are performed by PLITDLI call",
                        "IMS database is queried via DL/I",
                    ),
                ),
                (
                    r"CALL\s+AIBTDLI",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database operations are performed by AIBTDLI call",
                        "IMS database is queried via AIB",
                    ),
                ),
                (
                    r"PCB\s+POINTER",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Program communication block is referenced by PCB POINTER",
                        "IMS database is queried via PCB",
                    ),
                ),
                (
                    r"EXEC\s+IDMS",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database operations are performed via embedded statement",
                        "IDMS network database is queried",
                    ),
                ),
                (
                    r"BIND\s+RUN-UNIT",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database session is initialized by BIND RUN-UNIT",
                        "IDMS database run unit is connected",
                    ),
                ),
                (
                    r"READY\s+\w+\s+USAGE-MODE",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Area readiness is declared by READY USAGE-MODE",
                        "IDMS database area is queried",
                    ),
                ),
                (
                    r"OBTAIN\s+(CALC|FIRST|NEXT|PRIOR|LAST|OWNER|WITHIN)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database record is retrieved by OBTAIN command",
                        "IDMS database records are queried",
                    ),
                ),
                (
                    r"FIND\s+CALC",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database record is located by FIND CALC",
                        "IDMS records are queried by CALC key",
                    ),
                ),
                (
                    r"COMMIT\s+TASK",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database transaction is committed by COMMIT TASK",
                        "IDMS transactions are committed",
                    ),
                ),
                (
                    r"ROLLBACK\s+TASK",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database transaction is rolled back by ROLLBACK TASK",
                        "IDMS transactions are rolled back",
                    ),
                ),
                (
                    r"FINISH\s+TASK",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database run-unit is terminated by FINISH TASK",
                        "IDMS run unit is committed",
                    ),
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r"OPEN\s+FILE",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File is opened by OPEN statement",
                        "File handle is accessed for I/O operations",
                    ),
                ),
                (
                    r"READ\s+FILE",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File record is read by READ statement",
                        "File records are read",
                    ),
                ),
                (
                    r"WRITE\s+FILE",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File record is written by WRITE statement",
                        "File records are written",
                    ),
                ),
                (
                    r"CLOSE\s+FILE",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File is closed by CLOSE statement",
                        "File handle is closed for I/O operations",
                    ),
                ),
                (
                    r"DECLARE\s+.*\s+FILE",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File variable is declared by DECLARE statement",
                        "File variables are declared",
                    ),
                ),
            ],
        },
    },
)
