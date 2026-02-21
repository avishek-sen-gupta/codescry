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
                        "PL/I CICS WEB command for HTTP communication",
                        "This code uses PL/I CICS to handle inbound HTTP web requests",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+INVOKE\s+WEBSERVICE",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I CICS INVOKE WEBSERVICE command for web service calls",
                        "This code uses PL/I CICS to call outbound web services",
                    ),
                ),
                (
                    r"%INCLUDE\s+DFHWS",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PL/I CICS DFHWS include for web service definitions",
                        "This code uses PL/I CICS to interact with web service structures",
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
                        "PL/I PLISAXA built-in for SAX XML parsing",
                        "This code uses PL/I to interact with XML via SAX parsing",
                    ),
                ),
                (
                    r"PLISAXB",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PL/I PLISAXB built-in for SAX XML parsing with buffer",
                        "This code uses PL/I to interact with XML via buffered SAX parsing",
                    ),
                ),
                (
                    r"PLISAXC",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PL/I PLISAXC built-in for SAX XML parsing with callback",
                        "This code uses PL/I to interact with XML via callback-based SAX parsing",
                    ),
                ),
                (
                    r"XMLCHAR",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PL/I XMLCHAR built-in for XML character data handling",
                        "This code uses PL/I to interact with XML character data",
                    ),
                ),
                (
                    r"DOMTREE",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PL/I DOMTREE built-in for DOM XML tree manipulation",
                        "This code uses PL/I to interact with XML via DOM tree structures",
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
                        "PL/I MQSeries MQOPEN call for queue handle opening",
                        "This code uses PL/I MQSeries to interact with a message queue",
                    ),
                ),
                (
                    r"CALL\s+MQPUT",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I MQSeries MQPUT call sending a message to a queue",
                        "This code uses PL/I MQSeries to send outgoing messages to a message queue",
                    ),
                ),
                (
                    r"CALL\s+MQGET",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PL/I MQSeries MQGET call receiving a message from a queue",
                        "This code uses PL/I MQSeries to receive inbound messages from a message queue",
                    ),
                ),
                (
                    r"CALL\s+MQCLOSE",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PL/I MQSeries MQCLOSE call for queue handle closing",
                        "This code uses PL/I MQSeries to interact with message queue handles",
                    ),
                ),
                (
                    r"CALL\s+MQCONN",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PL/I MQSeries MQCONN call for queue manager connection",
                        "This code uses PL/I MQSeries to interact with a queue manager",
                    ),
                ),
                (
                    r"CALL\s+MQDISC",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PL/I MQSeries MQDISC call for queue manager disconnection",
                        "This code uses PL/I MQSeries to interact with queue manager connections",
                    ),
                ),
                (
                    r"%INCLUDE\s+CMQP",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PL/I MQSeries CMQP include for MQ constant definitions",
                        "This code uses PL/I MQSeries to interact with message queue structures",
                    ),
                ),
                (
                    r"%INCLUDE\s+CMQEPP",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PL/I MQSeries CMQEPP include for MQ extended constant definitions",
                        "This code uses PL/I MQSeries to interact with extended message queue structures",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+WRITEQ\s+TD",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I CICS WRITEQ TD command for transient data queue writing",
                        "This code uses PL/I CICS to send outgoing data to a transient data queue",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+READQ\s+TD",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PL/I CICS READQ TD command for transient data queue reading",
                        "This code uses PL/I CICS to receive inbound data from a transient data queue",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+WRITEQ\s+TS",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I CICS WRITEQ TS command for temporary storage queue writing",
                        "This code uses PL/I CICS to send outgoing data to a temporary storage queue",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+READQ\s+TS",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PL/I CICS READQ TS command for temporary storage queue reading",
                        "This code uses PL/I CICS to receive inbound data from a temporary storage queue",
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
                        "PL/I EZASOKET call for TCP/IP socket operations",
                        "This code uses PL/I to interact with TCP/IP sockets",
                    ),
                ),
                (
                    r"CALL\s+SOCKET",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PL/I SOCKET call for socket creation",
                        "This code uses PL/I to interact with network sockets",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+EXTRACT\s+TCPIP",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PL/I CICS EXTRACT TCPIP command for TCP/IP attribute extraction",
                        "This code uses PL/I CICS to handle inbound TCP/IP connection details",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+SOCKET",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PL/I CICS SOCKET command for socket operations",
                        "This code uses PL/I CICS to interact with network sockets",
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
                        "PL/I embedded SQL statement for database operations",
                        "This code uses PL/I embedded SQL to execute outbound database queries",
                    ),
                ),
                (
                    r"SQLCA",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I SQLCA structure for SQL communication area",
                        "This code uses PL/I embedded SQL to query a relational database",
                    ),
                ),
                (
                    r"SQLCODE",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I SQLCODE variable for SQL return code handling",
                        "This code uses PL/I embedded SQL to query a relational database",
                    ),
                ),
                (
                    r"%INCLUDE\s+SQLCA",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I SQLCA include for SQL communication area definition",
                        "This code uses PL/I embedded SQL to query a relational database",
                    ),
                ),
                (
                    r"%INCLUDE\s+SQLDA",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I SQLDA include for SQL descriptor area definition",
                        "This code uses PL/I embedded SQL to query a relational database dynamically",
                    ),
                ),
                (
                    r"DECLARE\s+.*\s+CURSOR",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I embedded SQL CURSOR declaration for result set iteration",
                        "This code uses PL/I embedded SQL to query a database cursor",
                    ),
                ),
                (
                    r"FETCH\s+.*\s+INTO",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I embedded SQL FETCH INTO for cursor row retrieval",
                        "This code uses PL/I embedded SQL to query rows from a database cursor",
                    ),
                ),
                (
                    r"SELECT\s+.*\s+INTO",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I embedded SQL SELECT INTO for single row retrieval",
                        "This code uses PL/I embedded SQL to query a single database row",
                    ),
                ),
                (
                    r"INSERT\s+INTO",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I embedded SQL INSERT INTO for database row insertion",
                        "This code uses PL/I embedded SQL to write rows to a database",
                    ),
                ),
                (
                    r"UPDATE\s+.*\s+SET",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I embedded SQL UPDATE SET for database row modification",
                        "This code uses PL/I embedded SQL to write updates to a database",
                    ),
                ),
                (
                    r"DELETE\s+FROM",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I embedded SQL DELETE FROM for database row deletion",
                        "This code uses PL/I embedded SQL to write deletions to a database",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+READ",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I CICS READ command for VSAM file/database record retrieval",
                        "This code uses PL/I CICS to query a VSAM database record",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+WRITE",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I CICS WRITE command for VSAM file/database record writing",
                        "This code uses PL/I CICS to write to a VSAM database record",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+REWRITE",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I CICS REWRITE command for VSAM file/database record update",
                        "This code uses PL/I CICS to write updates to a VSAM database record",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+DELETE",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I CICS DELETE command for VSAM file/database record deletion",
                        "This code uses PL/I CICS to write deletions to a VSAM database",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+STARTBR",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I CICS STARTBR command for VSAM browse operation start",
                        "This code uses PL/I CICS to query VSAM records via browse",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+READNEXT",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I CICS READNEXT command for VSAM forward browse",
                        "This code uses PL/I CICS to query the next VSAM database record",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+READPREV",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I CICS READPREV command for VSAM backward browse",
                        "This code uses PL/I CICS to query the previous VSAM database record",
                    ),
                ),
                (
                    r"CALL\s+PLITDLI",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I IMS PLITDLI call for DL/I database operations",
                        "This code uses PL/I IMS to query an IMS DL/I database",
                    ),
                ),
                (
                    r"CALL\s+AIBTDLI",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I IMS AIBTDLI call for AIB-based DL/I database operations",
                        "This code uses PL/I IMS to query an IMS DL/I database via AIB",
                    ),
                ),
                (
                    r"PCB\s+POINTER",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I IMS PCB POINTER for program communication block reference",
                        "This code uses PL/I IMS to query an IMS database via PCB",
                    ),
                ),
                (
                    r"EXEC\s+IDMS",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I IDMS embedded statement for CA IDMS database operations",
                        "This code uses PL/I IDMS to query a CA IDMS network database",
                    ),
                ),
                (
                    r"BIND\s+RUN-UNIT",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I IDMS BIND RUN-UNIT for database session initialisation",
                        "This code uses PL/I IDMS to connect to a CA IDMS database run unit",
                    ),
                ),
                (
                    r"READY\s+\w+\s+USAGE-MODE",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I IDMS READY USAGE-MODE for area readiness declaration",
                        "This code uses PL/I IDMS to query a CA IDMS database area",
                    ),
                ),
                (
                    r"OBTAIN\s+(CALC|FIRST|NEXT|PRIOR|LAST|OWNER|WITHIN)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I IDMS OBTAIN command for database record retrieval",
                        "This code uses PL/I IDMS to query records from a CA IDMS database",
                    ),
                ),
                (
                    r"FIND\s+CALC",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I IDMS FIND CALC command for database record location",
                        "This code uses PL/I IDMS to query records by CALC key in CA IDMS",
                    ),
                ),
                (
                    r"COMMIT\s+TASK",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I IDMS COMMIT TASK for database transaction commit",
                        "This code uses PL/I IDMS to write committed transactions to CA IDMS",
                    ),
                ),
                (
                    r"ROLLBACK\s+TASK",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I IDMS ROLLBACK TASK for database transaction rollback",
                        "This code uses PL/I IDMS to write transaction rollbacks to CA IDMS",
                    ),
                ),
                (
                    r"FINISH\s+TASK",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PL/I IDMS FINISH TASK for database run-unit termination",
                        "This code uses PL/I IDMS to write the final commit to a CA IDMS run unit",
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
                        "PL/I OPEN FILE statement for file opening",
                        "This code uses PL/I to interact with file handles for I/O",
                    ),
                ),
                (
                    r"READ\s+FILE",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PL/I READ FILE statement for file record reading",
                        "This code uses PL/I to interact with file data by reading records",
                    ),
                ),
                (
                    r"WRITE\s+FILE",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PL/I WRITE FILE statement for file record writing",
                        "This code uses PL/I to interact with file data by writing records",
                    ),
                ),
                (
                    r"CLOSE\s+FILE",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PL/I CLOSE FILE statement for file closing",
                        "This code uses PL/I to interact with file handles on close",
                    ),
                ),
                (
                    r"DECLARE\s+.*\s+FILE",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PL/I DECLARE FILE statement for file variable declaration",
                        "This code uses PL/I to interact with declared file variables",
                    ),
                ),
            ],
        },
    },
)
