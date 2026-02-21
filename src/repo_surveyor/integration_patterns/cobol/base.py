"""COBOL base integration patterns."""

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
                        "COBOL CICS WEB command handling inbound web requests",
                        "This code uses COBOL CICS to handle inbound HTTP web requests",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+INVOKE\s+WEBSERVICE",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL CICS INVOKE WEBSERVICE command calling an outbound web service",
                        "This code uses COBOL CICS to call an outbound web service",
                    ),
                ),
                (
                    r"DFHWS-",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL DFHWS- prefix referencing CICS web service data areas",
                        "This code uses COBOL CICS to interact with a web service",
                    ),
                ),
                (
                    r"WEB\s+SEND",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL CICS WEB SEND command transmitting an HTTP response",
                        "This code uses COBOL CICS to send outbound web data",
                    ),
                ),
                (
                    r"WEB\s+RECEIVE",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "COBOL CICS WEB RECEIVE command receiving inbound HTTP data",
                        "This code uses COBOL CICS to receive inbound web data",
                    ),
                ),
                (
                    r"URIMAP",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL CICS URIMAP resource defining a URI mapping",
                        "This code uses COBOL CICS to interact with a URI-mapped web endpoint",
                    ),
                ),
                (
                    r"WEBSERVICE",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL WEBSERVICE keyword referencing a web service definition",
                        "This code uses COBOL to interact with a web service",
                    ),
                ),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r"EXEC\s+CICS\s+TRANSFORM",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL CICS TRANSFORM command converting XML/SOAP data",
                        "This code uses COBOL CICS to interact with a SOAP message transform",
                    ),
                ),
                (
                    r"XML\s+PARSE",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL XML PARSE statement parsing an XML document",
                        "This code uses COBOL XML parsing to interact with XML/SOAP data",
                    ),
                ),
                (
                    r"XML\s+GENERATE",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL XML GENERATE statement producing an XML document",
                        "This code uses COBOL XML generation to interact with XML/SOAP data",
                    ),
                ),
                (
                    r"DFHWS2LS",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL DFHWS2LS CICS web service to language structure transform",
                        "This code uses COBOL CICS to interact with a SOAP web service",
                    ),
                ),
                (
                    r"DFHLS2WS",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL DFHLS2WS CICS language structure to web service transform",
                        "This code uses COBOL CICS to interact with a SOAP web service",
                    ),
                ),
                (
                    r"SOAPACTION",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL SOAPACTION header referencing a SOAP action",
                        "This code uses COBOL to interact with a SOAP web service",
                    ),
                ),
                (
                    r"WSDL",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL WSDL reference describing a SOAP web service interface",
                        "This code uses COBOL to interact with a WSDL-described web service",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"MQOPEN",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL MQ MQOPEN call opening an IBM MQ queue",
                        "This code uses COBOL IBM MQ to interact with a message queue",
                    ),
                ),
                (
                    r"MQPUT",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL MQ MQPUT call producing a message to an IBM MQ queue",
                        "This code uses COBOL IBM MQ to produce outbound messages",
                    ),
                ),
                (
                    r"MQGET",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "COBOL MQ MQGET call consuming a message from an IBM MQ queue",
                        "This code uses COBOL IBM MQ to receive inbound messages",
                    ),
                ),
                (
                    r"MQCLOSE",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL MQ MQCLOSE call closing an IBM MQ queue handle",
                        "This code uses COBOL IBM MQ to interact with a message queue",
                    ),
                ),
                (
                    r"MQCONN",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL MQ MQCONN call connecting to an IBM MQ queue manager",
                        "This code uses COBOL IBM MQ to connect to a message broker",
                    ),
                ),
                (
                    r"MQDISC",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL MQ MQDISC call disconnecting from an IBM MQ queue manager",
                        "This code uses COBOL IBM MQ to interact with a message broker",
                    ),
                ),
                (
                    r"MQOD",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL MQ MQOD object descriptor structure for IBM MQ",
                        "This code uses COBOL IBM MQ to interact with a message queue object",
                    ),
                ),
                (
                    r"MQMD",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL MQ MQMD message descriptor structure for IBM MQ",
                        "This code uses COBOL IBM MQ to interact with message metadata",
                    ),
                ),
                (
                    r"CMQC",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL CMQC copybook defining IBM MQ constants",
                        "This code uses COBOL IBM MQ to interact with a message queue",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+WRITEQ\s+TD",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL CICS WRITEQ TD command writing to a transient data queue",
                        "This code uses COBOL CICS to produce outbound transient data",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+READQ\s+TD",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "COBOL CICS READQ TD command reading from a transient data queue",
                        "This code uses COBOL CICS to receive inbound transient data",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+WRITEQ\s+TS",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL CICS WRITEQ TS command writing to a temporary storage queue",
                        "This code uses COBOL CICS to produce outbound temporary storage data",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+READQ\s+TS",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "COBOL CICS READQ TS command reading from a temporary storage queue",
                        "This code uses COBOL CICS to receive inbound temporary storage data",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+DEQ",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL CICS DEQ command releasing an enqueue resource",
                        "This code uses COBOL CICS to interact with resource locking",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+ENQ",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL CICS ENQ command acquiring an enqueue resource lock",
                        "This code uses COBOL CICS to interact with resource locking",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"EZASOKET",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL EZASOKET program name for TCP/IP socket API",
                        "This code uses COBOL TCP/IP sockets to interact with a network socket",
                    ),
                ),
                (
                    r"CALL\s+['\"]EZASOKET['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL CALL EZASOKET statement invoking the TCP/IP socket API",
                        "This code uses COBOL TCP/IP sockets to interact with a network socket",
                    ),
                ),
                (
                    r"SOCKET-",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL SOCKET- data area prefix for socket operation data",
                        "This code uses COBOL to interact with a network socket",
                    ),
                ),
                (
                    r"TCPIP",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL TCPIP reference for TCP/IP network communication",
                        "This code uses COBOL to interact with TCP/IP networking",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+EXTRACT\s+TCPIP",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "COBOL CICS EXTRACT TCPIP command retrieving inbound TCP/IP client info",
                        "This code uses COBOL CICS to receive inbound TCP/IP connection data",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+SOCKET",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL CICS SOCKET command performing socket operations",
                        "This code uses COBOL CICS to interact with a network socket",
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
                        "COBOL embedded SQL statement for database operations",
                        "This code uses COBOL embedded SQL to execute outbound database queries",
                    ),
                ),
                (
                    r"SQLCA",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL SQLCA communications area for SQL status codes",
                        "This code uses COBOL embedded SQL to interact with a database",
                    ),
                ),
                (
                    r"SQLCODE",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL SQLCODE field checking SQL return status",
                        "This code uses COBOL embedded SQL to interact with a database",
                    ),
                ),
                (
                    r"DCLGEN",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL DCLGEN generated table declaration for embedded SQL",
                        "This code uses COBOL embedded SQL to interact with a database table",
                    ),
                ),
                (
                    r"INCLUDE\s+SQLCA",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL INCLUDE SQLCA copybook importing the SQL communications area",
                        "This code uses COBOL embedded SQL to interact with a database",
                    ),
                ),
                (
                    r"DECLARE\s+.*\s+CURSOR",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL DECLARE CURSOR statement defining a database cursor",
                        "This code uses COBOL embedded SQL to query database rows",
                    ),
                ),
                (
                    r"FETCH\s+.*\s+INTO",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL FETCH INTO statement retrieving rows from a database cursor",
                        "This code uses COBOL embedded SQL to query database rows",
                    ),
                ),
                (
                    r"SELECT\s+.*\s+INTO",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL SELECT INTO statement fetching a single database row",
                        "This code uses COBOL embedded SQL to query a database",
                    ),
                ),
                (
                    r"INSERT\s+INTO",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL INSERT INTO statement writing a row to a database table",
                        "This code uses COBOL embedded SQL to write to a database",
                    ),
                ),
                (
                    r"UPDATE\s+.*\s+SET",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL UPDATE SET statement modifying rows in a database table",
                        "This code uses COBOL embedded SQL to write to a database",
                    ),
                ),
                (
                    r"DELETE\s+FROM",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL DELETE FROM statement removing rows from a database table",
                        "This code uses COBOL embedded SQL to write to a database",
                    ),
                ),
                (
                    r"READ\s+.*\s+FILE",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL READ FILE statement reading from a VSAM or sequential file",
                        "This code uses COBOL to interact with a file-based data store",
                    ),
                ),
                (
                    r"WRITE\s+.*\s+FILE",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL WRITE FILE statement writing to a VSAM or sequential file",
                        "This code uses COBOL to interact with a file-based data store",
                    ),
                ),
                (
                    r"REWRITE\s+.*\s+FILE",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL REWRITE FILE statement updating a record in a file",
                        "This code uses COBOL to interact with a file-based data store",
                    ),
                ),
                (
                    r"DELETE\s+.*\s+FILE",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL DELETE FILE statement removing a record from a file",
                        "This code uses COBOL to interact with a file-based data store",
                    ),
                ),
                (
                    r"START\s+.*\s+FILE",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL START FILE statement positioning a file cursor",
                        "This code uses COBOL to interact with a file-based data store",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+READ",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL CICS READ command reading a VSAM file record",
                        "This code uses COBOL CICS to query a VSAM data store",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+WRITE",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL CICS WRITE command writing a VSAM file record",
                        "This code uses COBOL CICS to write to a VSAM data store",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+REWRITE",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL CICS REWRITE command updating a VSAM file record",
                        "This code uses COBOL CICS to write to a VSAM data store",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+DELETE",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL CICS DELETE command removing a VSAM file record",
                        "This code uses COBOL CICS to write to a VSAM data store",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+STARTBR",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL CICS STARTBR command starting a VSAM browse operation",
                        "This code uses COBOL CICS to query a VSAM data store",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+READNEXT",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL CICS READNEXT command reading the next VSAM record in a browse",
                        "This code uses COBOL CICS to query a VSAM data store",
                    ),
                ),
                (
                    r"EXEC\s+CICS\s+READPREV",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL CICS READPREV command reading the previous VSAM record in a browse",
                        "This code uses COBOL CICS to query a VSAM data store",
                    ),
                ),
                (
                    r"CALL\s+['\"]CBLTDLI['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL CALL CBLTDLI statement invoking IMS DL/I database calls",
                        "This code uses COBOL IMS DL/I to query an IMS database",
                    ),
                ),
                (
                    r"CALL\s+['\"]PLITDLI['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL CALL PLITDLI statement invoking IMS DL/I via PL/I interface",
                        "This code uses COBOL IMS DL/I to query an IMS database",
                    ),
                ),
                (
                    r"CALL\s+['\"]AIBTDLI['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL CALL AIBTDLI statement invoking IMS DL/I via AIB interface",
                        "This code uses COBOL IMS DL/I to query an IMS database",
                    ),
                ),
                (
                    r"PCB\s+MASK",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL PCB MASK data area defining an IMS program communication block",
                        "This code uses COBOL IMS DL/I to interact with an IMS database",
                    ),
                ),
                (
                    r"SSA\s+AREA",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL SSA AREA data area defining an IMS segment search argument",
                        "This code uses COBOL IMS DL/I to query an IMS database segment",
                    ),
                ),
                (
                    r"IO-PCB",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IO-PCB I/O program communication block for IMS",
                        "This code uses COBOL IMS DL/I to interact with an IMS database",
                    ),
                ),
                (
                    r"DB-PCB",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL DB-PCB database program communication block for IMS",
                        "This code uses COBOL IMS DL/I to interact with an IMS database",
                    ),
                ),
                (
                    r"EXEC\s+IDMS",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL EXEC IDMS statement invoking a CA IDMS database operation",
                        "This code uses COBOL IDMS to query a network database",
                    ),
                ),
                (
                    r"BIND\s+RUN-UNIT",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS BIND RUN-UNIT statement initializing an IDMS run unit",
                        "This code uses COBOL IDMS to connect to a network database",
                    ),
                ),
                (
                    r"READY\s+\w+\s+USAGE-MODE",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS READY USAGE-MODE statement opening a database area",
                        "This code uses COBOL IDMS to interact with a network database area",
                    ),
                ),
                (
                    r"FINISH\s+TASK",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS FINISH TASK statement terminating an IDMS run unit",
                        "This code uses COBOL IDMS to interact with a network database",
                    ),
                ),
                (
                    r"OBTAIN\s+(CALC|FIRST|NEXT|PRIOR|LAST|OWNER|WITHIN)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS OBTAIN statement retrieving a record from a network database",
                        "This code uses COBOL IDMS to query a network database",
                    ),
                ),
                (
                    r"FIND\s+CALC",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS FIND CALC statement locating a record by calculated key",
                        "This code uses COBOL IDMS to query a network database",
                    ),
                ),
                (
                    r"FIND\s+FIRST\s+\w+\s+WITHIN",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS FIND FIRST WITHIN statement locating the first set member",
                        "This code uses COBOL IDMS to query a network database set",
                    ),
                ),
                (
                    r"FIND\s+NEXT\s+\w+\s+WITHIN",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS FIND NEXT WITHIN statement locating the next set member",
                        "This code uses COBOL IDMS to query a network database set",
                    ),
                ),
                (
                    r"FIND\s+PRIOR",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS FIND PRIOR statement locating the previous set member",
                        "This code uses COBOL IDMS to query a network database set",
                    ),
                ),
                (
                    r"FIND\s+LAST",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS FIND LAST statement locating the last set member",
                        "This code uses COBOL IDMS to query a network database set",
                    ),
                ),
                (
                    r"FIND\s+OWNER",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS FIND OWNER statement locating the owner of a set",
                        "This code uses COBOL IDMS to query a network database set",
                    ),
                ),
                (
                    r"FIND\s+\w+\s+WITHIN\s+\w+",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS FIND WITHIN statement locating a record within a set",
                        "This code uses COBOL IDMS to query a network database set",
                    ),
                ),
                (
                    r"CONNECT\s+\w+\s+TO\s+\w+",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS CONNECT TO statement inserting a record into a set",
                        "This code uses COBOL IDMS to write to a network database set",
                    ),
                ),
                (
                    r"DISCONNECT\s+\w+\s+FROM\s+\w+",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS DISCONNECT FROM statement removing a record from a set",
                        "This code uses COBOL IDMS to write to a network database set",
                    ),
                ),
                (
                    r"COMMIT\s+TASK",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS COMMIT TASK statement committing a database transaction",
                        "This code uses COBOL IDMS to write to a network database",
                    ),
                ),
                (
                    r"ROLLBACK\s+TASK",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS ROLLBACK TASK statement rolling back a database transaction",
                        "This code uses COBOL IDMS to interact with a network database transaction",
                    ),
                ),
                (
                    r"IDMS-STATUS",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS-STATUS field checking IDMS operation return status",
                        "This code uses COBOL IDMS to interact with a network database",
                    ),
                ),
                (
                    r"SUBSCHEMA-CTRL",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL SUBSCHEMA-CTRL area defining IDMS subschema control",
                        "This code uses COBOL IDMS to interact with a network database subschema",
                    ),
                ),
                (
                    r"COPY\s+IDMS\s+",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL COPY IDMS statement importing IDMS data definitions",
                        "This code uses COBOL IDMS to interact with a network database",
                    ),
                ),
                (
                    r"SCHEMA\s+SECTION",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL SCHEMA SECTION defining IDMS database schema references",
                        "This code uses COBOL IDMS to interact with a network database schema",
                    ),
                ),
                (
                    r"DC\s+RETURN",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS DC RETURN statement returning control to IDMS DC",
                        "This code uses COBOL IDMS to interact with an online database task",
                    ),
                ),
                (
                    r"GET\s+SCRATCH",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS GET SCRATCH statement retrieving scratch pad data",
                        "This code uses COBOL IDMS to query scratch pad storage",
                    ),
                ),
                (
                    r"PUT\s+SCRATCH",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS PUT SCRATCH statement storing scratch pad data",
                        "This code uses COBOL IDMS to write to scratch pad storage",
                    ),
                ),
                (
                    r"TRANSFER\s+CONTROL\s+TO",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS TRANSFER CONTROL TO statement transferring to another program",
                        "This code uses COBOL IDMS to call an outbound program",
                    ),
                ),
                (
                    r"IDMS-DC",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "COBOL IDMS-DC data area for IDMS online DC environment",
                        "This code uses COBOL IDMS to interact with an online database task",
                    ),
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r"OPEN\s+INPUT",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL OPEN INPUT statement opening a file for reading",
                        "This code uses COBOL to interact with a file for input",
                    ),
                ),
                (
                    r"OPEN\s+OUTPUT",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL OPEN OUTPUT statement opening a file for writing",
                        "This code uses COBOL to interact with a file for output",
                    ),
                ),
                (
                    r"OPEN\s+I-O",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL OPEN I-O statement opening a file for bidirectional access",
                        "This code uses COBOL to interact with a file for input and output",
                    ),
                ),
                (
                    r"\bREAD\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL READ statement reading a record from a file",
                        "This code uses COBOL to interact with a file",
                    ),
                ),
                (
                    r"\bWRITE\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL WRITE statement writing a record to a file",
                        "This code uses COBOL to interact with a file",
                    ),
                ),
                (
                    r"\bCLOSE\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL CLOSE statement closing an open file",
                        "This code uses COBOL to interact with a file",
                    ),
                ),
                (
                    r"FD\s+",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "COBOL FD file description entry declaring a file",
                        "This code uses COBOL to interact with a file",
                    ),
                ),
            ],
        },
    },
)
