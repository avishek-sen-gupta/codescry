"""COBOL base integration patterns."""

from ..types import BasePatternSpec, Confidence, IntegrationType, PatternKey

BASE = BasePatternSpec(
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                # CICS Web Services
                (r"EXEC\s+CICS\s+WEB\s+", Confidence.HIGH),
                (r"EXEC\s+CICS\s+INVOKE\s+WEBSERVICE", Confidence.HIGH),
                (r"DFHWS-", Confidence.HIGH),  # CICS Web Services copybooks
                (r"WEB\s+SEND", Confidence.HIGH),
                (r"WEB\s+RECEIVE", Confidence.HIGH),
                (r"URIMAP", Confidence.MEDIUM),
                (r"WEBSERVICE", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                # SOAP/XML processing
                (r"EXEC\s+CICS\s+TRANSFORM", Confidence.HIGH),
                (r"XML\s+PARSE", Confidence.HIGH),
                (r"XML\s+GENERATE", Confidence.HIGH),
                (r"DFHWS2LS", Confidence.HIGH),  # CICS SOAP converter
                (r"DFHLS2WS", Confidence.HIGH),  # CICS SOAP converter
                (r"SOAPACTION", Confidence.HIGH),
                (r"WSDL", Confidence.MEDIUM),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                # IBM MQ (MQSeries)
                (r"MQOPEN", Confidence.HIGH),
                (r"MQPUT", Confidence.HIGH),
                (r"MQGET", Confidence.HIGH),
                (r"MQCLOSE", Confidence.HIGH),
                (r"MQCONN", Confidence.HIGH),
                (r"MQDISC", Confidence.HIGH),
                (r"MQOD", Confidence.HIGH),  # Object Descriptor
                (r"MQMD", Confidence.HIGH),  # Message Descriptor
                (r"CMQC", Confidence.HIGH),  # MQ copybook
                # CICS queues
                (r"EXEC\s+CICS\s+WRITEQ\s+TD", Confidence.HIGH),
                (r"EXEC\s+CICS\s+READQ\s+TD", Confidence.HIGH),
                (r"EXEC\s+CICS\s+WRITEQ\s+TS", Confidence.HIGH),
                (r"EXEC\s+CICS\s+READQ\s+TS", Confidence.HIGH),
                (r"EXEC\s+CICS\s+DEQ", Confidence.MEDIUM),
                (r"EXEC\s+CICS\s+ENQ", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                # TCP/IP Sockets
                (r"EZASOKET", Confidence.HIGH),  # IBM TCP/IP socket API
                (r"CALL\s+['\"]EZASOKET['\"]", Confidence.HIGH),
                (r"SOCKET-", Confidence.MEDIUM),
                (r"TCPIP", Confidence.MEDIUM),
                # CICS sockets
                (r"EXEC\s+CICS\s+EXTRACT\s+TCPIP", Confidence.HIGH),
                (r"EXEC\s+CICS\s+SOCKET", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                # Embedded SQL (DB2)
                (r"EXEC\s+SQL", Confidence.HIGH),
                (r"SQLCA", Confidence.HIGH),
                (r"SQLCODE", Confidence.HIGH),
                (r"DCLGEN", Confidence.HIGH),
                (r"INCLUDE\s+SQLCA", Confidence.HIGH),
                (r"DECLARE\s+.*\s+CURSOR", Confidence.HIGH),
                (r"FETCH\s+.*\s+INTO", Confidence.HIGH),
                (r"SELECT\s+.*\s+INTO", Confidence.HIGH),
                (r"INSERT\s+INTO", Confidence.HIGH),
                (r"UPDATE\s+.*\s+SET", Confidence.HIGH),
                (r"DELETE\s+FROM", Confidence.HIGH),
                # VSAM
                (r"READ\s+.*\s+FILE", Confidence.MEDIUM),
                (r"WRITE\s+.*\s+FILE", Confidence.MEDIUM),
                (r"REWRITE\s+.*\s+FILE", Confidence.MEDIUM),
                (r"DELETE\s+.*\s+FILE", Confidence.MEDIUM),
                (r"START\s+.*\s+FILE", Confidence.MEDIUM),
                # CICS file control
                (r"EXEC\s+CICS\s+READ", Confidence.HIGH),
                (r"EXEC\s+CICS\s+WRITE", Confidence.HIGH),
                (r"EXEC\s+CICS\s+REWRITE", Confidence.HIGH),
                (r"EXEC\s+CICS\s+DELETE", Confidence.HIGH),
                (r"EXEC\s+CICS\s+STARTBR", Confidence.HIGH),
                (r"EXEC\s+CICS\s+READNEXT", Confidence.HIGH),
                (r"EXEC\s+CICS\s+READPREV", Confidence.HIGH),
                # IMS DB
                (r"CALL\s+['\"]CBLTDLI['\"]", Confidence.HIGH),
                (r"CALL\s+['\"]PLITDLI['\"]", Confidence.HIGH),  # PL/I variant
                (r"CALL\s+['\"]AIBTDLI['\"]", Confidence.HIGH),  # AIB variant
                (r"PCB\s+MASK", Confidence.HIGH),  # PCB mask definition
                (r"SSA\s+AREA", Confidence.MEDIUM),  # Segment search argument
                (r"IO-PCB", Confidence.HIGH),  # I/O PCB
                (r"DB-PCB", Confidence.HIGH),  # Database PCB
                # CA IDMS Database
                (r"EXEC\s+IDMS", Confidence.HIGH),  # IDMS DML prefix
                (r"BIND\s+RUN-UNIT", Confidence.HIGH),
                (
                    r"READY\s+\w+\s+USAGE-MODE",
                    Confidence.HIGH,
                ),  # Ready an area with usage mode
                (r"FINISH\s+TASK", Confidence.HIGH),
                (
                    r"OBTAIN\s+(CALC|FIRST|NEXT|PRIOR|LAST|OWNER|WITHIN)",
                    Confidence.HIGH,
                ),
                (r"FIND\s+CALC", Confidence.HIGH),  # Find via CALC key
                (r"FIND\s+FIRST\s+\w+\s+WITHIN", Confidence.HIGH),
                (r"FIND\s+NEXT\s+\w+\s+WITHIN", Confidence.HIGH),
                (r"FIND\s+PRIOR", Confidence.HIGH),
                (r"FIND\s+LAST", Confidence.HIGH),
                (r"FIND\s+OWNER", Confidence.HIGH),
                (r"FIND\s+\w+\s+WITHIN\s+\w+", Confidence.HIGH),
                (r"CONNECT\s+\w+\s+TO\s+\w+", Confidence.HIGH),  # Connect to set
                (
                    r"DISCONNECT\s+\w+\s+FROM\s+\w+",
                    Confidence.HIGH,
                ),  # Disconnect from set
                (r"COMMIT\s+TASK", Confidence.HIGH),
                (r"ROLLBACK\s+TASK", Confidence.HIGH),
                (r"IDMS-STATUS", Confidence.HIGH),  # Status check
                (r"SUBSCHEMA-CTRL", Confidence.HIGH),  # Subschema control block
                (r"COPY\s+IDMS\s+", Confidence.HIGH),  # IDMS copybook
                (r"SCHEMA\s+SECTION", Confidence.HIGH),  # IDMS schema section
                # IDMS-DC (Data Communications / Online)
                (r"DC\s+RETURN", Confidence.HIGH),
                (r"GET\s+SCRATCH", Confidence.HIGH),
                (r"PUT\s+SCRATCH", Confidence.HIGH),
                (r"TRANSFER\s+CONTROL\s+TO", Confidence.HIGH),
                (r"IDMS-DC", Confidence.HIGH),  # IDMS-DC section
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"OPEN\s+INPUT", Confidence.HIGH),
                (r"OPEN\s+OUTPUT", Confidence.HIGH),
                (r"OPEN\s+I-O", Confidence.HIGH),
                (r"\bREAD\b", Confidence.MEDIUM),
                (r"\bWRITE\b", Confidence.MEDIUM),
                (r"\bCLOSE\b", Confidence.MEDIUM),
                (r"FD\s+", Confidence.HIGH),
            ],
        },
    },
)
