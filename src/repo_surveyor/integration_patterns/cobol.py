"""COBOL integration patterns."""

from .types import Confidence, IntegrationType

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
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
        "patterns": [
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
        "patterns": [
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
        "patterns": [
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
        "patterns": [
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
            (r"GU\s+PCB", Confidence.HIGH),  # Get Unique
            (r"GN\s+PCB", Confidence.HIGH),  # Get Next
            (r"GNP\s+PCB", Confidence.HIGH),  # Get Next within Parent
            (r"ISRT\s+PCB", Confidence.HIGH),  # Insert
            (r"REPL\s+PCB", Confidence.HIGH),  # Replace
            (r"DLET\s+PCB", Confidence.HIGH),  # Delete
        ],
    },
}
