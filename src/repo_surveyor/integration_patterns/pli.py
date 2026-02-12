"""PL/I integration patterns."""

from .types import Confidence, IntegrationType

BASE_PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            # CICS Web Services
            (r"EXEC\s+CICS\s+WEB\s+", Confidence.HIGH),
            (r"EXEC\s+CICS\s+INVOKE\s+WEBSERVICE", Confidence.HIGH),
            (r"%INCLUDE\s+DFHWS", Confidence.HIGH),  # CICS Web Services includes
        ],
    },
    IntegrationType.SOAP: {
        "patterns": [
            # XML processing
            (r"PLISAXA", Confidence.HIGH),  # PL/I SAX parser
            (r"PLISAXB", Confidence.HIGH),  # PL/I SAX parser (event-based)
            (r"PLISAXC", Confidence.HIGH),  # PL/I SAX parser
            (r"XMLCHAR", Confidence.HIGH),  # XML character handling
            (r"DOMTREE", Confidence.MEDIUM),  # DOM tree processing
        ],
    },
    IntegrationType.MESSAGING: {
        "patterns": [
            # IBM MQ (MQSeries)
            (r"CALL\s+MQOPEN", Confidence.HIGH),
            (r"CALL\s+MQPUT", Confidence.HIGH),
            (r"CALL\s+MQGET", Confidence.HIGH),
            (r"CALL\s+MQCLOSE", Confidence.HIGH),
            (r"CALL\s+MQCONN", Confidence.HIGH),
            (r"CALL\s+MQDISC", Confidence.HIGH),
            (r"%INCLUDE\s+CMQP", Confidence.HIGH),  # MQ PL/I includes
            (r"%INCLUDE\s+CMQEPP", Confidence.HIGH),  # MQ includes
            # CICS queues
            (r"EXEC\s+CICS\s+WRITEQ\s+TD", Confidence.HIGH),
            (r"EXEC\s+CICS\s+READQ\s+TD", Confidence.HIGH),
            (r"EXEC\s+CICS\s+WRITEQ\s+TS", Confidence.HIGH),
            (r"EXEC\s+CICS\s+READQ\s+TS", Confidence.HIGH),
        ],
    },
    IntegrationType.SOCKET: {
        "patterns": [
            # TCP/IP Sockets
            (r"CALL\s+EZASOKET", Confidence.HIGH),
            (r"CALL\s+SOCKET", Confidence.MEDIUM),
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
            (r"%INCLUDE\s+SQLCA", Confidence.HIGH),
            (r"%INCLUDE\s+SQLDA", Confidence.HIGH),
            (r"DECLARE\s+.*\s+CURSOR", Confidence.HIGH),
            (r"FETCH\s+.*\s+INTO", Confidence.HIGH),
            (r"SELECT\s+.*\s+INTO", Confidence.HIGH),
            (r"INSERT\s+INTO", Confidence.HIGH),
            (r"UPDATE\s+.*\s+SET", Confidence.HIGH),
            (r"DELETE\s+FROM", Confidence.HIGH),
            # CICS file control
            (r"EXEC\s+CICS\s+READ", Confidence.HIGH),
            (r"EXEC\s+CICS\s+WRITE", Confidence.HIGH),
            (r"EXEC\s+CICS\s+REWRITE", Confidence.HIGH),
            (r"EXEC\s+CICS\s+DELETE", Confidence.HIGH),
            (r"EXEC\s+CICS\s+STARTBR", Confidence.HIGH),
            (r"EXEC\s+CICS\s+READNEXT", Confidence.HIGH),
            (r"EXEC\s+CICS\s+READPREV", Confidence.HIGH),
            # IMS DB
            (r"CALL\s+PLITDLI", Confidence.HIGH),  # PL/I DL/I interface
            (r"CALL\s+AIBTDLI", Confidence.HIGH),  # AIB interface
            (r"PCB\s+POINTER", Confidence.HIGH),
            # CA IDMS
            (r"EXEC\s+IDMS", Confidence.HIGH),
            (r"BIND\s+RUN-UNIT", Confidence.HIGH),
            (r"READY\s+\w+\s+USAGE-MODE", Confidence.HIGH),
            (r"OBTAIN\s+(CALC|FIRST|NEXT|PRIOR|LAST|OWNER|WITHIN)", Confidence.HIGH),
            (r"FIND\s+CALC", Confidence.HIGH),
            (r"COMMIT\s+TASK", Confidence.HIGH),
            (r"ROLLBACK\s+TASK", Confidence.HIGH),
            (r"FINISH\s+TASK", Confidence.HIGH),
        ],
    },
    IntegrationType.FILE_IO: {
        "patterns": [
            (r"OPEN\s+FILE", Confidence.HIGH),
            (r"READ\s+FILE", Confidence.HIGH),
            (r"WRITE\s+FILE", Confidence.HIGH),
            (r"CLOSE\s+FILE", Confidence.HIGH),
            (r"DECLARE\s+.*\s+FILE", Confidence.HIGH),
        ],
    },
}

FRAMEWORK_PATTERNS = {}
