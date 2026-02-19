"""PL/I base integration patterns."""

from ..types import (
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
                (r"EXEC\s+CICS\s+WEB\s+", Confidence.HIGH, SignalDirection.INWARD),
                (
                    r"EXEC\s+CICS\s+INVOKE\s+WEBSERVICE",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"%INCLUDE\s+DFHWS", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"PLISAXA", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"PLISAXB", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"PLISAXC", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"XMLCHAR", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"DOMTREE", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"CALL\s+MQOPEN", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"CALL\s+MQPUT", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"CALL\s+MQGET", Confidence.HIGH, SignalDirection.INWARD),
                (r"CALL\s+MQCLOSE", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"CALL\s+MQCONN", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"CALL\s+MQDISC", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"%INCLUDE\s+CMQP", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"%INCLUDE\s+CMQEPP", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (
                    r"EXEC\s+CICS\s+WRITEQ\s+TD",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"EXEC\s+CICS\s+READQ\s+TD", Confidence.HIGH, SignalDirection.INWARD),
                (
                    r"EXEC\s+CICS\s+WRITEQ\s+TS",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"EXEC\s+CICS\s+READQ\s+TS", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"CALL\s+EZASOKET", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"CALL\s+SOCKET", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (
                    r"EXEC\s+CICS\s+EXTRACT\s+TCPIP",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                ),
                (r"EXEC\s+CICS\s+SOCKET", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"EXEC\s+SQL", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"SQLCA", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"SQLCODE", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"%INCLUDE\s+SQLCA", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"%INCLUDE\s+SQLDA", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"DECLARE\s+.*\s+CURSOR", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"FETCH\s+.*\s+INTO", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"SELECT\s+.*\s+INTO", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"INSERT\s+INTO", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"UPDATE\s+.*\s+SET", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"DELETE\s+FROM", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"EXEC\s+CICS\s+READ", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"EXEC\s+CICS\s+WRITE", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"EXEC\s+CICS\s+REWRITE", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"EXEC\s+CICS\s+DELETE", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"EXEC\s+CICS\s+STARTBR", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"EXEC\s+CICS\s+READNEXT", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"EXEC\s+CICS\s+READPREV", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"CALL\s+PLITDLI", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"CALL\s+AIBTDLI", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"PCB\s+POINTER", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"EXEC\s+IDMS", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"BIND\s+RUN-UNIT", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"READY\s+\w+\s+USAGE-MODE", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"OBTAIN\s+(CALC|FIRST|NEXT|PRIOR|LAST|OWNER|WITHIN)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"FIND\s+CALC", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"COMMIT\s+TASK", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"ROLLBACK\s+TASK", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"FINISH\s+TASK", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"OPEN\s+FILE", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"READ\s+FILE", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"WRITE\s+FILE", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"CLOSE\s+FILE", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"DECLARE\s+.*\s+FILE", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
