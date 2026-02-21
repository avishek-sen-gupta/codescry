"""Pascal base integration patterns."""

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
                    r"TIdHTTP\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Pascal Indy TIdHTTP component for HTTP client requests",
                        "This code uses Pascal Indy to make outbound HTTP requests",
                    ),
                ),
                (
                    r"TIdHTTPServer\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Pascal Indy TIdHTTPServer component for HTTP server",
                        "This code uses Pascal Indy to handle inbound HTTP server requests",
                    ),
                ),
                (
                    r"THTTPSend\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Pascal Synapse THTTPSend component for HTTP client requests",
                        "This code uses Pascal Synapse to send outbound HTTP requests",
                    ),
                ),
                (
                    r"TRESTClient\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi REST TRESTClient component for REST API client",
                        "This code uses Delphi REST components to call outbound REST APIs",
                    ),
                ),
                (
                    r"TRESTRequest\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi REST TRESTRequest component for REST request building",
                        "This code uses Delphi REST components to send outbound REST requests",
                    ),
                ),
                (
                    r"TRESTResponse\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi REST TRESTResponse component for REST response handling",
                        "This code uses Delphi REST components to receive outbound REST responses",
                    ),
                ),
                (
                    r"TNetHTTPClient\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi TNetHTTPClient component for HTTP client requests",
                        "This code uses Delphi to send outbound HTTP requests",
                    ),
                ),
                (
                    r"TNetHTTPRequest\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi TNetHTTPRequest component for HTTP request execution",
                        "This code uses Delphi to send outbound HTTP requests",
                    ),
                ),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r"THTTPRIO\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi THTTPRIO component for SOAP web service client",
                        "This code uses Delphi to call outbound SOAP web services",
                    ),
                ),
                (
                    r"THTTPReqResp\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi THTTPReqResp component for SOAP HTTP request/response",
                        "This code uses Delphi to send outbound SOAP HTTP requests",
                    ),
                ),
                (
                    r"TWSDLHTTPWebNode\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Delphi TWSDLHTTPWebNode component for SOAP web service server",
                        "This code uses Delphi to handle inbound SOAP web service calls",
                    ),
                ),
                (
                    r"InvRegistry\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Delphi InvRegistry for interface invocation registry",
                        "This code uses Delphi to interact with the invocation registry",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"TIdMessage\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Pascal Indy TIdMessage component for message handling",
                        "This code uses Pascal Indy to interact with messages",
                    ),
                ),
                (
                    r"TJMSConnection\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Pascal TJMSConnection component for JMS messaging",
                        "This code uses Pascal to interact with a JMS message broker",
                    ),
                ),
                (
                    r"TMQTTClient\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Pascal TMQTTClient component for MQTT messaging",
                        "This code uses Pascal to interact with an MQTT message broker",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"TIdTCPServer\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Pascal Indy TIdTCPServer component for TCP server",
                        "This code uses Pascal Indy to accept inbound TCP connections",
                    ),
                ),
                (
                    r"TIdTCPClient\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Pascal Indy TIdTCPClient component for TCP client",
                        "This code uses Pascal Indy to connect to outbound TCP servers",
                    ),
                ),
                (
                    r"TIdUDPServer\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Pascal Indy TIdUDPServer component for UDP server",
                        "This code uses Pascal Indy to listen for inbound UDP datagrams",
                    ),
                ),
                (
                    r"TIdUDPClient\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Pascal Indy TIdUDPClient component for UDP client",
                        "This code uses Pascal Indy to send outbound UDP datagrams",
                    ),
                ),
                (
                    r"TServerSocket\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Delphi TServerSocket component for TCP server",
                        "This code uses Delphi to accept inbound TCP socket connections",
                    ),
                ),
                (
                    r"TClientSocket\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi TClientSocket component for TCP client",
                        "This code uses Delphi to connect to outbound TCP sockets",
                    ),
                ),
                (
                    r"TSocket\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Pascal TSocket component for socket communication",
                        "This code uses Pascal to interact with network sockets",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                # BDE
                (
                    r"TTable\b",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi BDE TTable component for database table access",
                        "This code uses Delphi BDE to query a database table",
                    ),
                ),
                (
                    r"TQuery\b",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi BDE TQuery component for database SQL query",
                        "This code uses Delphi BDE to query a database with SQL",
                    ),
                ),
                (
                    r"TDatabase\b",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi BDE TDatabase component for database connectivity",
                        "This code uses Delphi BDE to connect to a database",
                    ),
                ),
                # dbExpress
                (
                    r"TSQLConnection\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi dbExpress TSQLConnection component for database connectivity",
                        "This code uses Delphi dbExpress to connect to a database",
                    ),
                ),
                (
                    r"TSQLQuery\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi dbExpress TSQLQuery component for SQL query execution",
                        "This code uses Delphi dbExpress to query a database with SQL",
                    ),
                ),
                (
                    r"TSQLDataSet\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi dbExpress TSQLDataSet component for dataset retrieval",
                        "This code uses Delphi dbExpress to query a database dataset",
                    ),
                ),
                # FireDAC
                (
                    r"TFDConnection\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Pascal FireDAC TFDConnection for database connectivity",
                        "This code uses Pascal FireDAC to connect to a database",
                    ),
                ),
                (
                    r"TFDQuery\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Pascal FireDAC TFDQuery for SQL query execution",
                        "This code uses Pascal FireDAC to query a database with SQL",
                    ),
                ),
                (
                    r"TFDTable\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Pascal FireDAC TFDTable for database table access",
                        "This code uses Pascal FireDAC to query a database table",
                    ),
                ),
                (
                    r"TFDStoredProc\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Pascal FireDAC TFDStoredProc for stored procedure execution",
                        "This code uses Pascal FireDAC to call database stored procedures",
                    ),
                ),
                # ADO
                (
                    r"TADOConnection\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi ADO TADOConnection for database connectivity",
                        "This code uses Delphi ADO to connect to a database",
                    ),
                ),
                (
                    r"TADOQuery\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi ADO TADOQuery for SQL query execution",
                        "This code uses Delphi ADO to query a database with SQL",
                    ),
                ),
                (
                    r"TADOTable\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi ADO TADOTable for database table access",
                        "This code uses Delphi ADO to query a database table",
                    ),
                ),
                (
                    r"TADODataSet\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi ADO TADODataSet for dataset retrieval",
                        "This code uses Delphi ADO to query a database dataset",
                    ),
                ),
                # IBExpress
                (
                    r"TIBDatabase\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi IBExpress TIBDatabase for InterBase/Firebird connectivity",
                        "This code uses Delphi IBExpress to connect to an InterBase database",
                    ),
                ),
                (
                    r"TIBQuery\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi IBExpress TIBQuery for SQL query execution",
                        "This code uses Delphi IBExpress to query an InterBase database",
                    ),
                ),
                (
                    r"TIBTransaction\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Delphi IBExpress TIBTransaction for database transaction management",
                        "This code uses Delphi IBExpress to write to an InterBase database transactionally",
                    ),
                ),
                # UniDAC
                (
                    r"TUniConnection\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Pascal UniDAC TUniConnection for universal database connectivity",
                        "This code uses Pascal UniDAC to connect to a database",
                    ),
                ),
                (
                    r"TUniQuery\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Pascal UniDAC TUniQuery for SQL query execution",
                        "This code uses Pascal UniDAC to query a database with SQL",
                    ),
                ),
                # Zeos
                (
                    r"TZConnection\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Pascal Zeos TZConnection for database connectivity",
                        "This code uses Pascal Zeos to connect to a database",
                    ),
                ),
                (
                    r"TZQuery\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Pascal Zeos TZQuery for SQL query execution",
                        "This code uses Pascal Zeos to query a database with SQL",
                    ),
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r"\bAssignFile\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Pascal AssignFile procedure for file handle assignment",
                        "This code uses Pascal to interact with file handles for I/O",
                    ),
                ),
                (
                    r"TFileStream\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Delphi TFileStream component for file stream I/O",
                        "This code uses Delphi to interact with file streams",
                    ),
                ),
                (
                    r"TMemoryStream\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Delphi TMemoryStream component for in-memory stream I/O",
                        "This code uses Delphi to interact with in-memory streams",
                    ),
                ),
                (
                    r"TStringStream\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Delphi TStringStream component for string-based stream I/O",
                        "This code uses Delphi to interact with string-based streams",
                    ),
                ),
                (
                    r"TBufferedFileStream\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Delphi TBufferedFileStream component for buffered file I/O",
                        "This code uses Delphi to interact with buffered file streams",
                    ),
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r"TIdSMTP\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Pascal Indy TIdSMTP component for SMTP email sending",
                        "This code uses Pascal Indy to send outbound email via SMTP",
                    ),
                ),
                (
                    r"TIdPOP3\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Pascal Indy TIdPOP3 component for POP3 email retrieval",
                        "This code uses Pascal Indy to receive inbound email via POP3",
                    ),
                ),
                (
                    r"TIdIMAP4\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Pascal Indy TIdIMAP4 component for IMAP4 email retrieval",
                        "This code uses Pascal Indy to receive inbound email via IMAP4",
                    ),
                ),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (
                    r"TIdFTP\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Pascal Indy TIdFTP component for FTP file transfer",
                        "This code uses Pascal Indy to connect to an FTP server",
                    ),
                ),
                (
                    r"TIdSFTP\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Pascal Indy TIdSFTP component for SFTP file transfer",
                        "This code uses Pascal Indy to connect to an SFTP server",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"TTimer\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Delphi TTimer component for timer-based scheduling",
                        "This code uses Delphi to interact with timer-based scheduled tasks",
                    ),
                ),
                (
                    r"\bSetTimer\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Pascal SetTimer API call for Windows timer scheduling",
                        "This code uses Pascal to interact with Windows timer scheduling",
                    ),
                ),
                (
                    r"TThread\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Delphi TThread component for background thread execution",
                        "This code uses Delphi to interact with background threads",
                    ),
                ),
            ],
        },
    },
)
