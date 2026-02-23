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
                        "HTTP request is sent by client component",
                        "HTTP request is sent to outbound endpoint",
                    ),
                ),
                (
                    r"TIdHTTPServer\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is created for inbound requests",
                        "HTTP request is handled by inbound server",
                    ),
                ),
                (
                    r"THTTPSend\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP request is sent by client component",
                        "HTTP request is sent to outbound endpoint",
                    ),
                ),
                (
                    r"TRESTClient\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "REST API client is created by TRESTClient component",
                        "REST APIs are called for outbound requests",
                    ),
                ),
                (
                    r"TRESTRequest\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "REST request is built by TRESTRequest component",
                        "REST requests are sent for outbound calls",
                    ),
                ),
                (
                    r"TRESTResponse\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "REST response is handled by TRESTResponse component",
                        "REST responses are received from outbound calls",
                    ),
                ),
                (
                    r"TNetHTTPClient\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP client requests are made by TNetHTTPClient component",
                        "HTTP requests are sent for outbound calls",
                    ),
                ),
                (
                    r"TNetHTTPRequest\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP request is executed by TNetHTTPRequest component",
                        "HTTP requests are sent for outbound calls",
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
                        "SOAP service client is created by THTTPRIO component",
                        "SOAP web services are called for outbound requests",
                    ),
                ),
                (
                    r"THTTPReqResp\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SOAP HTTP request/response is handled by THTTPReqResp component",
                        "SOAP request is sent via HTTP outbound",
                    ),
                ),
                (
                    r"TWSDLHTTPWebNode\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "SOAP service server is created by TWSDLHTTPWebNode component",
                        "SOAP web service calls are handled for inbound requests",
                    ),
                ),
                (
                    r"InvRegistry\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Interface invocation registry is configured by InvRegistry",
                        "Invocation registry is accessed",
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
                        "Message is handled by messaging component",
                        "Message is processed by handler",
                    ),
                ),
                (
                    r"TJMSConnection\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "JMS connection is opened for messaging",
                        "Message broker is accessed via JMS",
                    ),
                ),
                (
                    r"TMQTTClient\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "MQTT connection is opened for messaging",
                        "Message broker is accessed via MQTT",
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
                        "TCP server is created for inbound connections",
                        "TCP connection is accepted for inbound requests",
                    ),
                ),
                (
                    r"TIdTCPClient\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TCP connection is opened by client",
                        "TCP connection is opened to outbound server",
                    ),
                ),
                (
                    r"TIdUDPServer\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "UDP server is created for inbound traffic",
                        "UDP datagram is received from inbound source",
                    ),
                ),
                (
                    r"TIdUDPClient\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "UDP connection is opened by client",
                        "UDP datagram is sent to outbound target",
                    ),
                ),
                (
                    r"TServerSocket\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TCP server is created by TServerSocket component",
                        "TCP socket connections are accepted for inbound traffic",
                    ),
                ),
                (
                    r"TClientSocket\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TCP client is created by TClientSocket component",
                        "TCP sockets are connected for outbound calls",
                    ),
                ),
                (
                    r"TSocket\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Socket communication is established",
                        "Network socket is accessed for communication",
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
                        "Database table is accessed by TTable component",
                        "Database table is queried via BDE",
                    ),
                ),
                (
                    r"TQuery\b",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Database query is executed by TQuery component",
                        "Database is queried with SQL via BDE",
                    ),
                ),
                (
                    r"TDatabase\b",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Database is connected via TDatabase component",
                        "Database is connected via BDE",
                    ),
                ),
                # dbExpress
                (
                    r"TSQLConnection\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database is connected via TSQLConnection component",
                        "Database is connected via dbExpress",
                    ),
                ),
                (
                    r"TSQLQuery\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL query is executed by TSQLQuery component",
                        "Database is queried with SQL via dbExpress",
                    ),
                ),
                (
                    r"TSQLDataSet\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Dataset is retrieved by TSQLDataSet component",
                        "Database dataset is queried via dbExpress",
                    ),
                ),
                # FireDAC
                (
                    r"TFDConnection\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database connection is opened with credentials",
                        "Database connection is opened with credentials",
                    ),
                ),
                (
                    r"TFDQuery\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL query is executed on database",
                        "Database is queried with SQL",
                    ),
                ),
                (
                    r"TFDTable\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database table is accessed for operations",
                        "Database table is queried for data",
                    ),
                ),
                (
                    r"TFDStoredProc\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Stored procedure is executed on database",
                        "Database procedure is called with parameters",
                    ),
                ),
                # ADO
                (
                    r"TADOConnection\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database is connected via TADOConnection component",
                        "Database is connected via ADO",
                    ),
                ),
                (
                    r"TADOQuery\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL query is executed by TADOQuery component",
                        "Database is queried with SQL via ADO",
                    ),
                ),
                (
                    r"TADOTable\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database table is accessed by TADOTable component",
                        "Database table is queried via ADO",
                    ),
                ),
                (
                    r"TADODataSet\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Dataset is retrieved by TADODataSet component",
                        "Database dataset is queried via ADO",
                    ),
                ),
                # IBExpress
                (
                    r"TIBDatabase\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "InterBase database is connected via TIBDatabase component",
                        "InterBase database is connected via IBExpress",
                    ),
                ),
                (
                    r"TIBQuery\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL query is executed by TIBQuery component",
                        "InterBase database is queried via IBExpress",
                    ),
                ),
                (
                    r"TIBTransaction\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database transaction is managed by TIBTransaction component",
                        "InterBase database is written transactionally via IBExpress",
                    ),
                ),
                # UniDAC
                (
                    r"TUniConnection\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database connection is opened with universal driver",
                        "Database connection is opened with credentials",
                    ),
                ),
                (
                    r"TUniQuery\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL query is executed on database",
                        "Database is queried with SQL",
                    ),
                ),
                # Zeos
                (
                    r"TZConnection\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database connection is opened with driver",
                        "Database connection is opened with credentials",
                    ),
                ),
                (
                    r"TZQuery\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL query is executed on database",
                        "Database is queried with SQL",
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
                        "File handle is assigned for file operations",
                        "File handle is accessed for I/O operations",
                    ),
                ),
                (
                    r"TFileStream\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File stream I/O is handled by TFileStream component",
                        "File streams are accessed",
                    ),
                ),
                (
                    r"TMemoryStream\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Memory stream I/O is handled by TMemoryStream component",
                        "Memory streams are accessed",
                    ),
                ),
                (
                    r"TStringStream\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "String stream I/O is handled by TStringStream component",
                        "String streams are accessed",
                    ),
                ),
                (
                    r"TBufferedFileStream\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File I/O is buffered by TBufferedFileStream component",
                        "File streams are accessed with buffering",
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
                        "SMTP email is sent to recipients",
                        "Email is sent via SMTP protocol",
                    ),
                ),
                (
                    r"TIdPOP3\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "POP3 email is retrieved from server",
                        "Email is received via POP3 protocol",
                    ),
                ),
                (
                    r"TIdIMAP4\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "IMAP4 email is retrieved from server",
                        "Email is received via IMAP4 protocol",
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
                        "FTP transfer is performed with remote server",
                        "FTP connection is opened for outbound transfer",
                    ),
                ),
                (
                    r"TIdSFTP\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SFTP transfer is performed with secure server",
                        "SFTP connection is opened for outbound transfer",
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
                        "Timer scheduling is handled by TTimer component",
                        "Timer-based tasks are scheduled",
                    ),
                ),
                (
                    r"\bSetTimer\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Timer is scheduled with Windows API",
                        "Timer is scheduled for Windows operations",
                    ),
                ),
                (
                    r"TThread\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Background thread is executed by TThread component",
                        "Background threads are accessed",
                    ),
                ),
            ],
        },
    },
)
