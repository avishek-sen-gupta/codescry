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
                (r"TIdHTTP\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TIdHTTPServer\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"THTTPSend\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TRESTClient\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TRESTRequest\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TRESTResponse\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TNetHTTPClient\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TNetHTTPRequest\b", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"THTTPRIO\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"THTTPReqResp\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TWSDLHTTPWebNode\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"InvRegistry\b", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"TIdMessage\b", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"TJMSConnection\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"TMQTTClient\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"TIdTCPServer\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"TIdTCPClient\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TIdUDPServer\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"TIdUDPClient\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TServerSocket\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"TClientSocket\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TSocket\b", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                # BDE
                (r"TTable\b", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"TQuery\b", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"TDatabase\b", Confidence.MEDIUM, SignalDirection.OUTWARD),
                # dbExpress
                (r"TSQLConnection\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TSQLQuery\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TSQLDataSet\b", Confidence.HIGH, SignalDirection.OUTWARD),
                # FireDAC
                (r"TFDConnection\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TFDQuery\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TFDTable\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TFDStoredProc\b", Confidence.HIGH, SignalDirection.OUTWARD),
                # ADO
                (r"TADOConnection\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TADOQuery\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TADOTable\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TADODataSet\b", Confidence.HIGH, SignalDirection.OUTWARD),
                # IBExpress
                (r"TIBDatabase\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TIBQuery\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TIBTransaction\b", Confidence.HIGH, SignalDirection.OUTWARD),
                # UniDAC
                (r"TUniConnection\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TUniQuery\b", Confidence.HIGH, SignalDirection.OUTWARD),
                # Zeos
                (r"TZConnection\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TZQuery\b", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"\bAssignFile\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"TFileStream\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"TMemoryStream\b", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"TStringStream\b", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"TBufferedFileStream\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"TIdSMTP\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TIdPOP3\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"TIdIMAP4\b", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (r"TIdFTP\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TIdSFTP\b", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"TTimer\b", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"\bSetTimer\b", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"TThread\b", Confidence.LOW, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
