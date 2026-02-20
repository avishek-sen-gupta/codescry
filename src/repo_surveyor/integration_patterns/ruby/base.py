"""Ruby base integration patterns."""

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
                (r"Net::HTTP", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"require ['\"]net/http['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"HTTParty", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Faraday\.new", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"RestClient\.", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"require ['\"]typhoeus['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"require ['\"]savon['\"]", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Savon\.client", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"require ['\"]bunny['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"Bunny\.new", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (
                    r"require ['\"]ruby-kafka['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"Kafka::Consumer", Confidence.HIGH, SignalDirection.INWARD),
                (r"Kafka::Producer", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"require ['\"]sneakers['\"]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                ),
                (r"Sidekiq", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"TCPServer\.new", Confidence.HIGH, SignalDirection.INWARD),
                (r"TCPSocket\.new", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"UDPSocket\.new", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"UNIXSocket", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (
                    r"require ['\"]faye-websocket['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"ActiveRecord::Base", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"ApplicationRecord", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"ActiveRecord::Migration", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bhas_many\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bhas_one\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bbelongs_to\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"\bhas_and_belongs_to_many\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"\bcreate_table\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\badd_column\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\badd_index\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\badd_reference\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bremove_column\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\brename_column\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bchange_column\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\.where\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"\.find_by\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"\.joins\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"\.includes\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"\.eager_load\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"\.preload\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"\bscope\b\s+:", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"\bvalidates\b", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"\bvalidate\b\s+:", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"require ['\"]sequel['\"]", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Sequel\.connect", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require ['\"]pg['\"]", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"PG::Connection", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require ['\"]mysql2['\"]", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Mysql2::Client", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require ['\"]mongo['\"]", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Mongo::Client", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Mongoid::Document", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require ['\"]neo4j['\"]", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Neo4j::Driver", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"require ['\"]activegraph['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"File\.open\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"File\.read\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"File\.write\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"IO\.read\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"Aws::S3", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require ['\"]fog['\"]", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"CarrierWave", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"ActiveStorage", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"require ['\"]grpc['\"]", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"GRPC::RpcServer", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"GraphQL::Schema", Confidence.HIGH, SignalDirection.INWARD),
                (r"require ['\"]graphql['\"]", Confidence.HIGH, SignalDirection.INWARD),
                (r"GraphQL::ObjectType", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"ActionMailer", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"< ApplicationMailer", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Mail\.deliver", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require ['\"]mail['\"]", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"Rails\.cache", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require ['\"]redis['\"]", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Redis\.new", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require ['\"]dalli['\"]", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Dalli::Client", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"ActionController::Live", Confidence.HIGH, SignalDirection.INWARD),
                (r"SSE\.new", Confidence.HIGH, SignalDirection.INWARD),
                (r"text/event-stream", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (
                    r"require ['\"]net/ftp['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (
                    r"require ['\"]net/sftp['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"Net::FTP", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Net::SFTP", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"require ['\"]whenever['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (
                    r"require ['\"]rufus-scheduler['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (
                    r"require ['\"]clockwork['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"Rufus::Scheduler", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
