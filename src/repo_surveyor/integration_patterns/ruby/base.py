"""Ruby base integration patterns."""

from ..types import BasePatternSpec, Confidence, IntegrationType, PatternKey

BASE = BasePatternSpec(
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"Net::HTTP", Confidence.HIGH),
                (r"require ['\"]net/http['\"]", Confidence.HIGH),
                (r"HTTParty", Confidence.HIGH),
                (r"Faraday\.new", Confidence.HIGH),
                (r"RestClient\.", Confidence.HIGH),
                (r"require ['\"]typhoeus['\"]", Confidence.HIGH),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"require ['\"]savon['\"]", Confidence.HIGH),
                (r"Savon\.client", Confidence.HIGH),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"require ['\"]bunny['\"]", Confidence.HIGH),
                (r"Bunny\.new", Confidence.HIGH),
                (r"require ['\"]ruby-kafka['\"]", Confidence.HIGH),
                (r"Kafka::Consumer", Confidence.HIGH),
                (r"Kafka::Producer", Confidence.HIGH),
                (r"require ['\"]sneakers['\"]", Confidence.HIGH),
                (r"Sidekiq", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"TCPServer\.new", Confidence.HIGH),
                (r"TCPSocket\.new", Confidence.HIGH),
                (r"UDPSocket\.new", Confidence.HIGH),
                (r"UNIXSocket", Confidence.HIGH),
                (r"require ['\"]faye-websocket['\"]", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"ActiveRecord::Base", Confidence.HIGH),
                (r"ApplicationRecord", Confidence.HIGH),
                (r"ActiveRecord::Migration", Confidence.HIGH),
                (r"\bhas_many\b", Confidence.HIGH),
                (r"\bhas_one\b", Confidence.HIGH),
                (r"\bbelongs_to\b", Confidence.HIGH),
                (r"\bhas_and_belongs_to_many\b", Confidence.HIGH),
                (r"\bcreate_table\b", Confidence.HIGH),
                (r"\badd_column\b", Confidence.HIGH),
                (r"\badd_index\b", Confidence.HIGH),
                (r"\badd_reference\b", Confidence.HIGH),
                (r"\bremove_column\b", Confidence.HIGH),
                (r"\brename_column\b", Confidence.HIGH),
                (r"\bchange_column\b", Confidence.HIGH),
                (r"\.where\(", Confidence.MEDIUM),
                (r"\.find_by\(", Confidence.MEDIUM),
                (r"\.joins\(", Confidence.MEDIUM),
                (r"\.includes\(", Confidence.MEDIUM),
                (r"\.eager_load\(", Confidence.MEDIUM),
                (r"\.preload\(", Confidence.MEDIUM),
                (r"\bscope\b\s+:", Confidence.MEDIUM),
                (r"\bvalidates\b", Confidence.MEDIUM),
                (r"\bvalidate\b\s+:", Confidence.MEDIUM),
                (r"require ['\"]sequel['\"]", Confidence.HIGH),
                (r"Sequel\.connect", Confidence.HIGH),
                (r"require ['\"]pg['\"]", Confidence.HIGH),
                (r"PG::Connection", Confidence.HIGH),
                (r"require ['\"]mysql2['\"]", Confidence.HIGH),
                (r"Mysql2::Client", Confidence.HIGH),
                (r"require ['\"]mongo['\"]", Confidence.HIGH),
                (r"Mongo::Client", Confidence.HIGH),
                (r"Mongoid::Document", Confidence.HIGH),
                (r"require ['\"]neo4j['\"]", Confidence.HIGH),
                (r"Neo4j::Driver", Confidence.HIGH),
                (r"require ['\"]activegraph['\"]", Confidence.HIGH),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"File\.open\(", Confidence.MEDIUM),
                (r"File\.read\(", Confidence.MEDIUM),
                (r"File\.write\(", Confidence.MEDIUM),
                (r"IO\.read\(", Confidence.MEDIUM),
                (r"Aws::S3", Confidence.HIGH),
                (r"require ['\"]fog['\"]", Confidence.HIGH),
                (r"CarrierWave", Confidence.HIGH),
                (r"ActiveStorage", Confidence.HIGH),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"require ['\"]grpc['\"]", Confidence.HIGH),
                (r"GRPC::RpcServer", Confidence.HIGH),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"GraphQL::Schema", Confidence.HIGH),
                (r"require ['\"]graphql['\"]", Confidence.HIGH),
                (r"GraphQL::ObjectType", Confidence.HIGH),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"ActionMailer", Confidence.HIGH),
                (r"< ApplicationMailer", Confidence.HIGH),
                (r"Mail\.deliver", Confidence.HIGH),
                (r"require ['\"]mail['\"]", Confidence.HIGH),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"Rails\.cache", Confidence.HIGH),
                (r"require ['\"]redis['\"]", Confidence.HIGH),
                (r"Redis\.new", Confidence.HIGH),
                (r"require ['\"]dalli['\"]", Confidence.HIGH),
                (r"Dalli::Client", Confidence.HIGH),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"ActionController::Live", Confidence.HIGH),
                (r"SSE\.new", Confidence.HIGH),
                (r"text/event-stream", Confidence.MEDIUM),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (r"require ['\"]net/ftp['\"]", Confidence.HIGH),
                (r"require ['\"]net/sftp['\"]", Confidence.HIGH),
                (r"Net::FTP", Confidence.HIGH),
                (r"Net::SFTP", Confidence.HIGH),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"require ['\"]whenever['\"]", Confidence.HIGH),
                (r"require ['\"]rufus-scheduler['\"]", Confidence.HIGH),
                (r"require ['\"]clockwork['\"]", Confidence.HIGH),
                (r"Rufus::Scheduler", Confidence.HIGH),
            ],
        },
    },
)
