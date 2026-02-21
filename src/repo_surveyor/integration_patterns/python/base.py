"""Python base integration patterns."""

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
                    r"import requests",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Python requests library for making HTTP requests",
                        "This code uses Python requests to make outbound HTTP API calls",
                    ),
                ),
                (
                    r"import httpx",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Python httpx library for async HTTP client requests",
                        "This code uses Python httpx to make outbound HTTP API calls",
                    ),
                ),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r"from zeep import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python Zeep library for SOAP web service client",
                        "This code uses Python Zeep to call outbound SOAP web services",
                    ),
                ),
                (
                    r"import zeep",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python Zeep library import for SOAP web service integration",
                        "This code uses Python Zeep to call outbound SOAP web services",
                    ),
                ),
                (
                    r"from suds",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python Suds library for SOAP web service client",
                        "This code uses Python Suds to call outbound SOAP web services",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"@\w+\.task",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Python Celery task decorator receiving incoming task messages",
                        "This code uses Python Celery to receive incoming messages from a message queue",
                    ),
                ),
                (
                    r"@shared_task",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Python Celery shared_task decorator for receiving reusable task messages",
                        "This code uses Python Celery to receive incoming messages from a message queue",
                    ),
                ),
                (
                    r"from celery import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python Celery library import for distributed task queue",
                        "This code uses Python Celery to interact with a message queue",
                    ),
                ),
                (
                    r"from kombu import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python Kombu library import for messaging and AMQP",
                        "This code uses Python Kombu to interact with a message queue",
                    ),
                ),
                (
                    r"import pika",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python Pika library import for RabbitMQ messaging",
                        "This code uses Python Pika to interact with a message queue",
                    ),
                ),
                (
                    r"from kafka import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python kafka-python library import for Kafka messaging",
                        "This code uses Python Kafka to interact with a Kafka topic",
                    ),
                ),
                (
                    r"from aiokafka import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python aiokafka library import for async Kafka messaging",
                        "This code uses Python aiokafka to interact with a Kafka topic",
                    ),
                ),
                (
                    r"KafkaConsumer",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Python KafkaConsumer for consuming messages from Kafka",
                        "This code uses Python Kafka to consume incoming messages from a Kafka topic",
                    ),
                ),
                (
                    r"KafkaProducer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python KafkaProducer for producing messages to Kafka",
                        "This code uses Python Kafka to send outgoing messages to a Kafka topic",
                    ),
                ),
                (
                    r"google\.cloud.*pubsub",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python Google Cloud Pub/Sub library for cloud messaging",
                        "This code uses Python Google Cloud to interact with a message queue",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"import websockets",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python websockets library import for WebSocket communication",
                        "This code uses Python websockets to interact with a WebSocket connection",
                    ),
                ),
                (
                    r"from websockets import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python websockets library for WebSocket communication",
                        "This code uses Python websockets to interact with a WebSocket connection",
                    ),
                ),
                (
                    r"import socketio",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python Socket.IO library import for real-time event communication",
                        "This code uses Python socketio to interact with a WebSocket connection",
                    ),
                ),
                (
                    r"from socketio import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python Socket.IO library for real-time event communication",
                        "This code uses Python socketio to interact with a WebSocket connection",
                    ),
                ),
                (
                    r"@socketio\.on",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Python Socket.IO event handler decorator listening for incoming events",
                        "This code uses Python socketio to listen for incoming WebSocket connection events",
                    ),
                ),
                (
                    r"@sio\.on",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Python Socket.IO instance event handler decorator listening for incoming events",
                        "This code uses Python socketio to listen for incoming WebSocket connection events",
                    ),
                ),
                (
                    r"import socket\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python socket library import for low-level network socket communication",
                        "This code uses Python socket to interact with a network socket",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"from sqlalchemy import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python SQLAlchemy ORM for database operations",
                        "This code uses Python SQLAlchemy to interact with a relational database",
                    ),
                ),
                (
                    r"import sqlalchemy",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python SQLAlchemy library import for database operations",
                        "This code uses Python SQLAlchemy to interact with a relational database",
                    ),
                ),
                (
                    r"from peewee import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python Peewee ORM for database operations",
                        "This code uses Python Peewee to interact with a relational database",
                    ),
                ),
                (
                    r"from tortoise",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python Tortoise ORM for async database operations",
                        "This code uses Python Tortoise to interact with a relational database",
                    ),
                ),
                (
                    r"import psycopg",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python psycopg driver for PostgreSQL database access",
                        "This code uses Python psycopg to query a relational database",
                    ),
                ),
                (
                    r"import pymysql",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python PyMySQL driver for MySQL database access",
                        "This code uses Python PyMySQL to query a relational database",
                    ),
                ),
                (
                    r"import asyncpg",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python asyncpg driver for async PostgreSQL database access",
                        "This code uses Python asyncpg to query a relational database",
                    ),
                ),
                (
                    r"@declarative",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python SQLAlchemy declarative base decorator for ORM model definition",
                        "This code uses Python SQLAlchemy to define a database model for persistence",
                    ),
                ),
                (
                    r"Column\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Python ORM Column definition for database schema mapping",
                        "This code uses Python ORM to define a database model for persistence",
                    ),
                ),
                (
                    r"relationship\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Python ORM relationship definition for database model associations",
                        "This code uses Python ORM to interact with a relational database",
                    ),
                ),
                (
                    r"from google\.cloud import firestore",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python Google Cloud Firestore client for NoSQL database access",
                        "This code uses Python Google Cloud to write to a database",
                    ),
                ),
                (
                    r"from cassandra\.cluster",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python Cassandra driver for distributed NoSQL database access",
                        "This code uses Python Cassandra to query a database",
                    ),
                ),
                (
                    r"from neo4j import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python Neo4j driver for graph database access",
                        "This code uses Python Neo4j to query a database",
                    ),
                ),
                (
                    r"import neo4j",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python Neo4j library import for graph database access",
                        "This code uses Python Neo4j to query a database",
                    ),
                ),
                (
                    r"from py2neo import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python py2neo library for Neo4j graph database access",
                        "This code uses Python py2neo to query a database",
                    ),
                ),
                (
                    r"from neomodel import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python neomodel ORM for Neo4j graph database access",
                        "This code uses Python neomodel to interact with a database",
                    ),
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r"open\(",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python built-in open() for filesystem file access",
                        "This code uses Python to interact with a file",
                    ),
                ),
                (
                    r"pathlib\.Path",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python pathlib.Path for filesystem path manipulation",
                        "This code uses Python pathlib to interact with a filesystem",
                    ),
                ),
                (
                    r"import shutil",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python shutil library import for high-level filesystem operations",
                        "This code uses Python shutil to interact with a filesystem",
                    ),
                ),
                (
                    r"import boto3",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python boto3 AWS SDK import for cloud storage access",
                        "This code uses Python boto3 to write to cloud storage",
                    ),
                ),
                (
                    r"from google\.cloud import storage",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python Google Cloud Storage client for cloud file access",
                        "This code uses Python Google Cloud to write to cloud storage",
                    ),
                ),
                (
                    r"from azure\.storage\.blob",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python Azure Blob Storage client for cloud file access",
                        "This code uses Python Azure to write to cloud storage",
                    ),
                ),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (
                    r"import grpc",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python grpc library import for gRPC service communication",
                        "This code uses Python grpc to integrate with a gRPC service",
                    ),
                ),
                (
                    r"grpc\.server",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Python grpc.server instantiation exposing a gRPC service endpoint",
                        "This code uses Python grpc to expose an inbound gRPC service",
                    ),
                ),
                (
                    r"add_.*Servicer_to_server",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Python gRPC servicer registration for handling inbound gRPC calls",
                        "This code uses Python grpc to handle incoming requests from a gRPC service",
                    ),
                ),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (
                    r"import graphene",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python Graphene library import for GraphQL schema definition",
                        "This code uses Python Graphene to interact with a GraphQL schema",
                    ),
                ),
                (
                    r"from ariadne import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python Ariadne library for schema-first GraphQL integration",
                        "This code uses Python Ariadne to interact with a GraphQL schema",
                    ),
                ),
                (
                    r"from strawberry import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python Strawberry library for code-first GraphQL schema definition",
                        "This code uses Python Strawberry to interact with a GraphQL schema",
                    ),
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r"import smtplib",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python smtplib library import for sending email via SMTP",
                        "This code uses Python smtplib to send an email message",
                    ),
                ),
                (
                    r"from email\.mime",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python email.mime module for constructing outbound email messages",
                        "This code uses Python email to send an email message",
                    ),
                ),
                (
                    r"import sendgrid",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python SendGrid library import for transactional email delivery",
                        "This code uses Python SendGrid to send an email message",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"import redis",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python redis library import for Redis cache access",
                        "This code uses Python redis to connect to a Redis cache",
                    ),
                ),
                (
                    r"from pymemcache",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python pymemcache library for Memcached cache access",
                        "This code uses Python pymemcache to connect to a cache store",
                    ),
                ),
                (
                    r"from cachetools",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python cachetools library for in-process caching utilities",
                        "This code uses Python cachetools to interact with a cache store",
                    ),
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"sse_starlette",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Python sse-starlette library for server-sent event streaming",
                        "This code uses Python sse-starlette to expose an inbound server-sent event stream",
                    ),
                ),
                (
                    r"asyncio\.Queue",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python asyncio.Queue for async producer-consumer data streaming",
                        "This code uses Python asyncio to interact with a server-sent event stream",
                    ),
                ),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (
                    r"import ftplib",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python ftplib library import for FTP server access",
                        "This code uses Python ftplib to connect to a FTP/SFTP server",
                    ),
                ),
                (
                    r"import paramiko",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python Paramiko library import for SSH and SFTP server access",
                        "This code uses Python Paramiko to connect to a FTP/SFTP server",
                    ),
                ),
                (
                    r"import pysftp",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Python pysftp library import for SFTP server access",
                        "This code uses Python pysftp to connect to a FTP/SFTP server",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"import schedule",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python schedule library import for job scheduling",
                        "This code uses Python schedule to interact with a scheduled task",
                    ),
                ),
                (
                    r"from apscheduler",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python APScheduler library for advanced task scheduling",
                        "This code uses Python APScheduler to interact with a scheduled task",
                    ),
                ),
                (
                    r"from celery\.schedules",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Python Celery schedules module for periodic task scheduling",
                        "This code uses Python Celery to interact with a scheduled task",
                    ),
                ),
            ],
        },
    },
)
