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
                        "HTTP requests are made by requests library",
                        "HTTP request is made with requests",
                    ),
                ),
                (
                    r"import httpx",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Async HTTP requests are made by httpx",
                        "HTTP request is made with httpx client",
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
                        "SOAP services are accessed by Zeep client",
                        "SOAP service is called with Zeep client",
                    ),
                ),
                (
                    r"import zeep",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SOAP integration is imported via Zeep library",
                        "SOAP service is called with Zeep client",
                    ),
                ),
                (
                    r"from suds",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SOAP services are accessed by Suds client",
                        "SOAP service is called with Suds client",
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
                        "Task messages are received by Celery decorator",
                        "Messages are consumed from queue via Celery",
                    ),
                ),
                (
                    r"@shared_task",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Reusable tasks are decorated by shared_task decorator",
                        "Messages are consumed from queue via Celery",
                    ),
                ),
                (
                    r"from celery import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Task queue is imported via Celery library",
                        "Message queue is accessed via Celery worker",
                    ),
                ),
                (
                    r"from kombu import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Messaging is imported via Kombu AMQP library",
                        "Message queue is accessed via Kombu",
                    ),
                ),
                (
                    r"import pika",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "RabbitMQ messaging is imported via Pika library",
                        "Message queue is accessed via Pika",
                    ),
                ),
                (
                    r"from kafka import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kafka messaging is imported via kafka-python library",
                        "Kafka topic is accessed for messaging",
                    ),
                ),
                (
                    r"from aiokafka import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Async Kafka messaging is imported via aiokafka",
                        "Kafka topic is accessed via aiokafka",
                    ),
                ),
                (
                    r"KafkaConsumer",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Kafka messages are consumed by KafkaConsumer",
                        "Messages are consumed from Kafka topic",
                    ),
                ),
                (
                    r"KafkaProducer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kafka messages are produced by KafkaProducer",
                        "Messages are sent to Kafka topic",
                    ),
                ),
                (
                    r"google\.cloud.*pubsub",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Cloud messaging is handled by Pub/Sub library",
                        "Message queue is accessed via Google Cloud",
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
                        "WebSocket library is imported for bidirectional communication",
                        "WebSocket connection is opened with websockets",
                    ),
                ),
                (
                    r"from websockets import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "WebSocket library is imported for bidirectional communication",
                        "WebSocket connection is opened with websockets",
                    ),
                ),
                (
                    r"import socketio",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Real-time communication is imported via Socket.IO library",
                        "WebSocket connection is opened with socketio",
                    ),
                ),
                (
                    r"from socketio import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Real-time events are communicated via Socket.IO library",
                        "WebSocket connection is opened with socketio",
                    ),
                ),
                (
                    r"@socketio\.on",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Socket events are handled by event decorator",
                        "WebSocket events are listened via socketio",
                    ),
                ),
                (
                    r"@sio\.on",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Socket events are handled by instance decorator",
                        "WebSocket events are listened via socketio",
                    ),
                ),
                (
                    r"import socket\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Socket library is imported for network communication",
                        "Network socket is opened for communication",
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
                        "Database operations are handled by SQLAlchemy ORM",
                        "Database is accessed via SQLAlchemy ORM",
                    ),
                ),
                (
                    r"import sqlalchemy",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database operations are imported via SQLAlchemy library",
                        "Database is accessed via SQLAlchemy ORM",
                    ),
                ),
                (
                    r"from peewee import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database operations are handled by Peewee ORM",
                        "Database is accessed via Peewee ORM",
                    ),
                ),
                (
                    r"from tortoise",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Async database operations are handled by Tortoise ORM",
                        "Database is accessed via Tortoise ORM",
                    ),
                ),
                (
                    r"import psycopg",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PostgreSQL database is accessed by psycopg driver",
                        "Database is queried with psycopg driver",
                    ),
                ),
                (
                    r"import pymysql",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MySQL database is accessed by PyMySQL driver",
                        "Database is queried with PyMySQL driver",
                    ),
                ),
                (
                    r"import asyncpg",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Async PostgreSQL is accessed by asyncpg driver",
                        "Database is queried with asyncpg driver",
                    ),
                ),
                (
                    r"@declarative",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ORM models are decorated by declarative base",
                        "Database model is defined with SQLAlchemy",
                    ),
                ),
                (
                    r"Column\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Database schema is mapped by ORM Column",
                        "Database model is defined for ORM persistence",
                    ),
                ),
                (
                    r"relationship\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Database models are associated by ORM relationship",
                        "Database is accessed via ORM interface",
                    ),
                ),
                (
                    r"from google\.cloud import firestore",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "NoSQL database is accessed via Firestore client",
                        "Database is written via Google Cloud client",
                    ),
                ),
                (
                    r"from cassandra\.cluster",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Distributed database is accessed by Cassandra driver",
                        "Database is queried with Cassandra driver",
                    ),
                ),
                (
                    r"from neo4j import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Graph database is accessed by Neo4j driver",
                        "Database is queried with Neo4j driver",
                    ),
                ),
                (
                    r"import neo4j",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Graph database is imported via Neo4j library",
                        "Database is queried with Neo4j driver",
                    ),
                ),
                (
                    r"from py2neo import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Graph database is accessed by py2neo library",
                        "Database is queried with py2neo",
                    ),
                ),
                (
                    r"from neomodel import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Graph database is accessed by neomodel ORM",
                        "Database is accessed with neomodel",
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
                        "Filesystem files are accessed by built-in open",
                        "File is accessed for operations",
                    ),
                ),
                (
                    r"pathlib\.Path",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Filesystem paths are manipulated by pathlib Path",
                        "Filesystem is accessed via pathlib",
                    ),
                ),
                (
                    r"import shutil",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Filesystem operations are imported via shutil library",
                        "Filesystem is accessed via shutil",
                    ),
                ),
                (
                    r"import boto3",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cloud storage is imported via boto3 SDK",
                        "Cloud storage is written via boto3",
                    ),
                ),
                (
                    r"from google\.cloud import storage",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cloud storage is accessed via Google client",
                        "Cloud storage is written via Google Cloud",
                    ),
                ),
                (
                    r"from azure\.storage\.blob",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cloud blob storage is accessed via Azure client",
                        "Cloud storage is written via Azure client",
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
                        "gRPC communication is imported via grpc library",
                        "gRPC service is accessed for integration",
                    ),
                ),
                (
                    r"grpc\.server",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "gRPC service is exposed by server instantiation",
                        "gRPC service is exposed for inbound requests",
                    ),
                ),
                (
                    r"add_.*Servicer_to_server",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "gRPC calls are handled by servicer registration",
                        "gRPC requests are handled by service",
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
                        "GraphQL schema is imported via Graphene library",
                        "GraphQL schema is accessed with Graphene",
                    ),
                ),
                (
                    r"from ariadne import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "GraphQL schema is defined with Ariadne library",
                        "GraphQL schema is accessed for query operations",
                    ),
                ),
                (
                    r"from strawberry import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "GraphQL schema is defined by Strawberry library",
                        "GraphQL schema is accessed with Strawberry",
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
                        "Email is sent via smtplib SMTP library",
                        "Email is sent via smtplib",
                    ),
                ),
                (
                    r"from email\.mime",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email messages are constructed by mime module",
                        "Email is sent via Python email",
                    ),
                ),
                (
                    r"import sendgrid",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Transactional email is imported via SendGrid library",
                        "Email is sent via SendGrid API",
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
                        "Redis cache is imported via redis library",
                        "Redis cache is connected for storage",
                    ),
                ),
                (
                    r"from pymemcache",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Memcached cache is accessed by pymemcache library",
                        "Cache is connected via pymemcache",
                    ),
                ),
                (
                    r"from cachetools",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "In-process caching is handled by cachetools library",
                        "Cache is accessed with cachetools",
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
                        "SSE library is imported for server-sent event streaming",
                        "Event stream is exposed via sse-starlette",
                    ),
                ),
                (
                    r"asyncio\.Queue",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Async data is streamed by asyncio Queue",
                        "Event stream is consumed via asyncio",
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
                        "FTP server is imported via ftplib library",
                        "FTP connection is opened with ftplib",
                    ),
                ),
                (
                    r"import paramiko",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SSH server is accessed via Paramiko library",
                        "FTP connection is opened with Paramiko",
                    ),
                ),
                (
                    r"import pysftp",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SFTP server is imported via pysftp library",
                        "SFTP connection is opened with pysftp",
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
                        "Jobs are scheduled by schedule library",
                        "Scheduled task is registered with schedule",
                    ),
                ),
                (
                    r"from apscheduler",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Tasks are scheduled by APScheduler library",
                        "Task is scheduled with timer",
                    ),
                ),
                (
                    r"from celery\.schedules",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Periodic tasks are scheduled by Celery schedules",
                        "Scheduled task is registered with Celery",
                    ),
                ),
            ],
        },
    },
)
