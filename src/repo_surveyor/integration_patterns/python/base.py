"""Python base integration patterns."""

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
                (r"import requests", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"import httpx", Confidence.MEDIUM, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"from zeep import", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import zeep", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"from suds", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"@\w+\.task", Confidence.HIGH, SignalDirection.INWARD),
                (r"@shared_task", Confidence.HIGH, SignalDirection.INWARD),
                (r"from celery import", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"from kombu import", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"import pika", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"from kafka import", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"from aiokafka import", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"KafkaConsumer", Confidence.HIGH, SignalDirection.INWARD),
                (r"KafkaProducer", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"google\.cloud.*pubsub", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"import websockets", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"from websockets import", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"import socketio", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"from socketio import", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"@socketio\.on", Confidence.HIGH, SignalDirection.INWARD),
                (r"@sio\.on", Confidence.HIGH, SignalDirection.INWARD),
                (r"import socket\b", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"from sqlalchemy import", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import sqlalchemy", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"from peewee import", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"from tortoise", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import psycopg", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import pymysql", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import asyncpg", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@declarative", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Column\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"relationship\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (
                    r"from google\.cloud import firestore",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"from cassandra\.cluster", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"from neo4j import", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import neo4j", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"from py2neo import", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"from neomodel import", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"open\(", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"pathlib\.Path", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"import shutil", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"import boto3", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"from google\.cloud import storage",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (
                    r"from azure\.storage\.blob",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"import grpc", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"grpc\.server", Confidence.HIGH, SignalDirection.INWARD),
                (r"add_.*Servicer_to_server", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"import graphene", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"from ariadne import", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"from strawberry import", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"import smtplib", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"from email\.mime", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import sendgrid", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"import redis", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"from pymemcache", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"from cachetools", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"sse_starlette", Confidence.HIGH, SignalDirection.INWARD),
                (r"asyncio\.Queue", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (r"import ftplib", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import paramiko", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import pysftp", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"import schedule", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"from apscheduler", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"from celery\.schedules", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
