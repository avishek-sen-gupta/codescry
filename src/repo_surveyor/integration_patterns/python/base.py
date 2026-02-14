"""Python base integration patterns."""

from ..types import BasePatternSpec, Confidence, IntegrationType, PatternKey

BASE = BasePatternSpec(
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import requests", Confidence.MEDIUM),
                (r"import httpx", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"from zeep import", Confidence.HIGH),
                (r"import zeep", Confidence.HIGH),
                (r"from suds", Confidence.HIGH),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"@\w+\.task", Confidence.HIGH),
                (r"@shared_task", Confidence.HIGH),
                (r"from celery import", Confidence.HIGH),
                (r"from kombu import", Confidence.HIGH),
                (r"import pika", Confidence.HIGH),
                (r"from kafka import", Confidence.HIGH),
                (r"from aiokafka import", Confidence.HIGH),
                (r"KafkaConsumer", Confidence.HIGH),
                (r"KafkaProducer", Confidence.HIGH),
                (r"google\.cloud.*pubsub", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"import websockets", Confidence.HIGH),
                (r"from websockets import", Confidence.HIGH),
                (r"import socketio", Confidence.HIGH),
                (r"from socketio import", Confidence.HIGH),
                (r"@socketio\.on", Confidence.HIGH),
                (r"@sio\.on", Confidence.HIGH),
                (r"import socket\b", Confidence.MEDIUM),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"from sqlalchemy import", Confidence.HIGH),
                (r"import sqlalchemy", Confidence.HIGH),
                (r"from peewee import", Confidence.HIGH),
                (r"from tortoise", Confidence.HIGH),
                (r"import psycopg", Confidence.HIGH),
                (r"import pymysql", Confidence.HIGH),
                (r"import asyncpg", Confidence.HIGH),
                (r"@declarative", Confidence.HIGH),
                (r"Column\(", Confidence.MEDIUM),
                (r"relationship\(", Confidence.MEDIUM),
                (r"from google\.cloud import firestore", Confidence.HIGH),
                (r"from cassandra\.cluster", Confidence.HIGH),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"open\(", Confidence.MEDIUM),
                (r"pathlib\.Path", Confidence.MEDIUM),
                (r"import shutil", Confidence.HIGH),
                (r"import boto3", Confidence.HIGH),
                (r"from google\.cloud import storage", Confidence.HIGH),
                (r"from azure\.storage\.blob", Confidence.HIGH),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"import grpc", Confidence.HIGH),
                (r"grpc\.server", Confidence.HIGH),
                (r"add_.*Servicer_to_server", Confidence.HIGH),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"import graphene", Confidence.HIGH),
                (r"from ariadne import", Confidence.HIGH),
                (r"from strawberry import", Confidence.HIGH),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"import smtplib", Confidence.HIGH),
                (r"from email\.mime", Confidence.HIGH),
                (r"import sendgrid", Confidence.HIGH),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"import redis", Confidence.HIGH),
                (r"from pymemcache", Confidence.HIGH),
                (r"from cachetools", Confidence.HIGH),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"sse_starlette", Confidence.HIGH),
                (r"asyncio\.Queue", Confidence.MEDIUM),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (r"import ftplib", Confidence.HIGH),
                (r"import paramiko", Confidence.HIGH),
                (r"import pysftp", Confidence.HIGH),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"import schedule", Confidence.HIGH),
                (r"from apscheduler", Confidence.HIGH),
                (r"from celery\.schedules", Confidence.HIGH),
            ],
        },
    },
)
