"""Python integration patterns."""

from .types import Confidence, IntegrationType

BASE_PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"import requests", Confidence.MEDIUM),
            (r"import httpx", Confidence.MEDIUM),
        ],
    },
    IntegrationType.SOAP: {
        "patterns": [
            (r"from zeep import", Confidence.HIGH),
            (r"import zeep", Confidence.HIGH),
            (r"from suds", Confidence.HIGH),
        ],
    },
    IntegrationType.MESSAGING: {
        "patterns": [
            (r"@\w+\.task", Confidence.HIGH),
            (r"@shared_task", Confidence.HIGH),
            (r"from celery import", Confidence.HIGH),
            (r"from kombu import", Confidence.HIGH),
            (r"import pika", Confidence.HIGH),
            (r"from kafka import", Confidence.HIGH),
            (r"from aiokafka import", Confidence.HIGH),
            (r"KafkaConsumer", Confidence.HIGH),
            (r"KafkaProducer", Confidence.HIGH),
        ],
    },
    IntegrationType.SOCKET: {
        "patterns": [
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
        "patterns": [
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
        ],
    },
}

FRAMEWORK_PATTERNS = {
    "Flask": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"@\w+\.route", Confidence.HIGH),
                (r"from flask import", Confidence.HIGH),
            ],
        },
    },
    "FastAPI": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"@\w+\.get", Confidence.HIGH),
                (r"@\w+\.post", Confidence.HIGH),
                (r"@\w+\.put", Confidence.HIGH),
                (r"@\w+\.delete", Confidence.HIGH),
                (r"from fastapi import", Confidence.HIGH),
            ],
        },
    },
    "Starlette": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"from starlette", Confidence.MEDIUM),
            ],
        },
    },
    "Django": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"@api_view", Confidence.HIGH),
                (r"@action", Confidence.MEDIUM),
                (r"from django\.http import", Confidence.HIGH),
                (r"from rest_framework", Confidence.HIGH),
                (r"APIView", Confidence.HIGH),
                (r"ViewSet", Confidence.HIGH),
                (r"GenericAPIView", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            "patterns": [
                (r"from django\.db import", Confidence.HIGH),
                (r"models\.Model", Confidence.HIGH),
                (r"ForeignKey\(", Confidence.MEDIUM),
            ],
        },
    },
    "aiohttp": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"from aiohttp import", Confidence.MEDIUM),
            ],
        },
    },
    "Tornado": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"import tornado", Confidence.HIGH),
                (r"from tornado\.web import", Confidence.HIGH),
                (r"from tornado\.httpclient import", Confidence.HIGH),
                (r"RequestHandler", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOCKET: {
            "patterns": [
                (r"from tornado\.websocket import", Confidence.HIGH),
                (r"WebSocketHandler", Confidence.HIGH),
            ],
        },
    },
    "Pyramid": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"from pyramid import", Confidence.HIGH),
                (r"from pyramid\.view import", Confidence.HIGH),
                (r"@view_config", Confidence.HIGH),
                (r"from pyramid\.config import", Confidence.HIGH),
            ],
        },
    },
}
