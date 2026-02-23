"""Laravel framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Laravel",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"Route::get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GET route is defined for inbound requests",
                        "HTTP GET request is handled by route",
                    ),
                ),
                (
                    r"Route::post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "POST route is defined for inbound requests",
                        "HTTP POST request is handled by route",
                    ),
                ),
                (
                    r"Route::put\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PUT route is defined for inbound requests",
                        "HTTP PUT request is handled by route",
                    ),
                ),
                (
                    r"Route::delete\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "DELETE route is defined for inbound requests",
                        "HTTP DELETE request is handled by route",
                    ),
                ),
                (
                    r"Route::patch\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PATCH route is defined for inbound requests",
                        "HTTP PATCH request is handled by route",
                    ),
                ),
                (
                    r"Route::resource\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "RESTful routes are defined for resources",
                        "RESTful resource endpoints are exposed for inbound requests",
                    ),
                ),
                (
                    r"Route::apiResource\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "API resource routes are defined",
                        "API resource endpoints are exposed for inbound requests",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"Eloquent",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database operations are handled by Eloquent",
                        "Database is queried via Eloquent ORM",
                    ),
                ),
                (
                    r"extends\s+Model\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database model is defined with Eloquent",
                        "Database model is accessed via Eloquent",
                    ),
                ),
                (
                    r"DB::table\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database table is targeted by query",
                        "Database table is queried outbound",
                    ),
                ),
                (
                    r"DB::select\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL SELECT query is executed",
                        "SQL SELECT query is executed outbound",
                    ),
                ),
                (
                    r"Schema::create\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database table is created with migration",
                        "Database table is written with schema definition",
                    ),
                ),
                (
                    r"->hasMany\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "One-to-many relationship is declared",
                        "Has-many relationships are queried outbound",
                    ),
                ),
                (
                    r"->belongsTo\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Inverse relationship is declared with belongsTo",
                        "Belongs-to relationships are queried outbound",
                    ),
                ),
                (
                    r"->belongsToMany\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Many-to-many relationship is declared",
                        "Many-to-many relationships are queried outbound",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"Queue::push\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Job is pushed to queue",
                        "Job is sent to outbound queue",
                    ),
                ),
                (
                    r"dispatch\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Job or event is dispatched",
                        "Job is sent to outbound queue",
                    ),
                ),
                (
                    r"implements\s+ShouldQueue",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Job is marked for queue dispatch",
                        "Job is sent to outbound queue",
                    ),
                ),
                (
                    r"Bus::dispatch\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Command is dispatched via Bus",
                        "Command is sent to outbound queue",
                    ),
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r"Mail::send\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email message is sent outbound",
                        "Email message is sent outbound",
                    ),
                ),
                (
                    r"Mail::to\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email recipient is addressed",
                        "Email is sent to recipient",
                    ),
                ),
                (
                    r"extends\s+Mailable\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email message is composed with Mailable",
                        "Email message is composed and sent",
                    ),
                ),
                (
                    r"Notification::send\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Notification is sent outbound",
                        "Notification is sent outbound",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"Cache::get\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cache value is read with get",
                        "Cache store is read for data",
                    ),
                ),
                (
                    r"Cache::put\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cache value is written with put",
                        "Cache store is written with data",
                    ),
                ),
                (
                    r"Cache::remember\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cache value is computed or retrieved",
                        "Cache store is accessed for read/write",
                    ),
                ),
                (
                    r"Cache::forget\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cache value is evicted with forget",
                        "Cache value is removed from store",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"\$schedule->command\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Artisan command is scheduled",
                        "Artisan command is scheduled for execution",
                    ),
                ),
                (
                    r"\$schedule->call\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Callable task is scheduled",
                        "Callable task is scheduled for execution",
                    ),
                ),
                (
                    r"\$schedule->job\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Queued job is scheduled",
                        "Queued job is scheduled for execution",
                    ),
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r"Storage::put\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "File is written to storage",
                        "File is written to outbound storage",
                    ),
                ),
                (
                    r"Storage::get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "File is read from storage",
                        "File is received from inbound storage",
                    ),
                ),
                (
                    r"Storage::delete\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "File is deleted from storage",
                        "File is deleted from outbound storage",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"broadcast\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Broadcast event is dispatched",
                        "Broadcast event is sent outbound",
                    ),
                ),
                (
                    r"Broadcast::channel\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Broadcast channel is defined for inbound events",
                        "Channel subscriptions are handled for inbound broadcasts",
                    ),
                ),
                (
                    r"Echo\.",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Broadcast channels are accessed by Echo client",
                        "Broadcast events are accessed outbound",
                    ),
                ),
            ],
        },
    },
)
