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
                        "Laravel Route::get defining an inbound HTTP GET route",
                        "This code uses Laravel routing to handle incoming HTTP GET requests",
                    ),
                ),
                (
                    r"Route::post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Laravel Route::post defining an inbound HTTP POST route",
                        "This code uses Laravel routing to handle incoming HTTP POST requests",
                    ),
                ),
                (
                    r"Route::put\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Laravel Route::put defining an inbound HTTP PUT route",
                        "This code uses Laravel routing to handle incoming HTTP PUT requests",
                    ),
                ),
                (
                    r"Route::delete\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Laravel Route::delete defining an inbound HTTP DELETE route",
                        "This code uses Laravel routing to handle incoming HTTP DELETE requests",
                    ),
                ),
                (
                    r"Route::patch\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Laravel Route::patch defining an inbound HTTP PATCH route",
                        "This code uses Laravel routing to handle incoming HTTP PATCH requests",
                    ),
                ),
                (
                    r"Route::resource\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Laravel Route::resource defining inbound RESTful resource routes",
                        "This code uses Laravel routing to expose inbound RESTful resource endpoints",
                    ),
                ),
                (
                    r"Route::apiResource\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Laravel Route::apiResource defining inbound API resource routes",
                        "This code uses Laravel routing to expose inbound API resource endpoints",
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
                        "Laravel Eloquent ORM for database model operations",
                        "This code uses Laravel Eloquent to query an outbound relational database",
                    ),
                ),
                (
                    r"extends\s+Model\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Laravel Eloquent Model subclass defining a database-backed model",
                        "This code uses Laravel Eloquent to interact with an outbound database model",
                    ),
                ),
                (
                    r"DB::table\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Laravel DB::table query builder targeting a database table",
                        "This code uses Laravel DB to query an outbound database table",
                    ),
                ),
                (
                    r"DB::select\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Laravel DB::select executing a raw SQL SELECT query",
                        "This code uses Laravel DB to execute an outbound raw SQL SELECT query",
                    ),
                ),
                (
                    r"Schema::create\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Laravel Schema::create defining a new database table migration",
                        "This code uses Laravel Schema to write a new table to an outbound database",
                    ),
                ),
                (
                    r"->hasMany\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Laravel Eloquent hasMany declaring a one-to-many relationship",
                        "This code uses Laravel Eloquent to query outbound has-many database relationships",
                    ),
                ),
                (
                    r"->belongsTo\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Laravel Eloquent belongsTo declaring an inverse relationship",
                        "This code uses Laravel Eloquent to query outbound belongs-to database relationships",
                    ),
                ),
                (
                    r"->belongsToMany\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Laravel Eloquent belongsToMany declaring a many-to-many relationship",
                        "This code uses Laravel Eloquent to query outbound many-to-many database relationships",
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
                        "Laravel Queue::push dispatching a job to the outbound queue",
                        "This code uses Laravel Queue to send a job to an outbound message queue",
                    ),
                ),
                (
                    r"dispatch\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Laravel dispatch() helper dispatching a job or event",
                        "This code uses Laravel to send a job or event to an outbound queue",
                    ),
                ),
                (
                    r"implements\s+ShouldQueue",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Laravel ShouldQueue interface marking a job for outbound queue dispatch",
                        "This code uses Laravel Queue to send a job to an outbound message queue",
                    ),
                ),
                (
                    r"Bus::dispatch\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Laravel Bus::dispatch dispatching a command via the command bus",
                        "This code uses Laravel Bus to send a command to an outbound queue or handler",
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
                        "Laravel Mail::send sending an outbound email message",
                        "This code uses Laravel Mail to send an outbound email message",
                    ),
                ),
                (
                    r"Mail::to\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Laravel Mail::to addressing an outbound email to a recipient",
                        "This code uses Laravel Mail to send an outbound email to a recipient",
                    ),
                ),
                (
                    r"extends\s+Mailable\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Laravel Mailable subclass composing an outbound email message",
                        "This code uses Laravel Mail to compose and send an outbound email message",
                    ),
                ),
                (
                    r"Notification::send\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Laravel Notification::send dispatching an outbound notification",
                        "This code uses Laravel Notifications to send an outbound notification",
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
                        "Laravel Cache::get reading a value from the outbound cache store",
                        "This code uses Laravel Cache to read from an outbound cache store",
                    ),
                ),
                (
                    r"Cache::put\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Laravel Cache::put writing a value to the outbound cache store",
                        "This code uses Laravel Cache to write to an outbound cache store",
                    ),
                ),
                (
                    r"Cache::remember\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Laravel Cache::remember reading or caching a computed value",
                        "This code uses Laravel Cache to read or write to an outbound cache store",
                    ),
                ),
                (
                    r"Cache::forget\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Laravel Cache::forget evicting a value from the outbound cache store",
                        "This code uses Laravel Cache to remove a value from an outbound cache store",
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
                        "Laravel scheduler $schedule->command scheduling an Artisan command",
                        "This code uses Laravel Scheduler to interact with a scheduled Artisan command",
                    ),
                ),
                (
                    r"\$schedule->call\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Laravel scheduler $schedule->call scheduling a callable task",
                        "This code uses Laravel Scheduler to interact with a scheduled callable task",
                    ),
                ),
                (
                    r"\$schedule->job\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Laravel scheduler $schedule->job scheduling a queued job",
                        "This code uses Laravel Scheduler to interact with a scheduled queued job",
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
                        "Laravel Storage::put writing a file to the configured storage disk",
                        "This code uses Laravel Storage to write a file to outbound storage",
                    ),
                ),
                (
                    r"Storage::get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Laravel Storage::get reading a file from the configured storage disk",
                        "This code uses Laravel Storage to receive a file from inbound storage",
                    ),
                ),
                (
                    r"Storage::delete\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Laravel Storage::delete removing a file from the configured storage disk",
                        "This code uses Laravel Storage to delete a file from outbound storage",
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
                        "Laravel broadcast() helper dispatching a broadcast event",
                        "This code uses Laravel Broadcasting to send an outbound broadcast event",
                    ),
                ),
                (
                    r"Broadcast::channel\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Laravel Broadcast::channel defining an inbound broadcast channel",
                        "This code uses Laravel Broadcasting to handle inbound channel subscriptions",
                    ),
                ),
                (
                    r"Echo\.",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Laravel Echo JavaScript client interacting with broadcast channels",
                        "This code uses Laravel Echo to interact with outbound broadcast events",
                    ),
                ),
            ],
        },
    },
)
