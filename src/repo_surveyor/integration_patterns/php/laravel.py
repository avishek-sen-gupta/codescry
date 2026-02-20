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
                (r"Route::get\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"Route::post\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"Route::put\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"Route::delete\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"Route::patch\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"Route::resource\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"Route::apiResource\(", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"Eloquent", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"extends\s+Model\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"DB::table\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"DB::select\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Schema::create\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"->hasMany\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"->belongsTo\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"->belongsToMany\(", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"Queue::push\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"dispatch\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"implements\s+ShouldQueue", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Bus::dispatch\(", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"Mail::send\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Mail::to\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"extends\s+Mailable\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Notification::send\(", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"Cache::get\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Cache::put\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Cache::remember\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Cache::forget\(", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"\$schedule->command\(", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"\$schedule->call\(", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"\$schedule->job\(", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"Storage::put\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Storage::get\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"Storage::delete\(", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"broadcast\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"Broadcast::channel\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"Echo\.", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
