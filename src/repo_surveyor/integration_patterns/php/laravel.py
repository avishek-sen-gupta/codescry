"""Laravel framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Laravel",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"Route::get\(", Confidence.HIGH),
                (r"Route::post\(", Confidence.HIGH),
                (r"Route::put\(", Confidence.HIGH),
                (r"Route::delete\(", Confidence.HIGH),
                (r"Route::patch\(", Confidence.HIGH),
                (r"Route::resource\(", Confidence.HIGH),
                (r"Route::apiResource\(", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"Eloquent", Confidence.HIGH),
                (r"extends\s+Model\b", Confidence.HIGH),
                (r"DB::table\(", Confidence.HIGH),
                (r"DB::select\(", Confidence.HIGH),
                (r"Schema::create\(", Confidence.HIGH),
                (r"->hasMany\(", Confidence.HIGH),
                (r"->belongsTo\(", Confidence.HIGH),
                (r"->belongsToMany\(", Confidence.HIGH),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"Queue::push\(", Confidence.HIGH),
                (r"dispatch\(", Confidence.MEDIUM),
                (r"implements\s+ShouldQueue", Confidence.HIGH),
                (r"Bus::dispatch\(", Confidence.HIGH),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"Mail::send\(", Confidence.HIGH),
                (r"Mail::to\(", Confidence.HIGH),
                (r"extends\s+Mailable\b", Confidence.HIGH),
                (r"Notification::send\(", Confidence.HIGH),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"Cache::get\(", Confidence.HIGH),
                (r"Cache::put\(", Confidence.HIGH),
                (r"Cache::remember\(", Confidence.HIGH),
                (r"Cache::forget\(", Confidence.HIGH),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"\$schedule->command\(", Confidence.HIGH),
                (r"\$schedule->call\(", Confidence.HIGH),
                (r"\$schedule->job\(", Confidence.HIGH),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"Storage::put\(", Confidence.HIGH),
                (r"Storage::get\(", Confidence.HIGH),
                (r"Storage::delete\(", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"broadcast\(", Confidence.MEDIUM),
                (r"Broadcast::channel\(", Confidence.HIGH),
                (r"Echo\.", Confidence.MEDIUM),
            ],
        },
    },
)
