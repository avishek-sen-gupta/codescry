"""Rails framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Rails",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"< ApplicationController",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP requests are handled by ApplicationController",
                        "HTTP request is handled by controller",
                    ),
                ),
                (
                    r"< ActionController::Base",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP requests are handled by controller base",
                        "HTTP requests are handled via Rails ActionController",
                    ),
                ),
                (
                    r"< ActionController::API",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "API endpoint is exposed via controller subclass",
                        "API requests are handled via Rails ActionController",
                    ),
                ),
                (
                    r"\brender\b\s+json:",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "JSON response is sent to requests",
                        "HTTP endpoint is exposed returning JSON responses",
                    ),
                ),
                (
                    r"\brespond_to\b\s+do",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Multi-format HTTP responses are handled by respond_to",
                        "HTTP request is handled with format-based responses",
                    ),
                ),
                (
                    r"\bbefore_action\b",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Request handling is executed before by callback",
                        "HTTP request is intercepted with pre-action callbacks",
                    ),
                ),
                (
                    r"\bafter_action\b",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Request handling is executed after by callback",
                        "HTTP request is processed with post-action callbacks",
                    ),
                ),
                (
                    r"\bresources\b\s+:",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "RESTful resource endpoints are defined by resources",
                        "RESTful endpoints are exposed via Rails routing",
                    ),
                ),
                (
                    r"\bget\b\s+['\"]",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "HTTP GET endpoint is defined by route",
                        "HTTP GET requests are handled via Rails routing",
                    ),
                ),
                (
                    r"\bpost\b\s+['\"]",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "HTTP POST endpoint is defined by route",
                        "POST route is registered for inbound HTTP requests",
                    ),
                ),
                (
                    r"\bput\b\s+['\"]",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "HTTP PUT endpoint is defined by route",
                        "PUT route is registered for inbound HTTP requests",
                    ),
                ),
                (
                    r"\bdelete\b\s+['\"]",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "HTTP DELETE endpoint is defined by route",
                        "HTTP DELETE requests are handled via Rails routing",
                    ),
                ),
                (
                    r"\bpatch\b\s+['\"]",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "HTTP PATCH endpoint is defined by route",
                        "PATCH route is registered for inbound HTTP requests",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"ActiveRecord::Schema\.define",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database schema is declared by Schema definition",
                        "Database schema is written via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\.transaction\s+do",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database operations are wrapped by transaction",
                        "Database operations are executed in transaction via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\.pluck\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database columns are queried by pluck selection",
                        "Column values are queried via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\.select\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Database query columns are specified by select",
                        "Selected columns are queried via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\.group\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Database query results are aggregated",
                        "Grouped results are queried via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\.order\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Database query results are sorted by order",
                        "Ordered results are queried via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\.limit\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Database query results are limited",
                        "Limited results are queried via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\.offset\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Database query results are paginated",
                        "Paginated results are queried via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\.having\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Aggregated query results are filtered",
                        "Filtered aggregates are queried via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\.count\b",
                    Confidence.LOW,
                    SignalDirection.OUTWARD,
                    (
                        "Database records are counted via query",
                        "Record count is queried via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\.exists\?",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Record presence is checked in database",
                        "Record existence is queried via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\.find_or_create_by",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database record is found or created",
                        "Record is written or queried via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\.find_or_initialize_by",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database record is found or initialized",
                        "Record is queried or initialized via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\.update_all\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database records are bulk-updated by update_all",
                        "Bulk updates are written via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\.delete_all\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database records are bulk-deleted",
                        "Records are deleted from database via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\.destroy_all\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database records are bulk-destroyed with callbacks",
                        "Records are destroyed via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\baccepts_nested_attributes_for\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Nested attributes are accepted for model writes",
                        "Nested attributes are written via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\bdependent:\s+:destroy\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Associated records are cascade-deleted",
                        "Records are cascade deleted via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\bforeign_key:",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database foreign key is declared",
                        "Foreign key is defined via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\bclass_name:",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Association model class is specified",
                        "Models are associated in database via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\bpolymorphic:\s+true\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Polymorphic association is declared with polymorphic flag",
                        "Polymorphic associations are defined via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\bthrough:",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Join-table association is declared through association",
                        "Through-associations are queried via Rails ActiveRecord",
                    ),
                ),
                (
                    r"\.arel_table",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL queries are built via arel table",
                        "Database queries are constructed via Rails Arel",
                    ),
                ),
                (
                    r"Arel\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database queries are built by Arel SQL AST",
                        "Database queries are built via Rails Arel",
                    ),
                ),
                (
                    r"ActiveGraph::Node",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Graph node model is declared",
                        "Graph database node is accessed via Rails ActiveGraph",
                    ),
                ),
                (
                    r"ActiveGraph::Relationship",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Graph relationship model is declared",
                        "Graph database relationships are accessed via Rails ActiveGraph",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"ActionCable",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket connections are handled by ActionCable",
                        "WebSocket connections are handled via Rails ActionCable",
                    ),
                ),
                (
                    r"< ApplicationCable::Channel",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket channel subscriptions are handled by ApplicationCable::Channel",
                        "WebSocket subscriptions are handled via Rails ActionCable",
                    ),
                ),
                (
                    r"< ApplicationCable::Connection",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket connections are managed by ApplicationCable::Connection",
                        "WebSocket connections are accepted via Rails ActionCable",
                    ),
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r"< ApplicationMailer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email messages are composed by ApplicationMailer",
                        "Email messages are sent via Rails ActionMailer",
                    ),
                ),
                (
                    r"< ActionMailer::Base",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email message is composed via mailer base",
                        "Email messages are sent via Rails ActionMailer",
                    ),
                ),
                (
                    r"\.deliver_now",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email is sent synchronously via mailer",
                        "Email is sent immediately via Rails ActionMailer",
                    ),
                ),
                (
                    r"\.deliver_later",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email is queued for asynchronous delivery",
                        "Email is sent asynchronously via Rails ActionMailer",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"Rails\.cache\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cache store is accessed by Rails.cache",
                        "Cache store is accessed for read-write operations",
                    ),
                ),
                (
                    r"\bcaches_action\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Action-level response caching is enabled",
                        "Response caching is applied at action level",
                    ),
                ),
                (
                    r"\bfragment_cache\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "View fragment caching is enabled",
                        "View fragment is cached",
                    ),
                ),
                (
                    r"config\.cache_store",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Application cache backend is configured",
                        "Cache store is configured for outbound integration",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"< ApplicationJob",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Background job is defined by ApplicationJob",
                        "Background job queue is accessed via Rails ActiveJob",
                    ),
                ),
                (
                    r"< ActiveJob::Base",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Background job is defined via base class",
                        "Background job queue is accessed via Rails ActiveJob",
                    ),
                ),
                (
                    r"\.perform_later",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Job is queued for asynchronous execution",
                        "Asynchronous job queue is accessed via Rails ActiveJob",
                    ),
                ),
                (
                    r"\.perform_now",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Job is executed synchronously",
                        "Job is executed immediately via Rails ActiveJob",
                    ),
                ),
            ],
        },
    },
)
