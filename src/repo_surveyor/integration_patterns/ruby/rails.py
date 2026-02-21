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
                        "Rails ApplicationController subclass handling inbound HTTP requests",
                        "This code uses Rails to handle inbound HTTP requests via a controller",
                    ),
                ),
                (
                    r"< ActionController::Base",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Rails ActionController::Base subclass handling inbound HTTP requests",
                        "This code uses Rails ActionController to handle inbound HTTP requests",
                    ),
                ),
                (
                    r"< ActionController::API",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Rails ActionController::API subclass exposing an inbound API endpoint",
                        "This code uses Rails ActionController::API to handle inbound API requests",
                    ),
                ),
                (
                    r"\brender\b\s+json:",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Rails render json: sending a JSON response to inbound requests",
                        "This code uses Rails to expose inbound HTTP endpoints returning JSON responses",
                    ),
                ),
                (
                    r"\brespond_to\b\s+do",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Rails respond_to block handling inbound multi-format HTTP responses",
                        "This code uses Rails to handle inbound HTTP requests with format-based responses",
                    ),
                ),
                (
                    r"\bbefore_action\b",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Rails before_action callback executing before inbound request handling",
                        "This code uses Rails to intercept inbound HTTP requests with pre-action callbacks",
                    ),
                ),
                (
                    r"\bafter_action\b",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Rails after_action callback executing after inbound request handling",
                        "This code uses Rails to process inbound HTTP requests with post-action callbacks",
                    ),
                ),
                (
                    r"\bresources\b\s+:",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Rails resources route defining RESTful inbound resource endpoints",
                        "This code uses Rails routing to expose inbound RESTful resource endpoints",
                    ),
                ),
                (
                    r"\bget\b\s+['\"]",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Rails get route defining an inbound HTTP GET endpoint",
                        "This code uses Rails routing to handle inbound HTTP GET requests",
                    ),
                ),
                (
                    r"\bpost\b\s+['\"]",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Rails post route defining an inbound HTTP POST endpoint",
                        "This code uses Rails routing to handle inbound HTTP POST requests",
                    ),
                ),
                (
                    r"\bput\b\s+['\"]",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Rails put route defining an inbound HTTP PUT endpoint",
                        "This code uses Rails routing to handle inbound HTTP PUT requests",
                    ),
                ),
                (
                    r"\bdelete\b\s+['\"]",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Rails delete route defining an inbound HTTP DELETE endpoint",
                        "This code uses Rails routing to handle inbound HTTP DELETE requests",
                    ),
                ),
                (
                    r"\bpatch\b\s+['\"]",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Rails patch route defining an inbound HTTP PATCH endpoint",
                        "This code uses Rails routing to handle inbound HTTP PATCH requests",
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
                        "Rails ActiveRecord::Schema.define declaring the outbound database schema",
                        "This code uses Rails ActiveRecord to write an outbound database schema definition",
                    ),
                ),
                (
                    r"\.transaction\s+do",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord transaction wrapping outbound database operations",
                        "This code uses Rails ActiveRecord to execute outbound database operations in a transaction",
                    ),
                ),
                (
                    r"\.pluck\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord pluck querying specific columns from the database",
                        "This code uses Rails ActiveRecord to query specific column values from an outbound database",
                    ),
                ),
                (
                    r"\.select\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord select specifying columns in a database query",
                        "This code uses Rails ActiveRecord to query selected columns from an outbound database",
                    ),
                ),
                (
                    r"\.group\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord group aggregating database query results",
                        "This code uses Rails ActiveRecord to query grouped results from an outbound database",
                    ),
                ),
                (
                    r"\.order\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord order sorting database query results",
                        "This code uses Rails ActiveRecord to query ordered results from an outbound database",
                    ),
                ),
                (
                    r"\.limit\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord limit capping the number of database query results",
                        "This code uses Rails ActiveRecord to query limited results from an outbound database",
                    ),
                ),
                (
                    r"\.offset\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord offset paginating database query results",
                        "This code uses Rails ActiveRecord to query paginated results from an outbound database",
                    ),
                ),
                (
                    r"\.having\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord having filtering aggregated database query results",
                        "This code uses Rails ActiveRecord to query filtered aggregates from an outbound database",
                    ),
                ),
                (
                    r"\.count\b",
                    Confidence.LOW,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord count querying the number of matching database records",
                        "This code uses Rails ActiveRecord to query a record count from an outbound database",
                    ),
                ),
                (
                    r"\.exists\?",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord exists? checking for record presence in the database",
                        "This code uses Rails ActiveRecord to query record existence in an outbound database",
                    ),
                ),
                (
                    r"\.find_or_create_by",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord find_or_create_by upsert-style database operation",
                        "This code uses Rails ActiveRecord to write or query a record in an outbound database",
                    ),
                ),
                (
                    r"\.find_or_initialize_by",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord find_or_initialize_by lazy database record initialization",
                        "This code uses Rails ActiveRecord to query or initialize a record against an outbound database",
                    ),
                ),
                (
                    r"\.update_all\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord update_all bulk-updating database records",
                        "This code uses Rails ActiveRecord to write bulk updates to an outbound database",
                    ),
                ),
                (
                    r"\.delete_all\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord delete_all bulk-deleting database records",
                        "This code uses Rails ActiveRecord to delete records from an outbound database",
                    ),
                ),
                (
                    r"\.destroy_all\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord destroy_all bulk-destroying database records with callbacks",
                        "This code uses Rails ActiveRecord to destroy records in an outbound database",
                    ),
                ),
                (
                    r"\baccepts_nested_attributes_for\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord accepts_nested_attributes_for enabling nested model writes",
                        "This code uses Rails ActiveRecord to write nested model attributes to an outbound database",
                    ),
                ),
                (
                    r"\bdependent:\s+:destroy\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord dependent: :destroy cascading deletes to associated records",
                        "This code uses Rails ActiveRecord to cascade deletion to associated outbound database records",
                    ),
                ),
                (
                    r"\bforeign_key:",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord foreign_key declaring a database foreign key constraint",
                        "This code uses Rails ActiveRecord to define a foreign key in an outbound database",
                    ),
                ),
                (
                    r"\bclass_name:",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord class_name specifying an association model class",
                        "This code uses Rails ActiveRecord to associate models in an outbound database",
                    ),
                ),
                (
                    r"\bpolymorphic:\s+true\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord polymorphic: true declaring a polymorphic association",
                        "This code uses Rails ActiveRecord to define polymorphic associations in an outbound database",
                    ),
                ),
                (
                    r"\bthrough:",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord through: declaring a join-table association",
                        "This code uses Rails ActiveRecord to query through-associations in an outbound database",
                    ),
                ),
                (
                    r"\.arel_table",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveRecord arel_table building low-level SQL queries",
                        "This code uses Rails Arel to construct outbound database queries programmatically",
                    ),
                ),
                (
                    r"Arel\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rails Arel SQL AST builder for low-level database queries",
                        "This code uses Rails Arel to build outbound database queries",
                    ),
                ),
                (
                    r"ActiveGraph::Node",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveGraph::Node declaring a graph database node model",
                        "This code uses Rails ActiveGraph to interact with an outbound graph database node",
                    ),
                ),
                (
                    r"ActiveGraph::Relationship",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActiveGraph::Relationship declaring a graph database relationship model",
                        "This code uses Rails ActiveGraph to interact with outbound graph database relationships",
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
                        "Rails ActionCable framework handling inbound WebSocket connections",
                        "This code uses Rails ActionCable to handle inbound WebSocket connections",
                    ),
                ),
                (
                    r"< ApplicationCable::Channel",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Rails ApplicationCable::Channel subclass handling inbound WebSocket channel subscriptions",
                        "This code uses Rails ActionCable to handle inbound WebSocket channel subscriptions",
                    ),
                ),
                (
                    r"< ApplicationCable::Connection",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Rails ApplicationCable::Connection subclass managing inbound WebSocket connections",
                        "This code uses Rails ActionCable to accept inbound WebSocket connections",
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
                        "Rails ApplicationMailer subclass composing outbound email messages",
                        "This code uses Rails ActionMailer to send outbound email messages",
                    ),
                ),
                (
                    r"< ActionMailer::Base",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActionMailer::Base subclass composing outbound email messages",
                        "This code uses Rails ActionMailer to send outbound email messages",
                    ),
                ),
                (
                    r"\.deliver_now",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActionMailer deliver_now sending an outbound email synchronously",
                        "This code uses Rails ActionMailer to send an outbound email immediately",
                    ),
                ),
                (
                    r"\.deliver_later",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rails ActionMailer deliver_later enqueuing an outbound email for async delivery",
                        "This code uses Rails ActionMailer to send an outbound email asynchronously",
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
                        "Rails Rails.cache accessor interacting with the configured cache store",
                        "This code uses Rails to write to or read from an outbound cache store",
                    ),
                ),
                (
                    r"\bcaches_action\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rails caches_action enabling action-level response caching",
                        "This code uses Rails to interact with action-level response caching",
                    ),
                ),
                (
                    r"\bfragment_cache\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rails fragment_cache enabling view fragment caching",
                        "This code uses Rails to interact with view fragment caching",
                    ),
                ),
                (
                    r"config\.cache_store",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rails config.cache_store configuring the application cache backend",
                        "This code uses Rails to configure the outbound cache store integration",
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
                        "Rails ApplicationJob subclass defining a background job",
                        "This code uses Rails ActiveJob to interact with a background job queue",
                    ),
                ),
                (
                    r"< ActiveJob::Base",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rails ActiveJob::Base subclass defining a background job",
                        "This code uses Rails ActiveJob to interact with a background job queue",
                    ),
                ),
                (
                    r"\.perform_later",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rails ActiveJob perform_later enqueuing a job for asynchronous execution",
                        "This code uses Rails ActiveJob to interact with an asynchronous job queue",
                    ),
                ),
                (
                    r"\.perform_now",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rails ActiveJob perform_now executing a job synchronously",
                        "This code uses Rails ActiveJob to interact with a job by executing it immediately",
                    ),
                ),
            ],
        },
    },
)
