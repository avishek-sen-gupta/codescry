"""Pipeline timing observer for measuring stage durations."""

import json
import time
from dataclasses import dataclass
from datetime import datetime, timezone
from typing import Protocol


def _format_local_time(epoch: float) -> str:
    """Format a Unix epoch timestamp as a local ISO 8601 string."""
    return datetime.fromtimestamp(epoch, tz=timezone.utc).astimezone().isoformat()


class Clock(Protocol):
    """Protocol for time access, enabling deterministic testing."""

    def monotonic(self) -> float: ...
    def wall(self) -> float: ...


class SystemClock:
    """Production clock backed by the system time module."""

    def monotonic(self) -> float:
        return time.monotonic()

    def wall(self) -> float:
        return time.time()


@dataclass(frozen=True)
class StageTimingRecord:
    """Record of a completed pipeline stage's timing."""

    stage: str
    start_time: float
    end_time: float
    duration_seconds: float


class PipelineTimer(Protocol):
    """Protocol for pipeline timing observers."""

    def stage_started(self, stage: str) -> None: ...
    def stage_completed(self, stage: str) -> None: ...
    def to_json(self, indent: int | None = 2) -> str: ...


class PipelineTimingObserver:
    """Concrete observer that records wall-clock durations for pipeline stages."""

    def __init__(self, clock: Clock = SystemClock()) -> None:
        self._clock = clock
        self._pending: dict[str, tuple[float, float]] = {}
        self._completed: list[StageTimingRecord] = []

    @property
    def completed(self) -> list[StageTimingRecord]:
        """Return the list of completed stage timing records."""
        return list(self._completed)

    def stage_started(self, stage: str) -> None:
        """Record the start time for a stage."""
        self._pending[stage] = (self._clock.monotonic(), self._clock.wall())

    def stage_completed(self, stage: str) -> None:
        """Record the completion of a stage and compute its duration."""
        mono_start, wall_start = self._pending.pop(stage)
        wall_end = self._clock.wall()
        duration = self._clock.monotonic() - mono_start
        self._completed.append(StageTimingRecord(
            stage=stage,
            start_time=wall_start,
            end_time=wall_end,
            duration_seconds=duration,
        ))

    def to_json(self, indent: int | None = 2) -> str:
        """Serialise timing results as JSON."""
        return json.dumps(
            {
                "stages": [
                    {
                        "stage": record.stage,
                        "start_time": _format_local_time(record.start_time),
                        "end_time": _format_local_time(record.end_time),
                        "duration_seconds": record.duration_seconds,
                    }
                    for record in self._completed
                ],
                "total_seconds": sum(
                    record.duration_seconds
                    for record in self._completed
                    if "." not in record.stage
                ),
            },
            indent=indent,
        )


class NullPipelineTimer:
    """No-op implementation of PipelineTimer for callers that don't need timing."""

    def stage_started(self, stage: str) -> None:
        pass

    def stage_completed(self, stage: str) -> None:
        pass

    def to_json(self, indent: int | None = 2) -> str:
        return json.dumps({"stages": [], "total_seconds": 0.0}, indent=indent)
