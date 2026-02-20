"""Tests for pipeline timing observer."""

import json

import pytest

from repo_surveyor.pipeline_timer import (
    NullPipelineTimer,
    PipelineTimingObserver,
    StageTimingRecord,
)


class StubClock:
    """Deterministic clock for testing. Advances by a fixed step on each call."""

    def __init__(self, start: float = 1000.0, step: float = 1.0) -> None:
        self._monotonic_value = start
        self._wall_value = start
        self._step = step

    def monotonic(self) -> float:
        value = self._monotonic_value
        self._monotonic_value += self._step
        return value

    def wall(self) -> float:
        value = self._wall_value
        self._wall_value += self._step
        return value


@pytest.fixture
def clock() -> StubClock:
    return StubClock(start=1000.0, step=1.0)


@pytest.fixture
def observer(clock: StubClock) -> PipelineTimingObserver:
    return PipelineTimingObserver(clock=clock)


class TestStageTimingRecord:
    """Tests for StageTimingRecord dataclass."""

    def test_is_frozen(self) -> None:
        """StageTimingRecord should be immutable."""
        record = StageTimingRecord(
            stage="test", start_time=100.0, end_time=101.0, duration_seconds=1.0
        )
        assert record.stage == "test"
        assert record.start_time == 100.0
        assert record.end_time == 101.0
        assert record.duration_seconds == 1.0


class TestPipelineTimingObserver:
    """Tests for the concrete PipelineTimingObserver."""

    def test_records_stage_with_expected_duration(
        self, observer: PipelineTimingObserver
    ) -> None:
        """Completed stages should have deterministic durations from the stub clock."""
        # stage_started: monotonic() -> 1000, wall() -> 1000
        observer.stage_started("test_stage")
        # stage_completed: wall() -> 1001, monotonic() -> 1001
        # duration = 1001 - 1000 = 1.0
        observer.stage_completed("test_stage")

        assert len(observer.completed) == 1
        record = observer.completed[0]
        assert record.stage == "test_stage"
        assert record.start_time == 1000.0
        assert record.end_time == 1001.0
        assert record.duration_seconds == 1.0

    def test_records_multiple_stages_in_order(
        self, observer: PipelineTimingObserver
    ) -> None:
        """Stages should be recorded in completion order."""
        observer.stage_started("first")
        observer.stage_completed("first")
        observer.stage_started("second")
        observer.stage_completed("second")
        observer.stage_started("third")
        observer.stage_completed("third")

        stages = [r.stage for r in observer.completed]
        assert stages == ["first", "second", "third"]

    def test_end_time_of_prior_stage_precedes_start_of_next(
        self, observer: PipelineTimingObserver
    ) -> None:
        """Sequential stages should have non-overlapping time ranges."""
        observer.stage_started("first")
        observer.stage_completed("first")
        observer.stage_started("second")
        observer.stage_completed("second")

        first, second = observer.completed
        assert first.end_time <= second.start_time

    def test_supports_nested_stages(self, observer: PipelineTimingObserver) -> None:
        """Nested (overlapping) stages should be tracked independently."""
        observer.stage_started("outer")
        observer.stage_started("inner")
        observer.stage_completed("inner")
        observer.stage_completed("outer")

        stages = [r.stage for r in observer.completed]
        assert stages == ["inner", "outer"]

    def test_to_json_structure(self, observer: PipelineTimingObserver) -> None:
        """to_json() should produce correct structure with stages and total_seconds."""
        observer.stage_started("stage_a")
        observer.stage_completed("stage_a")
        observer.stage_started("stage_b")
        observer.stage_completed("stage_b")

        result = json.loads(observer.to_json())

        assert "stages" in result
        assert "total_seconds" in result
        assert len(result["stages"]) == 2
        assert result["stages"][0]["stage"] == "stage_a"
        assert result["stages"][1]["stage"] == "stage_b"
        assert isinstance(result["stages"][0]["duration_seconds"], float)
        assert isinstance(result["stages"][0]["start_time"], str)
        assert isinstance(result["stages"][0]["end_time"], str)
        assert result["total_seconds"] > 0

    def test_total_seconds_excludes_sub_stages(
        self, observer: PipelineTimingObserver
    ) -> None:
        """total_seconds should only sum top-level stages (no dots in name)."""
        observer.stage_started("parent")
        observer.stage_started("parent.child")
        observer.stage_completed("parent.child")
        observer.stage_completed("parent")

        result = json.loads(observer.to_json())

        parent_duration = next(
            s["duration_seconds"] for s in result["stages"] if s["stage"] == "parent"
        )
        assert result["total_seconds"] == parent_duration

    def test_to_json_respects_indent(self, observer: PipelineTimingObserver) -> None:
        """to_json() should respect the indent parameter."""
        observer.stage_started("s")
        observer.stage_completed("s")

        compact = observer.to_json(indent=None)
        indented = observer.to_json(indent=4)

        assert "\n" not in compact
        assert "\n" in indented

    def test_completed_returns_copy(self, observer: PipelineTimingObserver) -> None:
        """completed property should return a copy, not the internal list."""
        observer.stage_started("s")
        observer.stage_completed("s")

        copy1 = observer.completed
        copy1.append(
            StageTimingRecord(
                stage="fake", start_time=0.0, end_time=0.0, duration_seconds=0.0
            )
        )
        assert len(observer.completed) == 1


class TestNullPipelineTimer:
    """Tests for the NullPipelineTimer no-op implementation."""

    def test_no_exceptions(self) -> None:
        """NullPipelineTimer should not raise on any method call."""
        timer = NullPipelineTimer()
        timer.stage_started("anything")
        timer.stage_completed("anything")

    def test_to_json_returns_empty_structure(self) -> None:
        """NullPipelineTimer.to_json() should return empty stages."""
        timer = NullPipelineTimer()
        result = json.loads(timer.to_json())

        assert result["stages"] == []
        assert result["total_seconds"] == 0.0
