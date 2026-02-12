"""Survey report model and formatting."""

from pydantic import BaseModel


class DirectoryMarker(BaseModel):
    """Technologies detected in a specific directory."""

    directory: str
    marker_file: str
    languages: list[str] = []
    package_managers: list[str] = []
    frameworks: list[str] = []
    infrastructure: list[str] = []


class SurveyReport(BaseModel):
    """Report containing detected technologies in a repository."""

    repo_path: str
    languages: list[str] = []
    package_managers: list[str] = []
    frameworks: list[str] = []
    infrastructure: list[str] = []
    directory_markers: list[DirectoryMarker] = []

    def to_json(self, indent: int | None = 2) -> str:
        """Generate a JSON representation of the report."""
        return self.model_dump_json(indent=indent)

    def to_text(self) -> str:
        """Generate a human-readable plain text report."""
        lines = [f"Repository Survey: {self.repo_path}", ""]

        sections = [
            ("Languages", self.languages),
            ("Package Managers", self.package_managers),
            ("Frameworks", self.frameworks),
            ("Infrastructure", self.infrastructure),
        ]

        for title, items in sections:
            if items:
                lines.append(f"{title}:")
                for item in sorted(items):
                    lines.append(f"  - {item}")
                lines.append("")

        if self.directory_markers:
            lines.append("Directory Markers:")
            for marker in sorted(self.directory_markers, key=lambda m: m.directory):
                lines.append(f"  {marker.directory}/")
                lines.append(f"    Marker: {marker.marker_file}")
                if marker.languages:
                    lines.append(
                        f"    Languages: {', '.join(sorted(marker.languages))}"
                    )
                if marker.package_managers:
                    lines.append(
                        f"    Package Managers: {', '.join(sorted(marker.package_managers))}"
                    )
                if marker.frameworks:
                    lines.append(
                        f"    Frameworks: {', '.join(sorted(marker.frameworks))}"
                    )
                if marker.infrastructure:
                    lines.append(
                        f"    Infrastructure: {', '.join(sorted(marker.infrastructure))}"
                    )
            lines.append("")

        return "\n".join(lines).rstrip() + "\n"
