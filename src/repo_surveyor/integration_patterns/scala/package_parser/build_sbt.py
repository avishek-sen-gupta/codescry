"""Parser for build.sbt files."""

from repo_surveyor.package_parsers.types import ParsedDependency

from .constants import BuildSbtPatterns

SOURCE = "build.sbt"


def parse(content: str) -> list[ParsedDependency]:
    """Parse dependencies from build.sbt content.

    Extracts artifact names from patterns like:
      libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0"
      libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-actor" % "2.8.0",
        "com.typesafe.akka" %% "akka-stream" % "2.8.0"
      )
    """
    names: list[str] = [
        m.group(1).strip().lower()
        for m in BuildSbtPatterns.SINGLE_DEP.finditer(content)
    ]

    return [ParsedDependency(name=n, source=SOURCE) for n in names]
