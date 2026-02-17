"""Constants for Scala package parsers."""

import re


class BuildSbtPatterns:
    """Regex patterns for build.sbt dependency extraction."""

    # Matches: "group" %% "artifact" % "version"
    #          "group" %  "artifact" % "version"
    SINGLE_DEP = re.compile(
        r'"[^"]+"\s+%%?\s+"([^"]+)"\s+%\s+"[^"]+"',
    )

    # Matches: libraryDependencies ++= Seq(...) blocks
    SEQ_BLOCK = re.compile(
        r"libraryDependencies\s*\+\+\s*=\s*Seq\s*\((.*?)\)",
        re.DOTALL,
    )
