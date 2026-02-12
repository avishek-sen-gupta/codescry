"""Detection logic for various technologies."""

import os
from pathlib import Path

# Indicator files mapped to their technologies
INDICATOR_FILES: dict[str, dict[str, list[str]]] = {
    # Python ecosystem
    "pyproject.toml": {"languages": ["Python"], "package_managers": ["Poetry"]},
    "requirements.txt": {"languages": ["Python"], "package_managers": ["pip"]},
    "setup.py": {"languages": ["Python"], "package_managers": ["pip"]},
    "Pipfile": {"languages": ["Python"], "package_managers": ["Pipenv"]},
    # JavaScript/Node ecosystem
    "package.json": {"languages": ["JavaScript"], "package_managers": ["npm"]},
    "yarn.lock": {"package_managers": ["Yarn"]},
    "pnpm-lock.yaml": {"package_managers": ["pnpm"]},
    # TypeScript
    "tsconfig.json": {"languages": ["TypeScript"]},
    # Java ecosystem
    "pom.xml": {"languages": ["Java"], "package_managers": ["Maven"]},
    "build.gradle": {"languages": ["Java"], "package_managers": ["Gradle"]},
    "build.gradle.kts": {
        "languages": ["Java", "Kotlin"],
        "package_managers": ["Gradle"],
    },
    # Go
    "go.mod": {"languages": ["Go"]},
    # Rust
    "Cargo.toml": {"languages": ["Rust"], "package_managers": ["Cargo"]},
    # Ruby
    "Gemfile": {"languages": ["Ruby"], "package_managers": ["Bundler"]},
    # .NET (legacy NuGet format)
    "packages.config": {"languages": ["C#"], "package_managers": ["NuGet"]},
    # Docker
    "Dockerfile": {"infrastructure": ["Docker"]},
    "docker-compose.yml": {"infrastructure": ["Docker Compose"]},
    "docker-compose.yaml": {"infrastructure": ["Docker Compose"]},
}

# File extension to language mapping
EXTENSION_LANGUAGES: dict[str, str] = {
    ".py": "Python",
    ".js": "JavaScript",
    ".ts": "TypeScript",
    ".tsx": "TypeScript",
    ".jsx": "JavaScript",
    ".java": "Java",
    ".kt": "Kotlin",
    ".go": "Go",
    ".rs": "Rust",
    ".rb": "Ruby",
    ".cs": "C#",
    ".cpp": "C++",
    ".c": "C",
    ".h": "C",
    ".hpp": "C++",
    ".swift": "Swift",
    ".php": "PHP",
    ".scala": "Scala",
    ".clj": "Clojure",
    ".ex": "Elixir",
    ".exs": "Elixir",
    ".erl": "Erlang",
    ".hs": "Haskell",
    ".ml": "OCaml",
    ".fs": "F#",
    ".r": "R",
    ".R": "R",
    ".jl": "Julia",
    ".lua": "Lua",
    ".pl": "Perl",
    ".sh": "Shell",
    ".bash": "Shell",
    ".zsh": "Shell",
    ".cbl": "COBOL",
    ".cob": "COBOL",
    ".cpy": "COBOL",
}

# Glob patterns for infrastructure detection
GLOB_PATTERNS: dict[str, dict[str, list[str]]] = {
    "*.csproj": {"languages": ["C#"], "package_managers": ["NuGet"], "frameworks": [".NET"]},
    "*.sln": {"frameworks": [".NET"]},
    "*.tf": {"infrastructure": ["Terraform"]},
}

# Kubernetes markers to look for in YAML files
K8S_MARKERS = [
    "apiVersion:",
    "kind: Deployment",
    "kind: Service",
    "kind: Pod",
    "kind: ConfigMap",
    "kind: Secret",
    "kind: Ingress",
    "kind: StatefulSet",
    "kind: DaemonSet",
]

# Framework detection patterns in package files
FRAMEWORK_PATTERNS: dict[str, dict[str, str]] = {
    # Python frameworks (in pyproject.toml or requirements.txt)
    "fastapi": {"frameworks": "FastAPI"},
    "django": {"frameworks": "Django"},
    "flask": {"frameworks": "Flask"},
    "starlette": {"frameworks": "Starlette"},
    "tornado": {"frameworks": "Tornado"},
    "pyramid": {"frameworks": "Pyramid"},
    "aiohttp": {"frameworks": "aiohttp"},
    # JavaScript/Node frameworks (in package.json)
    "react": {"frameworks": "React"},
    "vue": {"frameworks": "Vue.js"},
    "angular": {"frameworks": "Angular"},
    "next": {"frameworks": "Next.js"},
    "nuxt": {"frameworks": "Nuxt.js"},
    "express": {"frameworks": "Express"},
    "nestjs": {"frameworks": "NestJS"},
    "svelte": {"frameworks": "Svelte"},
    "gatsby": {"frameworks": "Gatsby"},
    "fastify": {"frameworks": "Fastify"},
    # Java frameworks (in pom.xml or build.gradle)
    "spring-boot": {"frameworks": "Spring"},
    "spring-web": {"frameworks": "Spring"},
    "spring-webmvc": {"frameworks": "Spring"},
    "jersey": {"frameworks": "JAX-RS"},
    "resteasy": {"frameworks": "JAX-RS"},
    "micronaut": {"frameworks": "Micronaut"},
    "quarkus": {"frameworks": "Quarkus"},
    "javalin": {"frameworks": "Javalin"},
    "dropwizard": {"frameworks": "Dropwizard"},
    "vertx-web": {"frameworks": "Vert.x"},
    "vertx-core": {"frameworks": "Vert.x"},
    "play-java": {"frameworks": "Play"},
    "play-scala": {"frameworks": "Play"},
    "cxf-rt-frontend-jaxrs": {"frameworks": "Apache CXF"},
    "cxf-rt-frontend-jaxws": {"frameworks": "Apache CXF"},
    "axis2": {"frameworks": "Apache Axis2"},
    "spring-ws": {"frameworks": "Spring WS"},
    "jaxws-rt": {"frameworks": "JAX-WS"},
    "metro-jax-ws": {"frameworks": "JAX-WS"},
    # Go frameworks (in go.mod)
    "gin-gonic/gin": {"frameworks": "Gin"},
    "labstack/echo": {"frameworks": "Echo"},
    "gofiber/fiber": {"frameworks": "Fiber"},
    "go-chi/chi": {"frameworks": "Chi"},
    "gorilla/mux": {"frameworks": "Gorilla"},
    # Rust frameworks (in Cargo.toml)
    "actix-web": {"frameworks": "Actix"},
    "axum": {"frameworks": "Axum"},
    "rocket": {"frameworks": "Rocket"},
    "warp": {"frameworks": "Warp"},
    # .NET frameworks (in .csproj or packages.config)
    "microsoft.aspnetcore": {"frameworks": "ASP.NET Core"},
    "microsoft.aspnet.webapi": {"frameworks": "ASP.NET Web API"},
    "servicestack": {"frameworks": "ServiceStack"},
    "nancy": {"frameworks": "Nancy"},
    "carter": {"frameworks": "Carter"},
    "system.servicemodel": {"frameworks": "WCF"},
    "corewcf": {"frameworks": "CoreWCF"},
}


def detect_from_indicator_files(repo_path: Path) -> dict[str, set[str]]:
    """Detect technologies from indicator files in root only."""
    results: dict[str, set[str]] = {
        "languages": set(),
        "package_managers": set(),
        "frameworks": set(),
        "infrastructure": set(),
    }

    for filename, techs in INDICATOR_FILES.items():
        if (repo_path / filename).exists():
            for category, items in techs.items():
                results[category].update(items)

    return results


def detect_indicator_files_with_directories(
    repo_path: Path,
) -> list[dict]:
    """Detect indicator files and associate them with their directories.

    Returns a list of dicts with directory, marker_file, and detected technologies.
    """
    results: list[dict] = []

    # Skip directories that shouldn't be scanned
    skip_dirs = {
        ".git",
        "node_modules",
        "__pycache__",
        ".venv",
        "venv",
        "env",
        ".env",
        "dist",
        "build",
        "target",
        ".tox",
        ".pytest_cache",
        ".mypy_cache",
        "vendor",
    }

    for root, dirs, files in os.walk(repo_path):
        # Skip excluded directories
        dirs[:] = [d for d in dirs if d not in skip_dirs and not d.startswith(".")]

        root_path = Path(root)
        rel_dir = root_path.relative_to(repo_path)
        dir_str = "." if rel_dir == Path(".") else str(rel_dir)

        for filename in files:
            if filename in INDICATOR_FILES:
                techs = INDICATOR_FILES[filename]
                marker_result = {
                    "directory": dir_str,
                    "marker_file": filename,
                    "languages": list(techs.get("languages", [])),
                    "package_managers": list(techs.get("package_managers", [])),
                    "frameworks": list(techs.get("frameworks", [])),
                    "infrastructure": list(techs.get("infrastructure", [])),
                }
                results.append(marker_result)

    return results


def detect_from_glob_patterns(repo_path: Path) -> dict[str, set[str]]:
    """Detect technologies from glob patterns."""
    from .package_parsers import match_frameworks, parse_dependencies

    results: dict[str, set[str]] = {
        "languages": set(),
        "package_managers": set(),
        "frameworks": set(),
        "infrastructure": set(),
    }

    for pattern, techs in GLOB_PATTERNS.items():
        matched_files = list(repo_path.glob(pattern)) + list(
            repo_path.glob(f"**/{pattern}")
        )
        if matched_files:
            for category, items in techs.items():
                results[category].update(items)

        # Parse matched files for framework dependencies
        for filepath in matched_files:
            try:
                content = filepath.read_text(encoding="utf-8", errors="ignore")
                deps = parse_dependencies(filepath.name, content)
                results["frameworks"].update(
                    match_frameworks(deps, FRAMEWORK_PATTERNS)
                )
            except (OSError, PermissionError):
                continue

    return results


def detect_kubernetes(repo_path: Path) -> bool:
    """Check if repository contains Kubernetes manifests."""
    yaml_files = list(repo_path.glob("**/*.yaml")) + list(repo_path.glob("**/*.yml"))

    for yaml_file in yaml_files:
        try:
            content = yaml_file.read_text(encoding="utf-8", errors="ignore")
            for marker in K8S_MARKERS:
                if marker in content:
                    return True
        except (OSError, PermissionError):
            continue

    return False


def detect_languages_from_extensions(repo_path: Path) -> set[str]:
    """Detect languages by scanning file extensions."""
    languages: set[str] = set()

    # Walk the repository, skipping common non-source directories
    skip_dirs = {
        ".git",
        "node_modules",
        "__pycache__",
        ".venv",
        "venv",
        "env",
        ".env",
        "dist",
        "build",
        "target",
        ".tox",
        ".pytest_cache",
        ".mypy_cache",
        "vendor",
    }

    for root, dirs, files in os.walk(repo_path):
        # Skip excluded directories
        dirs[:] = [d for d in dirs if d not in skip_dirs and not d.startswith(".")]

        for filename in files:
            ext = Path(filename).suffix.lower()
            if ext in EXTENSION_LANGUAGES:
                languages.add(EXTENSION_LANGUAGES[ext])

    return languages


def detect_frameworks_from_packages(repo_path: Path) -> set[str]:
    """Detect frameworks from package files."""
    from .package_parsers import match_frameworks, parse_dependencies

    frameworks: set[str] = set()

    config_files = [
        "pyproject.toml",
        "requirements.txt",
        "setup.py",
        "Pipfile",
        "package.json",
        "pom.xml",
        "build.gradle",
        "build.gradle.kts",
        "go.mod",
        "Cargo.toml",
    ]

    for filename in config_files:
        filepath = repo_path / filename
        if filepath.exists():
            try:
                content = filepath.read_text(encoding="utf-8", errors="ignore")
                deps = parse_dependencies(filename, content)
                frameworks.update(match_frameworks(deps, FRAMEWORK_PATTERNS))
            except (OSError, PermissionError):
                continue

    return frameworks


def detect_frameworks_for_file(filepath: Path) -> list[str]:
    """Detect frameworks from a specific package file."""
    from .package_parsers import match_frameworks, parse_dependencies

    if not filepath.exists():
        return []

    try:
        content = filepath.read_text(encoding="utf-8", errors="ignore")
    except (OSError, PermissionError):
        return []

    deps = parse_dependencies(filepath.name, content)
    return match_frameworks(deps, FRAMEWORK_PATTERNS)
