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
}

# Glob patterns for infrastructure detection
GLOB_PATTERNS: dict[str, dict[str, list[str]]] = {
    "*.csproj": {"languages": ["C#"], "frameworks": [".NET"]},
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
    results: dict[str, set[str]] = {
        "languages": set(),
        "package_managers": set(),
        "frameworks": set(),
        "infrastructure": set(),
    }

    for pattern, techs in GLOB_PATTERNS.items():
        if list(repo_path.glob(pattern)) or list(repo_path.glob(f"**/{pattern}")):
            for category, items in techs.items():
                results[category].update(items)

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
    frameworks: set[str] = set()

    # Check Python package files
    python_files = ["pyproject.toml", "requirements.txt", "setup.py", "Pipfile"]
    for filename in python_files:
        filepath = repo_path / filename
        if filepath.exists():
            try:
                content = filepath.read_text(encoding="utf-8", errors="ignore").lower()
                for pattern, tech in FRAMEWORK_PATTERNS.items():
                    if pattern in content and "frameworks" in tech:
                        frameworks.add(tech["frameworks"])
            except (OSError, PermissionError):
                continue

    # Check package.json for JS frameworks
    package_json = repo_path / "package.json"
    if package_json.exists():
        try:
            content = package_json.read_text(encoding="utf-8", errors="ignore").lower()
            for pattern, tech in FRAMEWORK_PATTERNS.items():
                if f'"{pattern}"' in content and "frameworks" in tech:
                    frameworks.add(tech["frameworks"])
        except (OSError, PermissionError):
            pass

    return frameworks


def detect_frameworks_for_file(filepath: Path) -> list[str]:
    """Detect frameworks from a specific package file."""
    frameworks: list[str] = []
    filename = filepath.name

    if not filepath.exists():
        return frameworks

    try:
        content = filepath.read_text(encoding="utf-8", errors="ignore").lower()
    except (OSError, PermissionError):
        return frameworks

    # Python package files
    if filename in ["pyproject.toml", "requirements.txt", "setup.py", "Pipfile"]:
        for pattern, tech in FRAMEWORK_PATTERNS.items():
            if pattern in content and "frameworks" in tech:
                frameworks.append(tech["frameworks"])

    # JavaScript package.json
    elif filename == "package.json":
        for pattern, tech in FRAMEWORK_PATTERNS.items():
            if f'"{pattern}"' in content and "frameworks" in tech:
                frameworks.append(tech["frameworks"])

    return frameworks
