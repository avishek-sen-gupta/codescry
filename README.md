# Cartographer

A Python library for analyzing repository technology stacks.

## Features

- Detects programming languages, package managers, frameworks, and infrastructure
- Scans indicator files (package.json, pyproject.toml, Dockerfile, etc.)
- Associates technologies with their containing directories (useful for monorepos)
- Generates plain text reports

## Installation

```bash
poetry install
```

## Usage

```python
from src.repo_surveyor import RepoSurveyor

surveyor = RepoSurveyor("/path/to/repo")
report = surveyor.tech_stacks()
print(report.to_text())
```

Example output:

```
Repository Survey: /path/to/repo

Languages:
  - JavaScript
  - Python
  - TypeScript

Package Managers:
  - Poetry
  - npm

Frameworks:
  - FastAPI
  - React

Infrastructure:
  - Docker

Directory Markers:
  ./
    Marker: docker-compose.yml
    Infrastructure: Docker Compose
  backend/
    Marker: pyproject.toml
    Languages: Python
    Package Managers: Poetry
    Frameworks: FastAPI
  frontend/
    Marker: package.json
    Languages: JavaScript
    Package Managers: npm
    Frameworks: React
```

## Detected Technologies

| Category | Indicator Files | Technologies |
|----------|-----------------|--------------|
| Python | `pyproject.toml`, `requirements.txt`, `setup.py`, `Pipfile` | Python, Poetry, pip, Pipenv |
| JavaScript/Node | `package.json`, `yarn.lock`, `pnpm-lock.yaml` | Node.js, npm, Yarn, pnpm |
| TypeScript | `tsconfig.json` | TypeScript |
| Java | `pom.xml`, `build.gradle`, `build.gradle.kts` | Java, Maven, Gradle |
| Go | `go.mod` | Go |
| Rust | `Cargo.toml` | Rust, Cargo |
| Ruby | `Gemfile` | Ruby, Bundler |
| .NET | `*.csproj`, `*.sln` | C#, .NET |
| Docker | `Dockerfile`, `docker-compose.yml` | Docker, Docker Compose |
| Terraform | `*.tf` | Terraform |
| Kubernetes | `*.yaml` with k8s markers | Kubernetes |

## Code Structure Analysis with CTags

Extract code symbols (classes, methods, fields, etc.) using Universal CTags:

```python
from src.repo_surveyor import RepoSurveyor

surveyor = RepoSurveyor("/path/to/repo")
result = surveyor.coarse_structure(languages=["Java"])

for entry in result.entries:
    print(f"{entry.kind}: {entry.name} at {entry.path}:{entry.line}")
```

Options:
- `languages`: Filter by language (e.g., `["Java", "Python"]`)
- `exclude_patterns`: Directories to exclude (defaults: `.git`, `.idea`, `target`, `node_modules`, etc.)
- `verbose`: Enable verbose CTags output

Each `CTagsEntry` contains:
- `name`, `path`, `kind`, `line`
- `scope`, `scope_kind` (containing class/function)
- `signature` (for methods)
- `language`

Requires [Universal CTags](https://github.com/universal-ctags/ctags) to be installed.

## Running Tests

```bash
poetry run pytest
```
