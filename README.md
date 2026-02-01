# Cartographer

A Python library for analyzing repository technology stacks and code structure.

## Features

- Detects programming languages, package managers, frameworks, and infrastructure
- Scans indicator files (package.json, pyproject.toml, Dockerfile, etc.)
- Associates technologies with their containing directories (useful for monorepos)
- Extracts code symbols using Universal CTags
- Persists analysis results to Neo4j graph database
- Generates plain text reports

## Installation

```bash
poetry install
```

## Usage

### Tech Stack Analysis

```python
from repo_surveyor import RepoSurveyor

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

### Code Structure Analysis with CTags

Extract code symbols (classes, methods, fields, etc.) using Universal CTags:

```python
from repo_surveyor import RepoSurveyor

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

### Persisting to Neo4j

Persist analysis results to a Neo4j graph database:

```python
from repo_surveyor import RepoSurveyor
from repo_surveyor.analysis_graph_builder import create_analysis_graph_builder

surveyor = RepoSurveyor("/path/to/repo")
report = surveyor.tech_stacks()
structure = surveyor.coarse_structure(languages=["Java"])

with create_analysis_graph_builder("bolt://localhost:7687", "neo4j", "password") as builder:
    builder.persist_tech_stacks(report)
    builder.persist_coarse_structure(structure, "/path/to/repo")
```

This creates a graph with:
- `Repository` nodes
- `Directory` nodes with hierarchy relationships
- `Language`, `PackageManager`, `Framework`, `Infrastructure` nodes
- `CodeSymbol` nodes with parent-child relationships based on scope

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

## Running Tests

```bash
poetry run pytest
```
