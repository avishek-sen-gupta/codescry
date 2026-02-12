"""Tests for structured package file parsing and framework matching."""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from repo_surveyor.package_parsers import (
    ParsedDependency,
    _dep_matches_pattern,
    match_frameworks,
    parse_dependencies,
)
from repo_surveyor.package_parsers import pyproject_toml
from repo_surveyor.package_parsers import requirements_txt
from repo_surveyor.package_parsers import pipfile
from repo_surveyor.package_parsers import setup_py
from repo_surveyor.package_parsers import package_json
from repo_surveyor.package_parsers import pom_xml
from repo_surveyor.package_parsers import build_gradle
from repo_surveyor.package_parsers import go_mod
from repo_surveyor.package_parsers import cargo_toml
from repo_surveyor.package_parsers import csproj
from repo_surveyor.package_parsers import packages_config
from repo_surveyor.detectors import FRAMEWORK_PATTERNS


# ---------------------------------------------------------------------------
# pyproject.toml parser
# ---------------------------------------------------------------------------
class TestPyprojectToml:
    def test_pep621_dependencies(self):
        content = """\
[project]
name = "myapp"
dependencies = [
    "fastapi>=0.100",
    "uvicorn[standard]",
    "SQLAlchemy>=2.0",
]
"""
        deps = pyproject_toml.parse(content)
        names = [d.name for d in deps]
        assert "fastapi" in names
        assert "uvicorn" in names
        assert "sqlalchemy" in names

    def test_poetry_dependencies(self):
        content = """\
[tool.poetry.dependencies]
python = "^3.11"
Django = "^4.2"
celery = "^5.3"

[tool.poetry.dev-dependencies]
pytest = "^7.0"
"""
        deps = pyproject_toml.parse(content)
        names = [d.name for d in deps]
        assert "django" in names
        assert "celery" in names
        assert "pytest" in names

    def test_poetry_group_dependencies(self):
        content = """\
[tool.poetry.group.dev.dependencies]
black = "^23.0"
flask = "^2.0"
"""
        deps = pyproject_toml.parse(content)
        names = [d.name for d in deps]
        assert "black" in names
        assert "flask" in names

    def test_optional_dependencies(self):
        content = """\
[project]
name = "mylib"
dependencies = ["requests"]

[project.optional-dependencies]
web = ["flask>=2.0", "gunicorn"]
"""
        deps = pyproject_toml.parse(content)
        names = [d.name for d in deps]
        assert "requests" in names
        assert "flask" in names
        assert "gunicorn" in names

    def test_malformed_toml_returns_empty(self):
        assert pyproject_toml.parse("this is not toml [[[") == []

    def test_source_field(self):
        content = '[project]\ndependencies = ["fastapi"]'
        deps = pyproject_toml.parse(content)
        assert deps[0].source == "pyproject.toml"


# ---------------------------------------------------------------------------
# requirements.txt parser
# ---------------------------------------------------------------------------
class TestRequirementsTxt:
    def test_basic_requirements(self):
        content = """\
flask==2.3.0
requests>=2.28
gunicorn
"""
        deps = requirements_txt.parse(content)
        names = [d.name for d in deps]
        assert "flask" in names
        assert "requests" in names
        assert "gunicorn" in names

    def test_skips_comments_and_blanks(self):
        content = """\
# Web framework
flask==2.3.0

# HTTP client
requests>=2.28
"""
        deps = requirements_txt.parse(content)
        assert len(deps) == 2

    def test_skips_option_lines(self):
        content = """\
-r base.txt
--index-url https://pypi.org/simple
flask==2.3.0
"""
        deps = requirements_txt.parse(content)
        assert len(deps) == 1
        assert deps[0].name == "flask"

    def test_extras_stripped(self):
        content = "uvicorn[standard]>=0.20"
        deps = requirements_txt.parse(content)
        assert deps[0].name == "uvicorn"


# ---------------------------------------------------------------------------
# Pipfile parser
# ---------------------------------------------------------------------------
class TestPipfile:
    def test_packages_and_dev_packages(self):
        content = """\
[packages]
django = "==4.2"
celery = "*"

[dev-packages]
pytest = "*"
"""
        deps = pipfile.parse(content)
        names = [d.name for d in deps]
        assert "django" in names
        assert "celery" in names
        assert "pytest" in names

    def test_malformed_returns_empty(self):
        assert pipfile.parse("not valid toml [[[") == []


# ---------------------------------------------------------------------------
# setup.py parser
# ---------------------------------------------------------------------------
class TestSetupPy:
    def test_install_requires(self):
        content = """\
from setuptools import setup

setup(
    name="myapp",
    install_requires=[
        "flask>=2.0",
        "SQLAlchemy>=1.4",
        "celery",
    ],
)
"""
        deps = setup_py.parse(content)
        names = [d.name for d in deps]
        assert "flask" in names
        assert "sqlalchemy" in names
        assert "celery" in names

    def test_no_install_requires(self):
        content = "from setuptools import setup\nsetup(name='mylib')"
        assert setup_py.parse(content) == []


# ---------------------------------------------------------------------------
# package.json parser
# ---------------------------------------------------------------------------
class TestPackageJson:
    def test_all_dependency_sections(self):
        content = """\
{
  "dependencies": {
    "react": "^18.0",
    "react-dom": "^18.0"
  },
  "devDependencies": {
    "typescript": "^5.0",
    "@types/react": "^18.0"
  },
  "peerDependencies": {
    "react": ">=17"
  }
}
"""
        deps = package_json.parse(content)
        names = [d.name for d in deps]
        assert "react" in names
        assert "react-dom" in names
        assert "typescript" in names
        assert "@types/react" in names

    def test_scoped_packages(self):
        content = (
            '{"dependencies": {"@nestjs/core": "^10.0", "@nestjs/common": "^10.0"}}'
        )
        deps = package_json.parse(content)
        names = [d.name for d in deps]
        assert "@nestjs/core" in names
        assert "@nestjs/common" in names

    def test_malformed_json_returns_empty(self):
        assert package_json.parse("not json {{{") == []


# ---------------------------------------------------------------------------
# pom.xml parser
# ---------------------------------------------------------------------------
class TestPomXml:
    def test_namespaced_pom(self):
        content = """\
<?xml version="1.0"?>
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <dependencies>
    <dependency>
      <groupId>org.springframework.boot</groupId>
      <artifactId>spring-boot-starter-web</artifactId>
    </dependency>
    <dependency>
      <groupId>io.projectreactor</groupId>
      <artifactId>reactive-streams</artifactId>
    </dependency>
  </dependencies>
</project>
"""
        deps = pom_xml.parse(content)
        names = [d.name for d in deps]
        assert "spring-boot-starter-web" in names
        assert "reactive-streams" in names

    def test_non_namespaced_pom(self):
        content = """\
<project>
  <dependencies>
    <dependency>
      <artifactId>jersey-server</artifactId>
    </dependency>
  </dependencies>
</project>
"""
        deps = pom_xml.parse(content)
        assert deps[0].name == "jersey-server"

    def test_malformed_xml_returns_empty(self):
        assert pom_xml.parse("not xml <<<") == []

    def test_reactive_streams_not_matched_as_react(self):
        """Regression: reactive-streams must not match 'react' pattern."""
        content = """\
<?xml version="1.0"?>
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <dependencies>
    <dependency>
      <groupId>io.projectreactor</groupId>
      <artifactId>reactive-streams</artifactId>
    </dependency>
  </dependencies>
</project>
"""
        deps = pom_xml.parse(content)
        frameworks = match_frameworks(deps, FRAMEWORK_PATTERNS)
        assert "React" not in frameworks


# ---------------------------------------------------------------------------
# build.gradle parser
# ---------------------------------------------------------------------------
class TestBuildGradle:
    def test_various_configurations(self):
        content = """\
dependencies {
    implementation 'org.springframework.boot:spring-boot-starter-web:3.1.0'
    api "io.micronaut:micronaut-http-server:4.0"
    testImplementation 'junit:junit:4.13'
    compileOnly 'org.projectlombok:lombok:1.18'
}
"""
        deps = build_gradle.parse(content)
        names = [d.name for d in deps]
        assert "spring-boot-starter-web" in names
        assert "micronaut-http-server" in names
        assert "junit" in names
        assert "lombok" in names

    def test_kotlin_dsl_parenthesised(self):
        content = """\
dependencies {
    implementation("io.quarkus:quarkus-resteasy:3.0")
    testImplementation("io.quarkus:quarkus-junit5:3.0")
}
"""
        deps = build_gradle.parse(content)
        names = [d.name for d in deps]
        assert "quarkus-resteasy" in names
        assert "quarkus-junit5" in names


# ---------------------------------------------------------------------------
# go.mod parser
# ---------------------------------------------------------------------------
class TestGoMod:
    def test_require_block(self):
        content = """\
module github.com/myorg/myapp

go 1.21

require (
\tgithub.com/gin-gonic/gin v1.9.1
\tgithub.com/go-chi/chi/v5 v5.0.10
)
"""
        deps = go_mod.parse(content)
        names = [d.name for d in deps]
        assert "github.com/gin-gonic/gin" in names
        assert "github.com/go-chi/chi/v5" in names

    def test_single_line_require(self):
        content = """\
module mymod

go 1.21

require github.com/labstack/echo/v4 v4.11.0
"""
        deps = go_mod.parse(content)
        names = [d.name for d in deps]
        assert "github.com/labstack/echo/v4" in names


# ---------------------------------------------------------------------------
# Cargo.toml parser
# ---------------------------------------------------------------------------
class TestCargoToml:
    def test_all_dependency_sections(self):
        content = """\
[dependencies]
actix-web = "4"
serde = { version = "1", features = ["derive"] }

[dev-dependencies]
tokio-test = "0.4"

[build-dependencies]
cc = "1"
"""
        deps = cargo_toml.parse(content)
        names = [d.name for d in deps]
        assert "actix-web" in names
        assert "serde" in names
        assert "tokio-test" in names
        assert "cc" in names

    def test_malformed_returns_empty(self):
        assert cargo_toml.parse("not valid toml [[[") == []


# ---------------------------------------------------------------------------
# .csproj parser
# ---------------------------------------------------------------------------
class TestCsproj:
    def test_sdk_style_project(self):
        content = """\
<Project Sdk="Microsoft.NET.Sdk.Web">
  <ItemGroup>
    <PackageReference Include="Microsoft.AspNetCore.Mvc" Version="2.2.0" />
    <PackageReference Include="Newtonsoft.Json" Version="13.0.1" />
  </ItemGroup>
</Project>
"""
        deps = csproj.parse(content)
        names = [d.name for d in deps]
        assert "microsoft.aspnetcore.mvc" in names
        assert "newtonsoft.json" in names

    def test_old_style_namespaced_project(self):
        content = """\
<?xml version="1.0" encoding="utf-8"?>
<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
  <ItemGroup>
    <PackageReference Include="System.ServiceModel.Http" Version="4.10.0" />
  </ItemGroup>
</Project>
"""
        deps = csproj.parse(content)
        names = [d.name for d in deps]
        assert "system.servicemodel.http" in names

    def test_multiple_item_groups(self):
        content = """\
<Project Sdk="Microsoft.NET.Sdk">
  <ItemGroup>
    <PackageReference Include="ServiceStack" Version="6.0" />
  </ItemGroup>
  <ItemGroup>
    <PackageReference Include="Carter" Version="7.0" />
  </ItemGroup>
</Project>
"""
        deps = csproj.parse(content)
        names = [d.name for d in deps]
        assert "servicestack" in names
        assert "carter" in names

    def test_malformed_xml_returns_empty(self):
        assert csproj.parse("not xml <<<") == []

    def test_source_field(self):
        content = '<Project><ItemGroup><PackageReference Include="Nancy" Version="2.0" /></ItemGroup></Project>'
        deps = csproj.parse(content)
        assert deps[0].source == ".csproj"


# ---------------------------------------------------------------------------
# packages.config parser
# ---------------------------------------------------------------------------
class TestPackagesConfig:
    def test_basic_packages(self):
        content = """\
<?xml version="1.0" encoding="utf-8"?>
<packages>
  <package id="Microsoft.AspNet.WebApi.Core" version="5.2.9" targetFramework="net48" />
  <package id="Newtonsoft.Json" version="13.0.1" targetFramework="net48" />
  <package id="Nancy" version="2.0.0" targetFramework="net48" />
</packages>
"""
        deps = packages_config.parse(content)
        names = [d.name for d in deps]
        assert "microsoft.aspnet.webapi.core" in names
        assert "newtonsoft.json" in names
        assert "nancy" in names

    def test_malformed_xml_returns_empty(self):
        assert packages_config.parse("not xml <<<") == []

    def test_source_field(self):
        content = '<packages><package id="Nancy" version="2.0" /></packages>'
        deps = packages_config.parse(content)
        assert deps[0].source == "packages.config"


# ---------------------------------------------------------------------------
# Smart matching logic
# ---------------------------------------------------------------------------
class TestDepMatchesPattern:
    def test_exact_match(self):
        assert _dep_matches_pattern("fastapi", "fastapi")

    def test_no_substring_match(self):
        assert not _dep_matches_pattern("reactive-streams", "react")

    def test_prefix_hyphen_match(self):
        assert _dep_matches_pattern("spring-boot-starter-web", "spring-boot")

    def test_prefix_no_hyphen_no_match(self):
        assert not _dep_matches_pattern("expression", "express")

    def test_path_segment_match(self):
        assert _dep_matches_pattern("github.com/gin-gonic/gin", "gin-gonic/gin")

    def test_npm_scoped_match(self):
        assert _dep_matches_pattern("@nestjs/core", "nestjs")

    def test_npm_scoped_no_partial(self):
        assert not _dep_matches_pattern("@nestjs/core", "nest")

    def test_no_false_positive_express(self):
        assert _dep_matches_pattern(
            "express-validator", "express-validator"
        )  # exact match is fine
        assert not _dep_matches_pattern("expression", "express")

    def test_path_segment_partial_no_match(self):
        assert _dep_matches_pattern("github.com/gofiber/fiber/v2", "gofiber/fiber")
        # "fiber" alone should not match a multi-segment pattern
        assert not _dep_matches_pattern("github.com/gofiber/fiber/v2", "fib")


# ---------------------------------------------------------------------------
# match_frameworks integration
# ---------------------------------------------------------------------------
class TestMatchFrameworks:
    def test_spring_detected_from_pom(self):
        deps = [ParsedDependency("spring-boot-starter-web", "pom.xml")]
        assert "Spring" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_react_not_detected_from_reactive_streams(self):
        deps = [ParsedDependency("reactive-streams", "pom.xml")]
        assert "React" not in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_express_not_detected_from_expression(self):
        deps = [ParsedDependency("expression", "package.json")]
        assert "Express" not in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_express_detected_from_express(self):
        deps = [ParsedDependency("express", "package.json")]
        assert "Express" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_nestjs_detected_from_scoped_package(self):
        deps = [ParsedDependency("@nestjs/core", "package.json")]
        assert "NestJS" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_gin_detected_from_go_mod(self):
        deps = [ParsedDependency("github.com/gin-gonic/gin", "go.mod")]
        assert "Gin" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_actix_detected_from_cargo(self):
        deps = [ParsedDependency("actix-web", "Cargo.toml")]
        assert "Actix" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_deduplicates_frameworks(self):
        deps = [
            ParsedDependency("spring-boot-starter-web", "pom.xml"),
            ParsedDependency("spring-web", "pom.xml"),
        ]
        frameworks = match_frameworks(deps, FRAMEWORK_PATTERNS)
        assert frameworks.count("Spring") == 1

    def test_multiple_frameworks_detected(self):
        deps = [
            ParsedDependency("fastapi", "pyproject.toml"),
            ParsedDependency("celery", "pyproject.toml"),
            ParsedDependency("starlette", "pyproject.toml"),
        ]
        frameworks = match_frameworks(deps, FRAMEWORK_PATTERNS)
        assert "FastAPI" in frameworks
        assert "Starlette" in frameworks

    def test_dropwizard_detected(self):
        deps = [ParsedDependency("dropwizard-core", "pom.xml")]
        assert "Dropwizard" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_vertx_detected(self):
        deps = [ParsedDependency("vertx-web", "pom.xml")]
        assert "Vert.x" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_vertx_core_detected(self):
        deps = [ParsedDependency("vertx-core", "pom.xml")]
        assert "Vert.x" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_play_java_detected(self):
        deps = [ParsedDependency("play-java", "build.gradle")]
        assert "Play" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_apache_cxf_jaxrs_detected(self):
        deps = [ParsedDependency("cxf-rt-frontend-jaxrs", "pom.xml")]
        assert "Apache CXF" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_apache_cxf_jaxws_detected(self):
        deps = [ParsedDependency("cxf-rt-frontend-jaxws", "pom.xml")]
        assert "Apache CXF" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_axis2_detected(self):
        deps = [ParsedDependency("axis2", "pom.xml")]
        assert "Apache Axis2" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_spring_ws_detected(self):
        deps = [ParsedDependency("spring-ws-core", "pom.xml")]
        assert "Spring WS" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_jaxws_rt_detected(self):
        deps = [ParsedDependency("jaxws-rt", "pom.xml")]
        assert "JAX-WS" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_metro_jaxws_detected(self):
        deps = [ParsedDependency("metro-jax-ws", "pom.xml")]
        assert "JAX-WS" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_aspnet_core_detected(self):
        deps = [ParsedDependency("microsoft.aspnetcore.mvc", ".csproj")]
        assert "ASP.NET Core" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_aspnet_webapi_detected(self):
        deps = [ParsedDependency("microsoft.aspnet.webapi.core", ".csproj")]
        assert "ASP.NET Web API" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_servicestack_detected(self):
        deps = [ParsedDependency("servicestack", ".csproj")]
        assert "ServiceStack" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_nancy_detected(self):
        deps = [ParsedDependency("nancy", "packages.config")]
        assert "Nancy" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_carter_detected(self):
        deps = [ParsedDependency("carter", ".csproj")]
        assert "Carter" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_wcf_detected(self):
        deps = [ParsedDependency("system.servicemodel.http", ".csproj")]
        assert "WCF" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_corewcf_detected(self):
        deps = [ParsedDependency("corewcf", ".csproj")]
        assert "CoreWCF" in match_frameworks(deps, FRAMEWORK_PATTERNS)

    def test_aspnet_core_via_prefix_match(self):
        """microsoft.aspnetcore.authentication should also match ASP.NET Core."""
        deps = [ParsedDependency("microsoft.aspnetcore.authentication", ".csproj")]
        assert "ASP.NET Core" in match_frameworks(deps, FRAMEWORK_PATTERNS)


# ---------------------------------------------------------------------------
# parse_dependencies dispatch
# ---------------------------------------------------------------------------
class TestParseDependencies:
    def test_dispatches_to_correct_parser(self):
        content = '{"dependencies": {"express": "^4.18"}}'
        deps = parse_dependencies("package.json", content)
        assert len(deps) == 1
        assert deps[0].name == "express"

    def test_unknown_filename_returns_empty(self):
        assert parse_dependencies("unknown.xyz", "some content") == []

    def test_build_gradle_kts_dispatches(self):
        content = (
            'implementation("org.springframework.boot:spring-boot-starter-web:3.0")'
        )
        deps = parse_dependencies("build.gradle.kts", content)
        names = [d.name for d in deps]
        assert "spring-boot-starter-web" in names

    def test_csproj_dispatches_by_extension(self):
        content = '<Project><ItemGroup><PackageReference Include="Carter" Version="7.0" /></ItemGroup></Project>'
        deps = parse_dependencies("MyApp.csproj", content)
        assert len(deps) == 1
        assert deps[0].name == "carter"

    def test_packages_config_dispatches(self):
        content = '<packages><package id="Nancy" version="2.0" /></packages>'
        deps = parse_dependencies("packages.config", content)
        assert len(deps) == 1
        assert deps[0].name == "nancy"
