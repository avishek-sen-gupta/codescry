"""Tests for the plugin registry and languages.json configuration."""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from repo_surveyor.language_plugin import PluginRegistry
from repo_surveyor.integration_patterns.types import Language


class TestIndicatorFilesParity:
    """Verify the registry produces the same INDICATOR_FILES as the old hardcoded dict."""

    def setup_method(self):
        self.registry = PluginRegistry()
        self.indicator_files = self.registry.indicator_files()

    def test_pyproject_toml(self):
        entry = self.indicator_files["pyproject.toml"]
        assert "Python" in entry["languages"]
        assert "Poetry" in entry["package_managers"]

    def test_requirements_txt(self):
        entry = self.indicator_files["requirements.txt"]
        assert "Python" in entry["languages"]
        assert "pip" in entry["package_managers"]

    def test_setup_py(self):
        entry = self.indicator_files["setup.py"]
        assert "Python" in entry["languages"]
        assert "pip" in entry["package_managers"]

    def test_pipfile(self):
        entry = self.indicator_files["Pipfile"]
        assert "Python" in entry["languages"]
        assert "Pipenv" in entry["package_managers"]

    def test_package_json(self):
        entry = self.indicator_files["package.json"]
        assert "JavaScript" in entry["languages"]
        assert "npm" in entry["package_managers"]

    def test_yarn_lock(self):
        entry = self.indicator_files["yarn.lock"]
        assert "Yarn" in entry["package_managers"]
        assert "languages" not in entry

    def test_pnpm_lock(self):
        entry = self.indicator_files["pnpm-lock.yaml"]
        assert "pnpm" in entry["package_managers"]

    def test_tsconfig(self):
        entry = self.indicator_files["tsconfig.json"]
        assert "TypeScript" in entry["languages"]

    def test_pom_xml(self):
        entry = self.indicator_files["pom.xml"]
        assert "Java" in entry["languages"]
        assert "Maven" in entry["package_managers"]

    def test_build_gradle(self):
        entry = self.indicator_files["build.gradle"]
        assert "Java" in entry["languages"]
        assert "Gradle" in entry["package_managers"]

    def test_build_gradle_kts(self):
        entry = self.indicator_files["build.gradle.kts"]
        assert "Java" in entry["languages"]
        assert "Kotlin" in entry["languages"]
        assert "Gradle" in entry["package_managers"]

    def test_go_mod(self):
        entry = self.indicator_files["go.mod"]
        assert "Go" in entry["languages"]

    def test_cargo_toml(self):
        entry = self.indicator_files["Cargo.toml"]
        assert "Rust" in entry["languages"]
        assert "Cargo" in entry["package_managers"]

    def test_gemfile(self):
        entry = self.indicator_files["Gemfile"]
        assert "Ruby" in entry["languages"]
        assert "Bundler" in entry["package_managers"]

    def test_packages_config(self):
        entry = self.indicator_files["packages.config"]
        assert "C#" in entry["languages"]
        assert "NuGet" in entry["package_managers"]

    def test_dockerfile(self):
        entry = self.indicator_files["Dockerfile"]
        assert "Docker" in entry["infrastructure"]

    def test_docker_compose_yml(self):
        entry = self.indicator_files["docker-compose.yml"]
        assert "Docker Compose" in entry["infrastructure"]

    def test_docker_compose_yaml(self):
        entry = self.indicator_files["docker-compose.yaml"]
        assert "Docker Compose" in entry["infrastructure"]


class TestExtensionLanguagesParity:
    """Verify extension_languages() matches the old EXTENSION_LANGUAGES dict."""

    def setup_method(self):
        self.ext_langs = PluginRegistry().extension_languages()

    def test_python(self):
        assert self.ext_langs[".py"] == "Python"

    def test_javascript(self):
        assert self.ext_langs[".js"] == "JavaScript"
        assert self.ext_langs[".jsx"] == "JavaScript"

    def test_typescript(self):
        assert self.ext_langs[".ts"] == "TypeScript"
        assert self.ext_langs[".tsx"] == "TypeScript"

    def test_java(self):
        assert self.ext_langs[".java"] == "Java"

    def test_kotlin(self):
        assert self.ext_langs[".kt"] == "Kotlin"

    def test_go(self):
        assert self.ext_langs[".go"] == "Go"

    def test_rust(self):
        assert self.ext_langs[".rs"] == "Rust"

    def test_ruby(self):
        assert self.ext_langs[".rb"] == "Ruby"

    def test_csharp(self):
        assert self.ext_langs[".cs"] == "C#"

    def test_cpp(self):
        assert self.ext_langs[".cpp"] == "C++"
        assert self.ext_langs[".hpp"] == "C++"

    def test_c(self):
        assert self.ext_langs[".c"] == "C"
        assert self.ext_langs[".h"] == "C"

    def test_shell(self):
        assert self.ext_langs[".sh"] == "Shell"
        assert self.ext_langs[".bash"] == "Shell"
        assert self.ext_langs[".zsh"] == "Shell"

    def test_cobol(self):
        assert self.ext_langs[".cbl"] == "COBOL"
        assert self.ext_langs[".cob"] == "COBOL"
        assert self.ext_langs[".cpy"] == "COBOL"

    def test_swift(self):
        assert self.ext_langs[".swift"] == "Swift"

    def test_php(self):
        assert self.ext_langs[".php"] == "PHP"

    def test_scala(self):
        assert self.ext_langs[".scala"] == "Scala"

    def test_clojure(self):
        assert self.ext_langs[".clj"] == "Clojure"

    def test_elixir(self):
        assert self.ext_langs[".ex"] == "Elixir"
        assert self.ext_langs[".exs"] == "Elixir"

    def test_erlang(self):
        assert self.ext_langs[".erl"] == "Erlang"

    def test_haskell(self):
        assert self.ext_langs[".hs"] == "Haskell"

    def test_ocaml(self):
        assert self.ext_langs[".ml"] == "OCaml"

    def test_fsharp(self):
        assert self.ext_langs[".fs"] == "F#"

    def test_r(self):
        assert self.ext_langs[".r"] == "R"
        assert self.ext_langs[".R"] == "R"

    def test_julia(self):
        assert self.ext_langs[".jl"] == "Julia"

    def test_lua(self):
        assert self.ext_langs[".lua"] == "Lua"

    def test_perl(self):
        assert self.ext_langs[".pl"] == "Perl"


class TestFrameworkPatternsParity:
    """Verify all_framework_patterns() matches the old FRAMEWORK_PATTERNS dict."""

    def setup_method(self):
        self.patterns = PluginRegistry().all_framework_patterns()

    def test_python_frameworks(self):
        assert self.patterns["fastapi"] == {"frameworks": "FastAPI"}
        assert self.patterns["django"] == {"frameworks": "Django"}
        assert self.patterns["flask"] == {"frameworks": "Flask"}
        assert self.patterns["starlette"] == {"frameworks": "Starlette"}
        assert self.patterns["tornado"] == {"frameworks": "Tornado"}
        assert self.patterns["pyramid"] == {"frameworks": "Pyramid"}
        assert self.patterns["aiohttp"] == {"frameworks": "aiohttp"}

    def test_javascript_frameworks(self):
        assert self.patterns["react"] == {"frameworks": "React"}
        assert self.patterns["vue"] == {"frameworks": "Vue.js"}
        assert self.patterns["angular"] == {"frameworks": "Angular"}
        assert self.patterns["next"] == {"frameworks": "Next.js"}
        assert self.patterns["nuxt"] == {"frameworks": "Nuxt.js"}
        assert self.patterns["express"] == {"frameworks": "Express"}
        assert self.patterns["nestjs"] == {"frameworks": "NestJS"}
        assert self.patterns["svelte"] == {"frameworks": "Svelte"}
        assert self.patterns["gatsby"] == {"frameworks": "Gatsby"}
        assert self.patterns["fastify"] == {"frameworks": "Fastify"}

    def test_java_frameworks(self):
        assert self.patterns["spring-boot"] == {"frameworks": "Spring"}
        assert self.patterns["spring-web"] == {"frameworks": "Spring"}
        assert self.patterns["jersey"] == {"frameworks": "JAX-RS"}
        assert self.patterns["micronaut"] == {"frameworks": "Micronaut"}
        assert self.patterns["quarkus"] == {"frameworks": "Quarkus"}
        assert self.patterns["javalin"] == {"frameworks": "Javalin"}
        assert self.patterns["dropwizard"] == {"frameworks": "Dropwizard"}
        assert self.patterns["vertx-web"] == {"frameworks": "Vert.x"}
        assert self.patterns["play-java"] == {"frameworks": "Play"}
        assert self.patterns["cxf-rt-frontend-jaxrs"] == {"frameworks": "Apache CXF"}
        assert self.patterns["axis2"] == {"frameworks": "Apache Axis2"}
        assert self.patterns["spring-ws"] == {"frameworks": "Spring WS"}
        assert self.patterns["jaxws-rt"] == {"frameworks": "JAX-WS"}
        assert self.patterns["metro-jax-ws"] == {"frameworks": "JAX-WS"}

    def test_go_frameworks(self):
        assert self.patterns["gin-gonic/gin"] == {"frameworks": "Gin"}
        assert self.patterns["labstack/echo"] == {"frameworks": "Echo"}
        assert self.patterns["gofiber/fiber"] == {"frameworks": "Fiber"}
        assert self.patterns["go-chi/chi"] == {"frameworks": "Chi"}
        assert self.patterns["gorilla/mux"] == {"frameworks": "Gorilla"}

    def test_rust_frameworks(self):
        assert self.patterns["actix-web"] == {"frameworks": "Actix"}
        assert self.patterns["axum"] == {"frameworks": "Axum"}
        assert self.patterns["rocket"] == {"frameworks": "Rocket"}
        assert self.patterns["warp"] == {"frameworks": "Warp"}

    def test_dotnet_frameworks(self):
        assert self.patterns["microsoft.aspnetcore"] == {"frameworks": "ASP.NET Core"}
        assert self.patterns["microsoft.aspnet.webapi"] == {"frameworks": "ASP.NET Web API"}
        assert self.patterns["servicestack"] == {"frameworks": "ServiceStack"}
        assert self.patterns["nancy"] == {"frameworks": "Nancy"}
        assert self.patterns["carter"] == {"frameworks": "Carter"}
        assert self.patterns["system.servicemodel"] == {"frameworks": "WCF"}
        assert self.patterns["corewcf"] == {"frameworks": "CoreWCF"}


class TestGlobPatternsParity:
    """Verify glob_patterns() matches the old GLOB_PATTERNS dict."""

    def setup_method(self):
        self.globs = PluginRegistry().glob_patterns()

    def test_csproj(self):
        entry = self.globs["*.csproj"]
        assert "C#" in entry["languages"]
        assert "NuGet" in entry["package_managers"]
        assert ".NET" in entry["frameworks"]

    def test_sln(self):
        entry = self.globs["*.sln"]
        assert ".NET" in entry["frameworks"]

    def test_terraform(self):
        entry = self.globs["*.tf"]
        assert "Terraform" in entry["infrastructure"]


class TestK8sMarkersParity:
    """Verify k8s_markers() matches the old K8S_MARKERS list."""

    def test_all_markers_present(self):
        markers = PluginRegistry().k8s_markers()
        assert "apiVersion:" in markers
        assert "kind: Deployment" in markers
        assert "kind: Service" in markers
        assert "kind: Pod" in markers
        assert "kind: ConfigMap" in markers
        assert "kind: Secret" in markers
        assert "kind: Ingress" in markers
        assert "kind: StatefulSet" in markers
        assert "kind: DaemonSet" in markers


class TestPluginSpecificBehavior:
    """Tests for plugin-specific features like sharing and additional_languages."""

    def setup_method(self):
        self.registry = PluginRegistry()

    def test_build_gradle_kts_signals_java_and_kotlin(self):
        indicator_files = self.registry.indicator_files()
        entry = indicator_files["build.gradle.kts"]
        assert "Java" in entry["languages"]
        assert "Kotlin" in entry["languages"]

    def test_tsconfig_has_no_parser(self):
        plugin = self.registry.get_plugin("TypeScript")
        assert plugin is not None
        tsconfig_indicators = [
            ind for ind in plugin.indicator_files if ind.filename == "tsconfig.json"
        ]
        assert len(tsconfig_indicators) == 1
        assert tsconfig_indicators[0].parser_module_name == ""

    def test_typescript_inherits_javascript_framework_patterns(self):
        ts_plugin = self.registry.get_plugin("TypeScript")
        assert ts_plugin is not None
        # Should have JavaScript framework patterns
        assert "react" in ts_plugin.framework_patterns
        assert "express" in ts_plugin.framework_patterns
        assert "vue" in ts_plugin.framework_patterns

    def test_kotlin_inherits_java_framework_patterns(self):
        kt_plugin = self.registry.get_plugin("Kotlin")
        assert kt_plugin is not None
        # Should have Java framework patterns
        assert "spring-boot" in kt_plugin.framework_patterns
        assert "micronaut" in kt_plugin.framework_patterns
        assert "quarkus" in kt_plugin.framework_patterns

    def test_yarn_lock_signals_package_manager(self):
        indicator_files = self.registry.indicator_files()
        entry = indicator_files["yarn.lock"]
        assert "Yarn" in entry["package_managers"]
        assert "languages" not in entry

    def test_csproj_glob_indicator_has_parser(self):
        plugin = self.registry.get_plugin("C#")
        assert plugin is not None
        csproj_globs = [
            g for g in plugin.glob_indicators if g.pattern == "*.csproj"
        ]
        assert len(csproj_globs) == 1
        assert csproj_globs[0].parser_module_name == "csproj"
        assert ".NET" in csproj_globs[0].frameworks

    def test_language_without_integration_module(self):
        ruby = self.registry.get_plugin("Ruby")
        assert ruby is not None
        assert ruby.integration_module_name == ""

        shell = self.registry.get_plugin("Shell")
        assert shell is not None
        assert shell.integration_module_name == ""

        cpp = self.registry.get_plugin("C++")
        assert cpp is not None
        assert cpp.integration_module_name == ""


class TestIntegrationPatternLayer:
    """Tests for the integration pattern registry API."""

    def setup_method(self):
        self.registry = PluginRegistry()

    def test_extension_to_language_enum_java(self):
        ext_map = self.registry.extension_to_language_enum()
        assert ext_map[".java"] == Language.JAVA

    def test_extension_to_language_enum_python(self):
        ext_map = self.registry.extension_to_language_enum()
        assert ext_map[".py"] == Language.PYTHON

    def test_extension_to_language_enum_typescript(self):
        ext_map = self.registry.extension_to_language_enum()
        assert ext_map[".ts"] == Language.TYPESCRIPT
        assert ext_map[".tsx"] == Language.TYPESCRIPT

    def test_extension_to_language_enum_cobol(self):
        ext_map = self.registry.extension_to_language_enum()
        assert ext_map[".cbl"] == Language.COBOL
        assert ext_map[".cob"] == Language.COBOL
        assert ext_map[".cpy"] == Language.COBOL

    def test_extension_to_language_enum_pli(self):
        ext_map = self.registry.extension_to_language_enum()
        assert ext_map[".pli"] == Language.PLI
        assert ext_map[".pl1"] == Language.PLI
        assert ext_map[".plinc"] == Language.PLI

    def test_extension_to_language_enum_excludes_languages_without_enum(self):
        ext_map = self.registry.extension_to_language_enum()
        # Shell, C++, C, etc. don't have Language enum entries
        assert ".sh" not in ext_map
        assert ".cpp" not in ext_map
        assert ".c" not in ext_map

    def test_language_modules_loads_all_expected_modules(self):
        modules = self.registry.language_to_integration_module()
        assert Language.JAVA in modules
        assert Language.PYTHON in modules
        assert Language.RUST in modules
        assert Language.TYPESCRIPT in modules
        assert Language.JAVASCRIPT in modules
        assert Language.GO in modules
        assert Language.CSHARP in modules
        assert Language.COBOL in modules
        assert Language.PLI in modules

    def test_language_modules_have_base_patterns(self):
        modules = self.registry.language_to_integration_module()
        for lang, module in modules.items():
            assert hasattr(module, "BASE_PATTERNS"), f"{lang} module missing BASE_PATTERNS"
            assert hasattr(module, "FRAMEWORK_PATTERNS"), f"{lang} module missing FRAMEWORK_PATTERNS"


class TestLanguageFromName:
    """Tests for Language.from_name() classmethod."""

    def test_known_language(self):
        assert Language.from_name("Java") == Language.JAVA
        assert Language.from_name("Python") == Language.PYTHON
        assert Language.from_name("C#") == Language.CSHARP
        assert Language.from_name("PL/I") == Language.PLI

    def test_unknown_language(self):
        assert Language.from_name("FORTRAN") is None
        assert Language.from_name("") is None

    def test_all_enum_members_round_trip(self):
        for member in Language:
            assert Language.from_name(member.value) == member


class TestAllPlugins:
    """Tests for direct plugin access."""

    def test_all_plugins_non_empty(self):
        registry = PluginRegistry()
        plugins = registry.all_plugins()
        assert len(plugins) > 0

    def test_get_plugin_returns_none_for_unknown(self):
        registry = PluginRegistry()
        assert registry.get_plugin("FORTRAN") is None

    def test_each_plugin_has_at_least_one_extension(self):
        registry = PluginRegistry()
        for plugin in registry.all_plugins():
            assert len(plugin.extensions) > 0, f"{plugin.name} has no extensions"
