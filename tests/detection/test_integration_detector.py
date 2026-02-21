"""Tests for integration point detection from file contents."""

import json
import tempfile
from pathlib import Path

import pytest

from repo_surveyor.detection.integration_detector import (
    Confidence,
    EntityType,
    FileMatch,
    IntegrationDetectorResult,
    IntegrationSignal,
    IntegrationType,
    Language,
    _build_import_gated_framework_map,
    _file_has_framework_import,
    _filter_frameworks_by_imports,
    classify_directory,
    detect_integrations,
    get_language_from_extension,
    get_patterns_for_language,
    scan_file_for_integrations,
)


class TestGetLanguageFromExtension:
    """Tests for language detection from file extension."""

    def test_java_extension(self) -> None:
        """Should detect Java from .java extension."""
        assert get_language_from_extension("Test.java") == Language.JAVA
        assert get_language_from_extension("/path/to/Test.java") == Language.JAVA

    def test_rust_extension(self) -> None:
        """Should detect Rust from .rs extension."""
        assert get_language_from_extension("main.rs") == Language.RUST

    def test_python_extension(self) -> None:
        """Should detect Python from .py extension."""
        assert get_language_from_extension("app.py") == Language.PYTHON

    def test_typescript_extension(self) -> None:
        """Should detect TypeScript from .ts and .tsx extensions."""
        assert get_language_from_extension("app.ts") == Language.TYPESCRIPT
        assert get_language_from_extension("component.tsx") == Language.TYPESCRIPT

    def test_javascript_extension(self) -> None:
        """Should detect JavaScript from .js and .jsx extensions."""
        assert get_language_from_extension("app.js") == Language.JAVASCRIPT
        assert get_language_from_extension("component.jsx") == Language.JAVASCRIPT

    def test_go_extension(self) -> None:
        """Should detect Go from .go extension."""
        assert get_language_from_extension("main.go") == Language.GO

    def test_csharp_extension(self) -> None:
        """Should detect C# from .cs extension."""
        assert get_language_from_extension("Program.cs") == Language.CSHARP

    def test_cobol_extension(self) -> None:
        """Should detect COBOL from .cbl, .cob, and .cpy extensions."""
        assert get_language_from_extension("PROGRAM.cbl") == Language.COBOL
        assert get_language_from_extension("program.cob") == Language.COBOL
        assert get_language_from_extension("COPYBOOK.cpy") == Language.COBOL

    def test_pli_extension(self) -> None:
        """Should detect PL/I from .pli and .pl1 extensions."""
        assert get_language_from_extension("PROGRAM.pli") == Language.PLI
        assert get_language_from_extension("program.pl1") == Language.PLI
        assert get_language_from_extension("include.plinc") == Language.PLI

    def test_php_extension(self) -> None:
        """Should detect PHP from .php extension."""
        assert get_language_from_extension("index.php") == Language.PHP

    def test_kotlin_extension(self) -> None:
        """Should detect Kotlin from .kt extension."""
        assert get_language_from_extension("Main.kt") == Language.KOTLIN

    def test_scala_extension(self) -> None:
        """Should detect Scala from .scala extension."""
        assert get_language_from_extension("App.scala") == Language.SCALA

    def test_ruby_extension(self) -> None:
        """Should detect Ruby from .rb extension."""
        assert get_language_from_extension("app.rb") == Language.RUBY

    def test_cpp_extension(self) -> None:
        """Should detect C++ from .cpp and .hpp extensions."""
        assert get_language_from_extension("main.cpp") == Language.CPP
        assert get_language_from_extension("header.hpp") == Language.CPP

    def test_c_extension(self) -> None:
        """Should detect C from .c and .h extensions."""
        assert get_language_from_extension("main.c") == Language.C
        assert get_language_from_extension("header.h") == Language.C

    def test_pascal_extension(self) -> None:
        """Should detect Pascal from .pas, .pp, .dpr, .lpr, and .inc extensions."""
        assert get_language_from_extension("Unit1.pas") == Language.PASCAL
        assert get_language_from_extension("Unit1.pp") == Language.PASCAL
        assert get_language_from_extension("Project1.dpr") == Language.PASCAL
        assert get_language_from_extension("Project1.lpr") == Language.PASCAL
        assert get_language_from_extension("types.inc") == Language.PASCAL

    def test_unknown_extension(self) -> None:
        """Should return None for unknown extensions."""
        assert get_language_from_extension("file.xyz") is None
        assert get_language_from_extension("file.txt") is None


class TestGetPatternsForLanguage:
    """Tests for pattern retrieval by language."""

    def test_returns_patterns_for_known_language(self) -> None:
        """Should return patterns for known languages."""
        patterns = get_patterns_for_language(Language.JAVA, frameworks=["Spring"])
        assert IntegrationType.HTTP_REST in patterns
        assert len(patterns[IntegrationType.HTTP_REST]) > 0

    def test_returns_common_patterns_for_none_language(self) -> None:
        """Should return common patterns when language is None."""
        patterns = get_patterns_for_language(None)
        assert IntegrationType.HTTP_REST in patterns
        # Should have common patterns
        assert len(patterns[IntegrationType.HTTP_REST]) > 0

    def test_rust_patterns_include_framework_imports(self) -> None:
        """Should include Rust-specific framework patterns when frameworks active."""
        patterns = get_patterns_for_language(
            Language.RUST, frameworks=["Actix", "Warp"]
        )
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("actix_web" in p for p in http_patterns)
        assert any("warp" in p for p in http_patterns)

    def test_php_base_patterns_loaded(self) -> None:
        """Should load PHP base patterns with HTTP and database types."""
        patterns = get_patterns_for_language(Language.PHP)
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("curl_init" in p for p in http_patterns)
        db_patterns = [p[0] for p in patterns[IntegrationType.DATABASE]]
        assert any("PDO" in p for p in db_patterns)

    def test_php_laravel_framework_patterns(self) -> None:
        """Should include Laravel patterns when framework is active."""
        patterns = get_patterns_for_language(Language.PHP, frameworks=["Laravel"])
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("Route::get" in p for p in http_patterns)

    def test_php_symfony_framework_patterns(self) -> None:
        """Should include Symfony patterns when framework is active."""
        patterns = get_patterns_for_language(Language.PHP, frameworks=["Symfony"])
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("AbstractController" in p for p in http_patterns)

    def test_php_slim_framework_patterns(self) -> None:
        """Should include Slim patterns when framework is active."""
        patterns = get_patterns_for_language(Language.PHP, frameworks=["Slim"])
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("app->get" in p for p in http_patterns)

    def test_kotlin_base_patterns_loaded(self) -> None:
        """Should load Kotlin base patterns with HTTP and database types."""
        patterns = get_patterns_for_language(Language.KOTLIN)
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("ktor" in p for p in http_patterns)
        db_patterns = [p[0] for p in patterns[IntegrationType.DATABASE]]
        assert any("exposed" in p for p in db_patterns)

    def test_kotlin_ktor_framework_patterns(self) -> None:
        """Should include Ktor patterns when framework is active."""
        patterns = get_patterns_for_language(Language.KOTLIN, frameworks=["Ktor"])
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("embeddedServer" in p for p in http_patterns)

    def test_kotlin_spring_framework_patterns(self) -> None:
        """Should include Spring patterns when framework is active."""
        patterns = get_patterns_for_language(Language.KOTLIN, frameworks=["Spring"])
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("coRouter" in p for p in http_patterns)

    def test_scala_base_patterns_loaded(self) -> None:
        """Should load Scala base patterns with HTTP and database types."""
        patterns = get_patterns_for_language(Language.SCALA)
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("sttp" in p for p in http_patterns)
        db_patterns = [p[0] for p in patterns[IntegrationType.DATABASE]]
        assert any("slick" in p for p in db_patterns)

    def test_scala_play_framework_patterns(self) -> None:
        """Should include Play patterns when framework is active."""
        patterns = get_patterns_for_language(Language.SCALA, frameworks=["Play"])
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("play" in p and "mvc" in p for p in http_patterns)

    def test_scala_akka_http_framework_patterns(self) -> None:
        """Should include Akka HTTP patterns when framework is active."""
        patterns = get_patterns_for_language(Language.SCALA, frameworks=["Akka HTTP"])
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("akka" in p and "scaladsl" in p for p in http_patterns)

    def test_scala_http4s_framework_patterns(self) -> None:
        """Should include http4s patterns when framework is active."""
        patterns = get_patterns_for_language(Language.SCALA, frameworks=["http4s"])
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("HttpRoutes" in p for p in http_patterns)

    def test_go_base_patterns_loaded(self) -> None:
        """Should load Go base patterns with HTTP and database types."""
        patterns = get_patterns_for_language(Language.GO)
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("HandleFunc" in p for p in http_patterns)
        db_patterns = [p[0] for p in patterns[IntegrationType.DATABASE]]
        assert any("gorm" in p for p in db_patterns)

    def test_csharp_base_patterns_loaded(self) -> None:
        """Should load C# base patterns with HTTP and database types."""
        patterns = get_patterns_for_language(Language.CSHARP)
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("ApiController" in p for p in http_patterns)
        db_patterns = [p[0] for p in patterns[IntegrationType.DATABASE]]
        assert any("EntityFramework" in p for p in db_patterns)

    def test_cobol_base_patterns_loaded(self) -> None:
        """Should load COBOL base patterns with database and messaging types."""
        patterns = get_patterns_for_language(Language.COBOL)
        db_patterns = [p[0] for p in patterns[IntegrationType.DATABASE]]
        assert any("EXEC" in p and "SQL" in p for p in db_patterns)
        msg_patterns = [p[0] for p in patterns[IntegrationType.MESSAGING]]
        assert any("MQOPEN" in p for p in msg_patterns)

    def test_javascript_base_patterns_loaded(self) -> None:
        """Should load JavaScript base patterns with HTTP and database types."""
        patterns = get_patterns_for_language(Language.JAVASCRIPT)
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("fastify" in p for p in http_patterns)
        db_patterns = [p[0] for p in patterns[IntegrationType.DATABASE]]
        assert any("sequelize" in p for p in db_patterns)

    def test_ruby_base_patterns_loaded(self) -> None:
        """Should load Ruby base patterns with HTTP and database types."""
        patterns = get_patterns_for_language(Language.RUBY)
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("Net::HTTP" in p for p in http_patterns)
        db_patterns = [p[0] for p in patterns[IntegrationType.DATABASE]]
        assert any("ActiveRecord" in p for p in db_patterns)

    def test_cpp_base_patterns_loaded(self) -> None:
        """Should load C++ base patterns with HTTP and database types."""
        patterns = get_patterns_for_language(Language.CPP)
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("curl" in p for p in http_patterns)
        db_patterns = [p[0] for p in patterns[IntegrationType.DATABASE]]
        assert any("sqlite3" in p for p in db_patterns)

    def test_pli_base_patterns_loaded(self) -> None:
        """Should load PL/I base patterns with database and messaging types."""
        patterns = get_patterns_for_language(Language.PLI)
        db_patterns = [p[0] for p in patterns[IntegrationType.DATABASE]]
        assert any("EXEC" in p and "SQL" in p for p in db_patterns)
        msg_patterns = [p[0] for p in patterns[IntegrationType.MESSAGING]]
        assert any("MQOPEN" in p or "MQPUT" in p for p in msg_patterns)

    def test_pascal_base_patterns_loaded(self) -> None:
        """Should load Pascal base patterns with HTTP, database, and socket types."""
        patterns = get_patterns_for_language(Language.PASCAL)
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("TIdHTTP" in p for p in http_patterns)
        db_patterns = [p[0] for p in patterns[IntegrationType.DATABASE]]
        assert any("TFDConnection" in p for p in db_patterns)
        socket_patterns = [p[0] for p in patterns[IntegrationType.SOCKET]]
        assert any("TIdTCPServer" in p for p in socket_patterns)

    def test_pascal_covers_nine_integration_types(self) -> None:
        """Pascal base patterns should cover 9 integration types."""
        patterns = get_patterns_for_language(Language.PASCAL)
        pascal_types = [
            t for t in IntegrationType if any(p[2] == "Pascal" for p in patterns[t])
        ]
        assert len(pascal_types) == 9


class TestClassifyDirectory:
    """Tests for directory classification."""

    def test_classify_controllers_directory(self) -> None:
        """Should classify 'controllers' as HTTP_REST."""
        matches = classify_directory("controllers")
        types = [m[0] for m in matches]
        assert IntegrationType.HTTP_REST in types

    def test_classify_api_directory(self) -> None:
        """Should classify 'api' as HTTP_REST."""
        matches = classify_directory("api")
        types = [m[0] for m in matches]
        assert IntegrationType.HTTP_REST in types

    def test_classify_repository_directory(self) -> None:
        """Should classify 'repository' as DATABASE."""
        matches = classify_directory("repository")
        types = [m[0] for m in matches]
        assert IntegrationType.DATABASE in types

    def test_classify_models_directory(self) -> None:
        """Should classify 'models' as DATABASE."""
        matches = classify_directory("models")
        types = [m[0] for m in matches]
        assert IntegrationType.DATABASE in types

    def test_classify_kafka_directory(self) -> None:
        """Should classify 'kafka' as MESSAGING."""
        matches = classify_directory("kafka")
        types = [m[0] for m in matches]
        assert IntegrationType.MESSAGING in types

    def test_classify_unknown_directory(self) -> None:
        """Should return empty list for unknown directories."""
        matches = classify_directory("utils")
        assert len(matches) == 0


class TestScanFileForIntegrations:
    """Tests for scanning individual files."""

    def test_scan_java_file_with_spring_annotations(self) -> None:
        """Should detect Spring annotations in Java files."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("""
@RestController
@RequestMapping("/api/users")
public class UserController {
    @GetMapping("/{id}")
    public User getUser(@PathVariable Long id) {
        return userService.findById(id);
    }
}
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Spring"]))
            assert len(points) > 0

            http_points = [
                p for p in points if p.integration_type == IntegrationType.HTTP_REST
            ]
            assert len(http_points) > 0

            # Check for high confidence matches
            high_conf = [p for p in http_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_rust_file_with_actix(self) -> None:
        """Should detect Actix patterns in Rust files."""
        with tempfile.NamedTemporaryFile(suffix=".rs", mode="w", delete=False) as f:
            f.write("""
use actix_web::{get, web, HttpResponse};

#[get("/hello")]
async fn hello() -> HttpResponse {
    HttpResponse::Ok().body("Hello!")
}
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Actix"]))
            assert len(points) > 0

            http_points = [
                p for p in points if p.integration_type == IntegrationType.HTTP_REST
            ]
            assert len(http_points) > 0

            # Should have high confidence matches for actix imports
            high_conf = [p for p in http_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_python_file_with_flask(self) -> None:
        """Should detect Flask patterns in Python files."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write("""
from flask import Flask, jsonify

app = Flask(__name__)

@app.route('/api/users')
def get_users():
    return jsonify([])
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            assert len(points) > 0

            http_points = [
                p for p in points if p.integration_type == IntegrationType.HTTP_REST
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()

    def test_scan_file_with_database_patterns(self) -> None:
        """Should detect database patterns."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("""
@Repository
public interface UserRepository extends JpaRepository<User, Long> {
    @Query("SELECT u FROM User u WHERE u.email = :email")
    User findByEmail(@Param("email") String email);
}
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Spring"]))
            db_points = [
                p for p in points if p.integration_type == IntegrationType.DATABASE
            ]
            assert len(db_points) > 0

            # @Repository should be high confidence
            high_conf = [p for p in db_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_file_with_messaging_patterns(self) -> None:
        """Should detect messaging patterns."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("""
@KafkaListener(topics = "orders")
public void handleOrder(OrderEvent event) {
    processOrder(event);
}
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Spring"]))
            msg_points = [
                p for p in points if p.integration_type == IntegrationType.MESSAGING
            ]
            assert len(msg_points) > 0
        finally:
            file_path.unlink()

    def test_scan_cobol_file_with_db2(self) -> None:
        """Should detect DB2 embedded SQL patterns in COBOL files."""
        with tempfile.NamedTemporaryFile(suffix=".cbl", mode="w", delete=False) as f:
            f.write("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTREAD.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.
       PROCEDURE DIVISION.
           EXEC SQL
               SELECT CUST_NAME, CUST_ADDR
               INTO :WS-CUST-NAME, :WS-CUST-ADDR
               FROM CUSTOMER
               WHERE CUST_ID = :WS-CUST-ID
           END-EXEC.
           IF SQLCODE = 0
               DISPLAY 'CUSTOMER FOUND'
           END-IF.
           STOP RUN.
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            assert len(points) > 0

            db_points = [
                p for p in points if p.integration_type == IntegrationType.DATABASE
            ]
            assert len(db_points) > 0

            # EXEC SQL should be high confidence
            high_conf = [p for p in db_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_cobol_file_with_mq(self) -> None:
        """Should detect MQ patterns in COBOL files."""
        with tempfile.NamedTemporaryFile(suffix=".cbl", mode="w", delete=False) as f:
            f.write("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. MQSEND.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MQOD.
       01  MQMD.
       PROCEDURE DIVISION.
           CALL 'MQOPEN' USING HCONN MQOD MQOO HOBJ CC RC.
           CALL 'MQPUT' USING HCONN HOBJ MQMD MQPMO MSGLENGTH MSGBUFFER CC RC.
           CALL 'MQCLOSE' USING HCONN HOBJ MQCO CC RC.
           STOP RUN.
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            assert len(points) > 0

            msg_points = [
                p for p in points if p.integration_type == IntegrationType.MESSAGING
            ]
            assert len(msg_points) > 0

            # MQPUT/MQGET should be high confidence
            high_conf = [p for p in msg_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_cobol_file_with_idms(self) -> None:
        """Should detect IDMS patterns in COBOL files."""
        with tempfile.NamedTemporaryFile(suffix=".cbl", mode="w", delete=False) as f:
            f.write("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. IDMSREAD.
       DATA DIVISION.
       SCHEMA SECTION.
           COPY IDMS SUBSCHEMA-DESCRIPTION.
       WORKING-STORAGE SECTION.
       01  SUBSCHEMA-CTRL.
       01  IDMS-STATUS.
       PROCEDURE DIVISION.
           BIND RUN-UNIT.
           READY EMPLOYEE-AREA USAGE-MODE UPDATE.
           OBTAIN CALC EMPLOYEE.
           IF IDMS-STATUS = '0000'
               COMMIT TASK
           ELSE
               ROLLBACK TASK
           END-IF.
           FINISH TASK.
           STOP RUN.
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            assert len(points) > 0

            db_points = [
                p for p in points if p.integration_type == IntegrationType.DATABASE
            ]
            assert len(db_points) > 0

            # BIND RUN-UNIT, OBTAIN CALC, and SUBSCHEMA-CTRL should be high confidence
            high_conf = [p for p in db_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0

            # Check for specific IDMS patterns
            patterns = [p.matched_pattern for p in db_points]
            assert any(
                "BIND" in p or "OBTAIN" in p or "SUBSCHEMA-CTRL" in p for p in patterns
            )
        finally:
            file_path.unlink()

    def test_scan_pli_file_with_db2(self) -> None:
        """Should detect DB2 embedded SQL patterns in PL/I files."""
        with tempfile.NamedTemporaryFile(suffix=".pli", mode="w", delete=False) as f:
            f.write("""CUSTREAD: PROC OPTIONS(MAIN);
   DCL SQLCA EXTERNAL;
   %INCLUDE SQLCA;

   EXEC SQL
       SELECT CUST_NAME, CUST_ADDR
       INTO :WS_CUST_NAME, :WS_CUST_ADDR
       FROM CUSTOMER
       WHERE CUST_ID = :WS_CUST_ID;

   IF SQLCODE = 0 THEN
       PUT SKIP LIST('CUSTOMER FOUND');
   END;
END CUSTREAD;
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            assert len(points) > 0

            db_points = [
                p for p in points if p.integration_type == IntegrationType.DATABASE
            ]
            assert len(db_points) > 0

            # EXEC SQL and SQLCA should be high confidence
            high_conf = [p for p in db_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_pli_file_with_mq(self) -> None:
        """Should detect MQ patterns in PL/I files."""
        with tempfile.NamedTemporaryFile(suffix=".pli", mode="w", delete=False) as f:
            f.write("""MQSEND: PROC OPTIONS(MAIN);
   %INCLUDE CMQP;

   CALL MQCONN(QMGR_NAME, HCONN, CC, RC);
   CALL MQOPEN(HCONN, MQOD, MQOO_OUTPUT, HOBJ, CC, RC);
   CALL MQPUT(HCONN, HOBJ, MQMD, MQPMO, MSG_LEN, MSG_BUFFER, CC, RC);
   CALL MQCLOSE(HCONN, HOBJ, MQCO_NONE, CC, RC);
   CALL MQDISC(HCONN, CC, RC);

END MQSEND;
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            assert len(points) > 0

            msg_points = [
                p for p in points if p.integration_type == IntegrationType.MESSAGING
            ]
            assert len(msg_points) > 0

            # CALL MQPUT/MQGET should be high confidence
            high_conf = [p for p in msg_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_cobol_file_with_cics(self) -> None:
        """Should detect CICS patterns in COBOL files."""
        with tempfile.NamedTemporaryFile(suffix=".cbl", mode="w", delete=False) as f:
            f.write("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSWEB.
       PROCEDURE DIVISION.
           EXEC CICS WEB SEND
               FROM(WS-RESPONSE)
               FROMLENGTH(WS-RESP-LEN)
               MEDIATYPE('application/json')
           END-EXEC.
           EXEC CICS READ
               FILE('CUSTFILE')
               INTO(WS-CUST-REC)
               RIDFLD(WS-CUST-KEY)
           END-EXEC.
           EXEC CICS RETURN END-EXEC.
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            assert len(points) > 0

            # Should detect HTTP/REST (CICS WEB)
            http_points = [
                p for p in points if p.integration_type == IntegrationType.HTTP_REST
            ]
            assert len(http_points) > 0

            # Should detect DATABASE (CICS READ)
            db_points = [
                p for p in points if p.integration_type == IntegrationType.DATABASE
            ]
            assert len(db_points) > 0
        finally:
            file_path.unlink()

    def test_scan_pascal_file_with_firedac_database(self) -> None:
        """Should detect FireDAC database patterns in Pascal files."""
        with tempfile.NamedTemporaryFile(suffix=".pas", mode="w", delete=False) as f:
            f.write("""unit DataModule;

interface

uses
  FireDAC.Comp.Client, FireDAC.Stan.Def;

type
  TDM = class(TDataModule)
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
  end;

implementation

procedure TDM.LoadData;
begin
  FDQuery1.SQL.Text := 'SELECT * FROM customers';
  FDQuery1.Open;
end;

end.
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            assert len(points) > 0

            db_points = [
                p for p in points if p.integration_type == IntegrationType.DATABASE
            ]
            assert len(db_points) > 0

            high_conf = [p for p in db_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_pascal_file_with_indy_http(self) -> None:
        """Should detect Indy HTTP patterns in Pascal files."""
        with tempfile.NamedTemporaryFile(suffix=".pas", mode="w", delete=False) as f:
            f.write("""unit HttpClient;

interface

uses
  IdHTTP;

type
  TMyClient = class
    FHttp: TIdHTTP;
  end;

implementation

procedure TMyClient.FetchData;
var
  Response: string;
begin
  Response := FHttp.Get('https://api.example.com/data');
end;

end.
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            assert len(points) > 0

            http_points = [
                p for p in points if p.integration_type == IntegrationType.HTTP_REST
            ]
            assert len(http_points) > 0

            high_conf = [p for p in http_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_pascal_file_with_indy_tcp_server(self) -> None:
        """Should detect Indy TCP server patterns in Pascal files."""
        with tempfile.NamedTemporaryFile(suffix=".pas", mode="w", delete=False) as f:
            f.write("""unit SocketServer;

interface

uses
  IdTCPServer;

type
  TMyServer = class
    FServer: TIdTCPServer;
  end;

implementation

end.
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            assert len(points) > 0

            socket_points = [
                p for p in points if p.integration_type == IntegrationType.SOCKET
            ]
            assert len(socket_points) > 0
        finally:
            file_path.unlink()

    def test_scan_file_returns_line_info(self) -> None:
        """Should return correct line number and content."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("""package com.example;

import org.springframework.web.bind.annotation.*;

@RestController
public class MyController {
}
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Spring"]))
            rest_controller_points = [
                p for p in points if "@RestController" in p.matched_pattern
            ]
            assert len(rest_controller_points) > 0
            point = rest_controller_points[0]
            assert point.match.line_number == 5
            assert "@RestController" in point.match.line_content
        finally:
            file_path.unlink()


class TestDetectIntegrations:
    """Tests for the main detect_integrations function."""

    def test_detect_integrations_returns_result(self) -> None:
        """Should return IntegrationDetectorResult."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create a simple Java file
            java_file = Path(tmpdir) / "Test.java"
            java_file.write_text("@RestController\npublic class Test {}")

            result = detect_integrations(tmpdir, directory_frameworks={".": ["Spring"]})

            assert isinstance(result, IntegrationDetectorResult)
            assert result.files_scanned >= 1

    def test_detect_integrations_finds_http_rest(self) -> None:
        """Should detect HTTP/REST integration points."""
        with tempfile.TemporaryDirectory() as tmpdir:
            java_file = Path(tmpdir) / "UserController.java"
            java_file.write_text("""
@RestController
@RequestMapping("/api/users")
public class UserController {
    @GetMapping
    public List<User> getAll() { return null; }
}
""")

            result = detect_integrations(tmpdir, directory_frameworks={".": ["Spring"]})

            http_points = [
                p
                for p in result.integration_points
                if p.integration_type == IntegrationType.HTTP_REST
            ]
            assert len(http_points) >= 1

    def test_detect_integrations_with_language_filter(self) -> None:
        """Should filter by language when specified."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create files in different languages using base patterns
            java_file = Path(tmpdir) / "Test.java"
            java_file.write_text("@Entity\npublic class Test {}")

            py_file = Path(tmpdir) / "app.py"
            py_file.write_text("import requests\n")

            # Filter to Java only
            result = detect_integrations(tmpdir, languages=[Language.JAVA])

            assert result.files_scanned == 1

    def test_detect_integrations_empty_directory(self) -> None:
        """Should handle empty directory."""
        with tempfile.TemporaryDirectory() as tmpdir:
            result = detect_integrations(tmpdir)

            assert result.files_scanned == 0
            assert len(result.integration_points) == 0

    def test_detect_integrations_nonexistent_path(self) -> None:
        """Should handle non-existent path."""
        result = detect_integrations("/nonexistent/path/12345")

        assert result.files_scanned == 0
        assert len(result.integration_points) == 0

    def test_detect_integrations_skips_excluded_dirs(self) -> None:
        """Should skip excluded directories like node_modules."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create file in node_modules
            node_modules = Path(tmpdir) / "node_modules"
            node_modules.mkdir()
            js_file = node_modules / "package.js"
            js_file.write_text("app.get('/api', (req, res) => {});")

            # Create file outside node_modules
            src_file = Path(tmpdir) / "app.js"
            src_file.write_text("app.get('/api', (req, res) => {});")

            result = detect_integrations(tmpdir)

            # Should only scan the file outside node_modules
            assert result.files_scanned == 1

    def test_detect_integrations_excludes_directory_matches(self) -> None:
        """Directory classification is disabled; no DIRECTORY signals should appear."""
        with tempfile.TemporaryDirectory() as tmpdir:
            controllers = Path(tmpdir) / "controllers"
            controllers.mkdir()
            java_file = controllers / "UserController.java"
            java_file.write_text("public class UserController {}")

            result = detect_integrations(tmpdir)

            dir_points = [
                p
                for p in result.integration_points
                if p.entity_type == EntityType.DIRECTORY
            ]
            assert len(dir_points) == 0


class TestIntegrationSignalDataclass:
    """Tests for the IntegrationSignal dataclass."""

    def test_integration_point_is_frozen(self) -> None:
        """Should be immutable."""
        match = FileMatch(
            file_path="/test.java",
            line_number=1,
            line_content="test",
            language=Language.JAVA,
        )
        point = IntegrationSignal(
            match=match,
            integration_type=IntegrationType.HTTP_REST,
            confidence=Confidence.HIGH,
            matched_pattern="@RestController",
            entity_type=EntityType.FILE_CONTENT,
            source="Spring",
        )

        with pytest.raises(Exception):  # FrozenInstanceError
            point.confidence = Confidence.LOW  # type: ignore


class TestFileMatchDataclass:
    """Tests for the FileMatch dataclass."""

    def test_file_match_is_frozen(self) -> None:
        """Should be immutable."""
        match = FileMatch(
            file_path="/test.java",
            line_number=1,
            line_content="test",
            language=Language.JAVA,
        )

        with pytest.raises(Exception):  # FrozenInstanceError
            match.line_number = 2  # type: ignore


class TestConfidenceLevels:
    """Tests for confidence level assignment."""

    def test_high_confidence_for_annotations(self) -> None:
        """Annotations should have high confidence."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("@RestController\npublic class Test {}")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Spring"]))
            annotation_points = [
                p for p in points if "@RestController" in p.matched_pattern
            ]
            assert len(annotation_points) > 0
            assert annotation_points[0].confidence == Confidence.HIGH
        finally:
            file_path.unlink()

    def test_low_confidence_for_generic_terms(self) -> None:
        """Generic terms should have low confidence."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write('callExternalApi("http://example.com/api");\n')
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            api_points = [p for p in points if "(?i)\\bapi\\b" in p.matched_pattern]
            assert len(api_points) > 0
            assert api_points[0].confidence == Confidence.LOW
        finally:
            file_path.unlink()


class TestFrameworkAwarePatterns:
    """Tests for framework-specific pattern matching."""

    def test_flask_patterns_only_with_flask_framework(self) -> None:
        """Flask-specific patterns should only match when Flask framework is active."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write("from flask import Flask\n@app.route('/hello')\n")
            f.flush()
            file_path = Path(f.name)

        try:
            # Without Flask framework: should NOT match flask-specific pattern
            points_without = list(scan_file_for_integrations(file_path))
            flask_points = [
                p
                for p in points_without
                if "from flask import" in p.matched_pattern
                or r"@\w+\.route" in p.matched_pattern
            ]
            assert len(flask_points) == 0

            # With Flask framework: should match
            points_with = list(
                scan_file_for_integrations(file_path, frameworks=["Flask"])
            )
            flask_points = [
                p for p in points_with if r"@\w+\.route" in p.matched_pattern
            ]
            assert len(flask_points) > 0
            assert flask_points[0].confidence == Confidence.HIGH
        finally:
            file_path.unlink()

    def test_fastapi_patterns_only_with_fastapi_framework(self) -> None:
        """FastAPI-specific patterns should only match when FastAPI framework is active."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write(
                "from fastapi import FastAPI\n@app.get('/users')\ndef get_users(): pass\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            # Without FastAPI framework
            points_without = list(scan_file_for_integrations(file_path))
            fastapi_points = [
                p for p in points_without if r"@\w+\.get" in p.matched_pattern
            ]
            assert len(fastapi_points) == 0

            # With FastAPI framework
            points_with = list(
                scan_file_for_integrations(file_path, frameworks=["FastAPI"])
            )
            fastapi_points = [
                p for p in points_with if r"@\w+\.get" in p.matched_pattern
            ]
            assert len(fastapi_points) > 0
        finally:
            file_path.unlink()

    def test_django_patterns_only_with_django_framework(self) -> None:
        """Django-specific patterns should only match when Django framework is active."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write(
                "from django.db import models\nclass User(models.Model):\n    pass\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            # Without Django framework
            points_without = list(scan_file_for_integrations(file_path))
            django_db_points = [
                p for p in points_without if r"models\.Model" in p.matched_pattern
            ]
            assert len(django_db_points) == 0

            # With Django framework
            points_with = list(
                scan_file_for_integrations(file_path, frameworks=["Django"])
            )
            django_db_points = [
                p for p in points_with if r"models\.Model" in p.matched_pattern
            ]
            assert len(django_db_points) > 0
        finally:
            file_path.unlink()

    def test_base_python_patterns_always_match(self) -> None:
        """Base language patterns should match regardless of framework."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write(
                "from sqlalchemy import create_engine\n"
                "import requests\n"
                "name = Column(String)\n"
                "http = requests.Session()\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            # Without any framework
            points = list(scan_file_for_integrations(file_path))
            sqlalchemy_points = [
                p for p in points if p.integration_type == IntegrationType.DATABASE
            ]
            requests_points = [
                p for p in points if p.integration_type == IntegrationType.HTTP_REST
            ]
            assert len(sqlalchemy_points) > 0
            assert len(requests_points) > 0

            # With a framework - base patterns should still match
            points_with_fw = list(
                scan_file_for_integrations(file_path, frameworks=["Flask"])
            )
            sqlalchemy_points = [
                p
                for p in points_with_fw
                if p.integration_type == IntegrationType.DATABASE
            ]
            assert len(sqlalchemy_points) > 0
        finally:
            file_path.unlink()

    def test_nestjs_patterns_only_with_nestjs_framework(self) -> None:
        """NestJS-specific patterns should only match when NestJS framework is active."""
        with tempfile.NamedTemporaryFile(suffix=".ts", mode="w", delete=False) as f:
            f.write("@Controller('users')\n@Get()\n")
            f.flush()
            file_path = Path(f.name)

        try:
            # Without NestJS framework
            points_without = list(scan_file_for_integrations(file_path))
            nestjs_points = [
                p
                for p in points_without
                if r"@Controller\(" in p.matched_pattern
                or r"@Get\(" in p.matched_pattern
            ]
            assert len(nestjs_points) == 0

            # With NestJS framework
            points_with = list(
                scan_file_for_integrations(file_path, frameworks=["NestJS"])
            )
            nestjs_points = [
                p
                for p in points_with
                if r"@Controller\(" in p.matched_pattern
                or r"@Get\(" in p.matched_pattern
            ]
            assert len(nestjs_points) > 0
        finally:
            file_path.unlink()

    def test_express_js_patterns_only_with_express_framework(self) -> None:
        """Express-specific patterns should only match when Express framework is active."""
        with tempfile.NamedTemporaryFile(suffix=".js", mode="w", delete=False) as f:
            f.write("const express = require('express');\napp.get('/api', handler);\n")
            f.flush()
            file_path = Path(f.name)

        try:
            # Without Express framework
            points_without = list(scan_file_for_integrations(file_path))
            express_points = [
                p
                for p in points_without
                if "require" in p.matched_pattern and "express" in p.matched_pattern
            ]
            assert len(express_points) == 0

            # With Express framework
            points_with = list(
                scan_file_for_integrations(file_path, frameworks=["Express"])
            )
            express_points = [
                p
                for p in points_with
                if "require" in p.matched_pattern and "express" in p.matched_pattern
            ]
            assert len(express_points) > 0
        finally:
            file_path.unlink()

    def test_multiple_frameworks_combine_patterns(self) -> None:
        """Multiple active frameworks should combine their patterns."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write(
                "from flask import Flask\n"
                "from django.db import models\n"
                "@app.route('/hello')\n"
                "class User(models.Model):\n"
                "    pass\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(
                scan_file_for_integrations(file_path, frameworks=["Flask", "Django"])
            )
            flask_points = [p for p in points if r"@\w+\.route" in p.matched_pattern]
            django_points = [p for p in points if r"models\.Model" in p.matched_pattern]
            assert len(flask_points) > 0
            assert len(django_points) > 0
        finally:
            file_path.unlink()

    def test_unrecognised_framework_is_ignored(self) -> None:
        """An unrecognised framework name should not cause errors."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write("import requests\nhttp = requests.Session()\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(
                scan_file_for_integrations(
                    file_path, frameworks=["NonExistentFramework"]
                )
            )
            http_points = [
                p for p in points if p.integration_type == IntegrationType.HTTP_REST
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()


class TestGetPatternsForLanguageWithFrameworks:
    """Tests for get_patterns_for_language with framework parameter."""

    def test_no_frameworks_returns_base_only(self) -> None:
        """Without frameworks, should return only common + base patterns."""
        patterns = get_patterns_for_language(Language.PYTHON)
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        # Base pattern should be present
        assert any("import requests" in p for p in http_patterns)
        # Framework-specific pattern should NOT be present
        assert not any("from flask import" in p for p in http_patterns)
        assert not any("from fastapi import" in p for p in http_patterns)

    def test_flask_framework_adds_flask_patterns(self) -> None:
        """With Flask framework, should include Flask-specific patterns."""
        patterns = get_patterns_for_language(Language.PYTHON, frameworks=["Flask"])
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("from flask import" in p for p in http_patterns)
        assert any(r"@\w+\.route" in p for p in http_patterns)
        # Base should still be there
        assert any("import requests" in p for p in http_patterns)

    def test_django_framework_adds_database_patterns(self) -> None:
        """With Django framework, should include Django database patterns."""
        patterns = get_patterns_for_language(Language.PYTHON, frameworks=["Django"])
        db_patterns = [p[0] for p in patterns[IntegrationType.DATABASE]]
        assert any(r"from django\.db import" in p for p in db_patterns)
        assert any(r"models\.Model" in p for p in db_patterns)
        # Base database patterns should still be there
        assert any("from sqlalchemy import" in p for p in db_patterns)

    def test_nestjs_framework_adds_nestjs_patterns(self) -> None:
        """With NestJS framework, should include NestJS-specific patterns."""
        patterns = get_patterns_for_language(Language.TYPESCRIPT, frameworks=["NestJS"])
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any(r"@Controller\(" in p for p in http_patterns)
        assert any(r"@Get\(" in p for p in http_patterns)

    def test_none_language_ignores_frameworks(self) -> None:
        """With None language, frameworks parameter should be ignored."""
        patterns_without = get_patterns_for_language(None)
        patterns_with = get_patterns_for_language(None, frameworks=["Flask"])
        assert patterns_without == patterns_with


class TestDetectIntegrationsWithFrameworks:
    """Tests for detect_integrations with directory_frameworks."""

    def test_framework_patterns_applied_per_directory(self) -> None:
        """Framework-specific patterns should apply based on directory mapping."""
        with tempfile.TemporaryDirectory() as tmpdir:
            py_file = Path(tmpdir) / "app.py"
            py_file.write_text("from flask import Flask\n@app.route('/hello')\n")

            result = detect_integrations(
                tmpdir,
                directory_frameworks={".": ["Flask"]},
            )

            flask_points = [
                p
                for p in result.integration_points
                if p.entity_type == EntityType.FILE_CONTENT
                and (
                    "from flask import" in p.matched_pattern
                    or r"@\w+\.route" in p.matched_pattern
                )
            ]
            assert len(flask_points) > 0

    def test_no_framework_patterns_without_mapping(self) -> None:
        """Without directory_frameworks, framework-specific patterns should not match."""
        with tempfile.TemporaryDirectory() as tmpdir:
            py_file = Path(tmpdir) / "app.py"
            py_file.write_text("from flask import Flask\n@app.route('/hello')\n")

            result = detect_integrations(tmpdir)

            flask_specific_points = [
                p
                for p in result.integration_points
                if p.entity_type == EntityType.FILE_CONTENT
                and (
                    "from flask import" in p.matched_pattern
                    or r"@\w+\.route" in p.matched_pattern
                )
            ]
            assert len(flask_specific_points) == 0

    def test_subdirectory_inherits_parent_frameworks(self) -> None:
        """Files in subdirectories should inherit frameworks from parent directories."""
        with tempfile.TemporaryDirectory() as tmpdir:
            sub_dir = Path(tmpdir) / "api"
            sub_dir.mkdir()
            py_file = sub_dir / "routes.py"
            py_file.write_text("from fastapi import FastAPI\n@app.get('/users')\n")

            result = detect_integrations(
                tmpdir,
                directory_frameworks={".": ["FastAPI"]},
            )

            fastapi_points = [
                p
                for p in result.integration_points
                if p.entity_type == EntityType.FILE_CONTENT
                and (
                    "from fastapi import" in p.matched_pattern
                    or r"@\w+\.get" in p.matched_pattern
                )
            ]
            assert len(fastapi_points) > 0

    def test_different_frameworks_per_directory(self) -> None:
        """Different directories can have different frameworks."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Backend with FastAPI
            backend = Path(tmpdir) / "backend"
            backend.mkdir()
            backend_file = backend / "main.py"
            backend_file.write_text(
                "from fastapi import FastAPI\n@app.get('/users')\ndef get_users(): pass\n"
            )

            # Frontend API with Flask
            frontend = Path(tmpdir) / "frontend"
            frontend.mkdir()
            frontend_file = frontend / "app.py"
            frontend_file.write_text(
                "from flask import Flask\n@app.route('/hello')\ndef hello(): pass\n"
            )

            result = detect_integrations(
                tmpdir,
                directory_frameworks={
                    "backend": ["FastAPI"],
                    "frontend": ["Flask"],
                },
            )

            # FastAPI pattern should match in backend
            fastapi_points = [
                p
                for p in result.integration_points
                if r"@\w+\.get" in p.matched_pattern
            ]
            assert len(fastapi_points) > 0
            assert "backend" in fastapi_points[0].match.file_path

            # Flask route pattern should match in frontend
            flask_points = [
                p
                for p in result.integration_points
                if r"@\w+\.route" in p.matched_pattern
            ]
            assert len(flask_points) > 0
            assert "frontend" in flask_points[0].match.file_path


class TestJavalinFrameworkPatterns:
    """Tests for Javalin framework-specific pattern matching."""

    def test_javalin_patterns_only_with_javalin_framework(self) -> None:
        """Javalin-specific patterns should only match when Javalin framework is active."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                "import io.javalin.Javalin;\n"
                "var app = Javalin.create();\n"
                'app.get("/hello", ctx -> ctx.result("Hello"));\n'
            )
            f.flush()
            file_path = Path(f.name)

        try:
            # Without Javalin framework: should NOT match javalin-specific patterns
            points_without = list(scan_file_for_integrations(file_path))
            javalin_points = [
                p
                for p in points_without
                if "import io\\.javalin" in p.matched_pattern
                or "Javalin\\.create" in p.matched_pattern
            ]
            assert len(javalin_points) == 0

            # With Javalin framework: should match
            points_with = list(
                scan_file_for_integrations(file_path, frameworks=["Javalin"])
            )
            javalin_points = [
                p
                for p in points_with
                if "import io\\.javalin" in p.matched_pattern
                or "Javalin\\.create" in p.matched_pattern
            ]
            assert len(javalin_points) > 0
            assert all(p.confidence == Confidence.HIGH for p in javalin_points)
        finally:
            file_path.unlink()

    def test_javalin_route_patterns(self) -> None:
        """Javalin route handler patterns should match with Javalin framework."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                "import io.javalin.Javalin;\n"
                'app.get("/users", ctx -> ctx.json(users));\n'
                'app.post("/users", ctx -> { });\n'
                'app.delete("/users/{id}", ctx -> { });\n'
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Javalin"]))
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            matched_patterns = {p.matched_pattern for p in http_points}
            assert r"\w*\.get\(" in matched_patterns
            assert r"\w*\.post\(" in matched_patterns
            assert r"\w*\.delete\(" in matched_patterns
        finally:
            file_path.unlink()

    def test_javalin_websocket_patterns(self) -> None:
        """Javalin WebSocket patterns should match with Javalin framework."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("import io.javalin.Javalin;\n" 'app.ws("/websocket", ws -> { });\n')
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Javalin"]))
            ws_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SOCKET
                and r"\w*\.ws\(" in p.matched_pattern
            ]
            assert len(ws_points) > 0
        finally:
            file_path.unlink()

    def test_javalin_method_chain_route_patterns(self) -> None:
        """Method-chain continuations (.get(, .post(, etc.) should match without receiver."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                "import io.javalin.Javalin;\n"
                "Javalin.create(config -> {})\n"
                '    .get("/api/users", ctx -> {})\n'
                '    .post("/api/users", ctx -> {})\n'
                '    .delete("/api/users/{id}", ctx -> {});\n'
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Javalin"]))
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            matched_patterns = {p.matched_pattern for p in http_points}
            assert r"\w*\.get\(" in matched_patterns
            assert r"\w*\.post\(" in matched_patterns
            assert r"\w*\.delete\(" in matched_patterns
        finally:
            file_path.unlink()

    def test_javalin_method_chain_websocket(self) -> None:
        """Method-chain .ws( should match without receiver."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                "import io.javalin.Javalin;\n"
                "Javalin.create(config -> {})\n"
                '    .ws("/websocket", ws -> {});\n'
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Javalin"]))
            ws_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SOCKET
                and r"\w*\.ws\(" in p.matched_pattern
            ]
            assert len(ws_points) > 0
        finally:
            file_path.unlink()

    def test_javalin_base_patterns_still_match(self) -> None:
        """Base Java patterns should still match alongside Javalin patterns."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                "import io.javalin.Javalin;\n"
                "var app = Javalin.create();\n"
                "@Entity\n"
                "public class User { }\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Javalin"]))
            javalin_points = [
                p for p in points if "Javalin\\.create" in p.matched_pattern
            ]
            entity_points = [p for p in points if "@Entity" in p.matched_pattern]
            assert len(javalin_points) > 0
            assert len(entity_points) > 0
        finally:
            file_path.unlink()


class TestImportGating:
    """Tests for import-based gating of framework patterns."""

    def test_javalin_patterns_gated_without_import(self) -> None:
        """Java file with map.get() but no Javalin import should not produce Javalin matches.

        Import gating is now a separate pipeline stage; scan_file_for_integrations
        receives pre-gated frameworks (empty list when gated out).
        """
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                "import java.util.Map;\n"
                "Map<String, String> m = new HashMap<>();\n"
                'm.get("key");\n'
            )
            f.flush()
            file_path = Path(f.name)

        try:
            # Pre-gated: Javalin filtered out by import gating stage
            points = list(scan_file_for_integrations(file_path, frameworks=[]))
            javalin_sourced = [p for p in points if p.source == "Javalin"]
            assert len(javalin_sourced) == 0
        finally:
            file_path.unlink()

    def test_javalin_patterns_applied_with_import(self) -> None:
        """Java file with Javalin import + app.get( should produce Javalin matches."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                "import io.javalin.Javalin;\n"
                'app.get("/hello", ctx -> ctx.result("Hello"));\n'
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Javalin"]))
            javalin_sourced = [p for p in points if p.source == "Javalin"]
            assert len(javalin_sourced) > 0
        finally:
            file_path.unlink()

    def test_spring_ungated_without_import_patterns(self) -> None:
        """Spring patterns should apply regardless of imports (empty import_patterns)."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("@RestController\npublic class MyController { }\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Spring"]))
            spring_sourced = [p for p in points if p.source == "Spring"]
            assert len(spring_sourced) > 0
        finally:
            file_path.unlink()

    def test_express_ts_gated_without_import(self) -> None:
        """TS file with obj.get() but no Express import should not produce Express matches.

        Import gating is now a separate pipeline stage; scan_file_for_integrations
        receives pre-gated frameworks (empty list when gated out).
        """
        with tempfile.NamedTemporaryFile(suffix=".ts", mode="w", delete=False) as f:
            f.write("const val = obj.get('key');\n")
            f.flush()
            file_path = Path(f.name)

        try:
            # Pre-gated: Express filtered out by import gating stage
            points = list(scan_file_for_integrations(file_path, frameworks=[]))
            express_sourced = [p for p in points if p.source == "Express"]
            assert len(express_sourced) == 0
        finally:
            file_path.unlink()

    def test_base_and_common_patterns_always_apply(self) -> None:
        """Base and common patterns should still fire even when framework is gated out.

        Import gating is now a separate pipeline stage; scan_file_for_integrations
        receives pre-gated frameworks (empty list when gated out).
        """
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                "import javax.persistence.Entity;\n"
                "@Entity\n"
                "public class User { }\n"
                'm.get("key");\n'
            )
            f.flush()
            file_path = Path(f.name)

        try:
            # Pre-gated: Javalin filtered out by import gating stage
            points = list(scan_file_for_integrations(file_path, frameworks=[]))
            # No Javalin patterns since frameworks list is pre-gated
            javalin_sourced = [p for p in points if p.source == "Javalin"]
            assert len(javalin_sourced) == 0
            # But base Java patterns (@Entity) should still match
            entity_points = [p for p in points if "@Entity" in p.matched_pattern]
            assert len(entity_points) > 0
        finally:
            file_path.unlink()

    def test_filter_frameworks_by_imports(self) -> None:
        """Unit test for _filter_frameworks_by_imports."""
        content_with_javalin = 'import io.javalin.Javalin;\napp.get("/hello");\n'
        content_without_javalin = 'import java.util.Map;\nm.get("key");\n'

        # With Javalin import: Javalin kept
        result = _filter_frameworks_by_imports(
            content_with_javalin, Language.JAVA, ["Javalin", "Spring"]
        )
        assert "Javalin" in result
        assert "Spring" in result

        # Without Javalin import: Javalin filtered out, Spring kept (ungated)
        result = _filter_frameworks_by_imports(
            content_without_javalin, Language.JAVA, ["Javalin", "Spring"]
        )
        assert "Javalin" not in result
        assert "Spring" in result

    def test_file_has_framework_import(self) -> None:
        """Unit test for _file_has_framework_import."""
        content = "import io.javalin.Javalin;\npublic class App {}\n"
        assert _file_has_framework_import(content, (r"import io\.javalin",))
        assert not _file_has_framework_import(content, (r"import io\.quarkus",))
        assert not _file_has_framework_import(
            "public class App {}", (r"import io\.javalin",)
        )

    def test_build_import_gated_framework_map(self, tmp_path: Path) -> None:
        """_build_import_gated_framework_map gates frameworks by file imports."""
        repo = tmp_path / "repo"
        repo.mkdir()

        # File with Javalin import — Javalin should survive gating
        gated_file = repo / "WithJavalin.java"
        gated_file.write_text(
            "import io.javalin.Javalin;\n"
            'app.get("/hello", ctx -> ctx.result("Hello"));\n'
        )

        # File without Javalin import — Javalin should be gated out
        ungated_file = repo / "WithoutJavalin.java"
        ungated_file.write_text("import java.util.Map;\n" 'm.get("key");\n')

        directory_frameworks = {".": ["Javalin", "Spring"]}

        result = _build_import_gated_framework_map(
            iter([gated_file, ungated_file]),
            repo,
            directory_frameworks,
        )

        # Javalin + Spring both survive for file with Javalin import
        assert "Javalin" in result[gated_file]
        assert "Spring" in result[gated_file]

        # Only Spring survives for file without Javalin import
        assert "Javalin" not in result[ungated_file]
        assert "Spring" in result[ungated_file]

    def test_build_import_gated_framework_map_unreadable_file(
        self, tmp_path: Path
    ) -> None:
        """Files that cannot be read are excluded from the map."""
        repo = tmp_path / "repo"
        repo.mkdir()

        missing_file = repo / "Missing.java"

        result = _build_import_gated_framework_map(
            iter([missing_file]),
            repo,
            {".": ["Spring"]},
        )

        assert missing_file not in result


class TestExpressMethodChainPatterns:
    """Tests for Express method-chain continuation matching."""

    def test_js_express_method_chain(self) -> None:
        """JS Express method-chain .get(, .post( should match without receiver."""
        with tempfile.NamedTemporaryFile(suffix=".js", mode="w", delete=False) as f:
            f.write(
                "const express = require('express');\n"
                "const app = express()\n"
                '    .get("/users", handler)\n'
                '    .post("/users", handler);\n'
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Express"]))
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            matched_patterns = {p.matched_pattern for p in http_points}
            assert r"\w*\.get\(" in matched_patterns
            assert r"\w*\.post\(" in matched_patterns
        finally:
            file_path.unlink()

    def test_ts_express_method_chain(self) -> None:
        """TS Express method-chain .get(, .put( should match without receiver."""
        with tempfile.NamedTemporaryFile(suffix=".ts", mode="w", delete=False) as f:
            f.write(
                "import express from 'express';\n"
                "const app = express()\n"
                '    .get("/users", handler)\n'
                '    .put("/users", handler);\n'
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Express"]))
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            matched_patterns = {p.matched_pattern for p in http_points}
            assert r"\w*\.get\(" in matched_patterns
            assert r"\w*\.put\(" in matched_patterns
        finally:
            file_path.unlink()


class TestFileIoPatterns:
    """Tests for FILE_IO integration type detection."""

    def test_java_file_input_stream(self) -> None:
        """Should detect FileInputStream as FILE_IO."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("FileInputStream fis = new FileInputStream(path);\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            file_io_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.FILE_IO
                and p.confidence == Confidence.HIGH
            ]
            assert len(file_io_points) > 0
        finally:
            file_path.unlink()

    def test_python_open(self) -> None:
        """Should detect open() as FILE_IO."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write("with open('data.txt') as f:\n    data = f.read()\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            file_io_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.FILE_IO
                and "open\\(" in p.matched_pattern
            ]
            assert len(file_io_points) > 0
        finally:
            file_path.unlink()

    def test_python_pathlib(self) -> None:
        """Should detect pathlib.Path as FILE_IO."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write("p = pathlib.Path('/tmp/data.txt')\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            file_io_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.FILE_IO
                and "pathlib" in p.matched_pattern
            ]
            assert len(file_io_points) > 0
        finally:
            file_path.unlink()

    def test_cobol_open_input(self) -> None:
        """Should detect OPEN INPUT as FILE_IO in COBOL."""
        with tempfile.NamedTemporaryFile(suffix=".cbl", mode="w", delete=False) as f:
            f.write("       OPEN INPUT CUSTOMER-FILE.\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            file_io_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.FILE_IO
                and p.confidence == Confidence.HIGH
            ]
            assert len(file_io_points) > 0
        finally:
            file_path.unlink()

    def test_cobol_open_output(self) -> None:
        """Should detect OPEN OUTPUT as FILE_IO in COBOL."""
        with tempfile.NamedTemporaryFile(suffix=".cbl", mode="w", delete=False) as f:
            f.write("       OPEN OUTPUT REPORT-FILE.\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            file_io_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.FILE_IO
                and p.confidence == Confidence.HIGH
            ]
            assert len(file_io_points) > 0
        finally:
            file_path.unlink()

    def test_go_os_open(self) -> None:
        """Should detect os.Open as FILE_IO in Go."""
        with tempfile.NamedTemporaryFile(suffix=".go", mode="w", delete=False) as f:
            f.write('f, err := os.Open("data.txt")\n')
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            file_io_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.FILE_IO
                and p.confidence == Confidence.HIGH
            ]
            assert len(file_io_points) > 0
        finally:
            file_path.unlink()

    def test_pli_open_file(self) -> None:
        """Should detect OPEN FILE as FILE_IO in PL/I."""
        with tempfile.NamedTemporaryFile(suffix=".pli", mode="w", delete=False) as f:
            f.write("OPEN FILE(CUSTFILE) INPUT;\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            file_io_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.FILE_IO
                and p.confidence == Confidence.HIGH
            ]
            assert len(file_io_points) > 0
        finally:
            file_path.unlink()


class TestGrpcPatterns:
    """Tests for GRPC integration type detection."""

    def test_java_grpc_import(self) -> None:
        """Should detect io.grpc usage as GRPC in Java."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                "import io.grpc.ManagedChannel;\n"
                "ManagedChannel channel = ManagedChannelBuilder.forTarget(target).build();\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            grpc_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.GRPC
                and p.confidence == Confidence.HIGH
            ]
            assert len(grpc_points) > 0
        finally:
            file_path.unlink()

    def test_python_grpc_import(self) -> None:
        """Should detect grpc.server as GRPC in Python."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write("import grpc\nserver = grpc.server(futures.ThreadPoolExecutor())\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            grpc_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.GRPC
                and p.confidence == Confidence.HIGH
            ]
            assert len(grpc_points) > 0
        finally:
            file_path.unlink()

    def test_go_grpc_import(self) -> None:
        """Should detect gRPC usage as GRPC in Go."""
        with tempfile.NamedTemporaryFile(suffix=".go", mode="w", delete=False) as f:
            f.write(
                'import "google.golang.org/grpc"\n'
                "pb.RegisterGreeterServer(s, &server{})\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            grpc_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.GRPC
                and p.confidence == Confidence.HIGH
            ]
            assert len(grpc_points) > 0
        finally:
            file_path.unlink()

    def test_csharp_grpc_core(self) -> None:
        """Should detect Grpc.Core usage as GRPC in C#."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write(
                "using Grpc.Core;\n"
                "ServerServiceDefinition service = GreeterService.BindService(new Greeter());\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            grpc_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.GRPC
                and p.confidence == Confidence.HIGH
            ]
            assert len(grpc_points) > 0
        finally:
            file_path.unlink()

    def test_rust_tonic_grpc(self) -> None:
        """Should detect tonic:: usage as GRPC in Rust."""
        with tempfile.NamedTemporaryFile(suffix=".rs", mode="w", delete=False) as f:
            f.write(
                "use tonic::transport::Server;\n"
                "#[tonic::async_trait]\nimpl Greeter for MyGreeter {}\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            grpc_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.GRPC
                and p.confidence == Confidence.HIGH
            ]
            assert len(grpc_points) > 0
        finally:
            file_path.unlink()


class TestDropwizardFrameworkPatterns:
    """Tests for Dropwizard framework-specific pattern matching."""

    def test_dropwizard_http_rest_patterns(self) -> None:
        """Should detect Dropwizard HTTP/REST patterns."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                "import io.dropwizard.Application;\n"
                '@Path("/users")\n'
                "@Produces(MediaType.APPLICATION_JSON)\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(
                scan_file_for_integrations(file_path, frameworks=["Dropwizard"])
            )
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            assert len(http_points) > 0
            matched = {p.matched_pattern for p in http_points}
            assert r"@Path\(" in matched or r"@Produces" in matched
        finally:
            file_path.unlink()

    def test_dropwizard_patterns_only_with_framework(self) -> None:
        """Dropwizard patterns should not match without framework."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("import io.dropwizard.Application;\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            dropwizard_points = [
                p for p in points if "import io\\.dropwizard" in p.matched_pattern
            ]
            assert len(dropwizard_points) == 0
        finally:
            file_path.unlink()


class TestVertxFrameworkPatterns:
    """Tests for Vert.x framework-specific pattern matching."""

    def test_vertx_http_rest_patterns(self) -> None:
        """Should detect Vert.x HTTP/REST patterns."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                "import io.vertx.core.Vertx;\n"
                "vertx.createHttpServer();\n"
                "Router.router(vertx);\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Vert.x"]))
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()

    def test_vertx_socket_patterns(self) -> None:
        """Should detect Vert.x WebSocket patterns."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("ServerWebSocket ws = conn.webSocket();\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Vert.x"]))
            socket_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SOCKET
                and "ServerWebSocket" in p.matched_pattern
            ]
            assert len(socket_points) > 0
        finally:
            file_path.unlink()

    def test_vertx_messaging_patterns(self) -> None:
        """Should detect Vert.x EventBus patterns."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("EventBus eb = vertx.eventBus();\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Vert.x"]))
            msg_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.MESSAGING
                and p.confidence == Confidence.HIGH
            ]
            assert len(msg_points) > 0
        finally:
            file_path.unlink()


class TestCsharpFrameworkPatterns:
    """Tests for C# framework-specific pattern matching."""

    def test_aspnet_core_http_rest(self) -> None:
        """Should detect ASP.NET Core HTTP/REST patterns."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write(
                "var builder = WebApplication.CreateBuilder(args);\n"
                'app.MapGet("/hello", () => "Hello World");\n'
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(
                scan_file_for_integrations(file_path, frameworks=["ASP.NET Core"])
            )
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()

    def test_aspnet_core_grpc(self) -> None:
        """Should detect ASP.NET Core gRPC patterns."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write("app.MapGrpcService<GreeterService>();\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(
                scan_file_for_integrations(file_path, frameworks=["ASP.NET Core"])
            )
            grpc_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.GRPC
                and "MapGrpcService" in p.matched_pattern
            ]
            assert len(grpc_points) > 0
        finally:
            file_path.unlink()

    def test_wcf_soap(self) -> None:
        """Should detect WCF SOAP patterns."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write(
                "using System.ServiceModel;\n"
                "var host = new ServiceHost(typeof(MyService));\n"
                "var binding = new BasicHttpBinding();\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["WCF"]))
            soap_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SOAP
                and p.confidence == Confidence.HIGH
            ]
            assert len(soap_points) > 0
        finally:
            file_path.unlink()

    def test_corewcf_soap(self) -> None:
        """Should detect CoreWCF SOAP patterns."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write(
                "using CoreWCF;\n"
                "using CoreWCF.Http;\n"
                "CoreWCF.Http.BasicHttpBinding binding = new();\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["CoreWCF"]))
            soap_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SOAP
                and p.confidence == Confidence.HIGH
            ]
            assert len(soap_points) > 0
        finally:
            file_path.unlink()

    def test_servicestack_http_rest(self) -> None:
        """Should detect ServiceStack HTTP/REST patterns."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write(
                "using ServiceStack;\npublic class MyRequest : IReturn<MyResponse> {}\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(
                scan_file_for_integrations(file_path, frameworks=["ServiceStack"])
            )
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()

    def test_nancy_http_rest(self) -> None:
        """Should detect Nancy HTTP/REST patterns."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write(
                'public class MyModule : NancyModule {\n    Get["/"] = _ => "Hello";\n}\n'
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Nancy"]))
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()

    def test_carter_http_rest(self) -> None:
        """Should detect Carter HTTP/REST patterns."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write("public class MyModule : ICarterModule {}\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Carter"]))
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and "ICarterModule" in p.matched_pattern
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()


class TestGoSoapPatterns:
    """Tests for Go SOAP patterns (gap fill)."""

    def test_go_gowsdl(self) -> None:
        """Should detect gowsdl SOAP usage in Go."""
        with tempfile.NamedTemporaryFile(suffix=".go", mode="w", delete=False) as f:
            f.write(
                'import "github.com/hooklift/gowsdl"\n'
                "client := newSoapClient(wsdl)\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            soap_points = [
                p for p in points if p.integration_type == IntegrationType.SOAP
            ]
            assert len(soap_points) > 0
        finally:
            file_path.unlink()

    def test_go_encoding_xml(self) -> None:
        """Should detect encoding/xml SOAP usage in Go."""
        with tempfile.NamedTemporaryFile(suffix=".go", mode="w", delete=False) as f:
            f.write('import "encoding/xml"\n' "var soap = NewEnvelopeBuilder(data)\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            soap_points = [
                p for p in points if p.integration_type == IntegrationType.SOAP
            ]
            assert len(soap_points) > 0
        finally:
            file_path.unlink()


class TestRustSoapPatterns:
    """Tests for Rust SOAP patterns (gap fill)."""

    def test_rust_yaserde(self) -> None:
        """Should detect yaserde SOAP usage in Rust."""
        with tempfile.NamedTemporaryFile(suffix=".rs", mode="w", delete=False) as f:
            f.write(
                "use yaserde::YaDeserialize;\n"
                "let envelope = parse_soap_envelope(xml);\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            soap_points = [
                p for p in points if p.integration_type == IntegrationType.SOAP
            ]
            assert len(soap_points) > 0
        finally:
            file_path.unlink()

    def test_rust_quick_xml(self) -> None:
        """Should detect quick-xml SOAP usage in Rust."""
        with tempfile.NamedTemporaryFile(suffix=".rs", mode="w", delete=False) as f:
            f.write("use quick_xml::Reader;\n" "let wsdl = Reader::from_str(data);\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            soap_points = [
                p for p in points if p.integration_type == IntegrationType.SOAP
            ]
            assert len(soap_points) > 0
        finally:
            file_path.unlink()


class TestDirectoryClassificationNewTypes:
    """Tests for directory classification with FILE_IO and GRPC types."""

    def test_classify_uploads_directory(self) -> None:
        """Should classify 'uploads' as FILE_IO."""
        matches = classify_directory("uploads")
        types = [m[0] for m in matches]
        assert IntegrationType.FILE_IO in types

    def test_classify_storage_directory(self) -> None:
        """Should classify 'storage' as FILE_IO."""
        matches = classify_directory("storage")
        types = [m[0] for m in matches]
        assert IntegrationType.FILE_IO in types

    def test_classify_proto_directory(self) -> None:
        """Should classify 'proto' as GRPC."""
        matches = classify_directory("proto")
        types = [m[0] for m in matches]
        assert IntegrationType.GRPC in types

    def test_classify_grpc_directory(self) -> None:
        """Should classify 'grpc' as GRPC."""
        matches = classify_directory("grpc")
        types = [m[0] for m in matches]
        assert IntegrationType.GRPC in types


class TestIntegrationDetectorResultJson:
    """Tests for IntegrationDetectorResult.to_json()."""

    def test_to_json_returns_valid_json(self) -> None:
        """to_json() should return parseable JSON with correct structure."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("@Entity\npublic class User {}\n")
            f.flush()
            file_path = Path(f.name)

        try:
            result = detect_integrations(str(file_path.parent))
            parsed = json.loads(result.to_json())

            assert "files_scanned" in parsed
            assert "integration_points" in parsed
            assert isinstance(parsed["files_scanned"], int)
            assert isinstance(parsed["integration_points"], list)
        finally:
            file_path.unlink()

    def test_to_json_point_fields(self) -> None:
        """Each integration point should have all expected fields."""
        match = FileMatch(
            file_path="/test.java",
            line_number=5,
            line_content="@RestController",
            language=Language.JAVA,
        )
        signal = IntegrationSignal(
            match=match,
            integration_type=IntegrationType.HTTP_REST,
            confidence=Confidence.HIGH,
            matched_pattern="@RestController",
            entity_type=EntityType.FILE_CONTENT,
            source="Java",
        )
        result = IntegrationDetectorResult(
            integration_points=[signal],
            files_scanned=1,
        )
        parsed = json.loads(result.to_json())

        point = parsed["integration_points"][0]
        assert point["integration_type"] == "http_rest"
        assert point["confidence"] == "high"
        assert point["matched_pattern"] == "@RestController"
        assert point["entity_type"] == "file_content"
        assert point["source"] == "Java"
        assert point["match"]["file_path"] == "/test.java"
        assert point["match"]["line_number"] == 5
        assert point["match"]["line_content"] == "@RestController"
        assert point["match"]["language"] == "Java"

    def test_to_json_empty_result(self) -> None:
        """to_json() should handle an empty result."""
        result = IntegrationDetectorResult(
            integration_points=[],
            files_scanned=0,
        )
        parsed = json.loads(result.to_json())

        assert parsed["files_scanned"] == 0
        assert parsed["integration_points"] == []

    def test_to_json_respects_indent(self) -> None:
        """to_json() should respect the indent parameter."""
        result = IntegrationDetectorResult(
            integration_points=[],
            files_scanned=0,
        )
        compact = result.to_json(indent=None)
        indented = result.to_json(indent=4)

        assert "\n" not in compact
        assert "\n" in indented

    def test_to_json_none_language(self) -> None:
        """to_json() should handle None language gracefully."""
        match = FileMatch(
            file_path="/unknown.xyz",
            line_number=1,
            line_content="http endpoint",
            language=None,
        )
        signal = IntegrationSignal(
            match=match,
            integration_type=IntegrationType.HTTP_REST,
            confidence=Confidence.LOW,
            matched_pattern="(?i)\\bhttp\\b",
            entity_type=EntityType.FILE_CONTENT,
            source="common",
        )
        result = IntegrationDetectorResult(
            integration_points=[signal],
            files_scanned=1,
        )
        parsed = json.loads(result.to_json())

        assert parsed["integration_points"][0]["match"]["language"] is None


class TestGraphqlPatterns:
    """Tests for GRAPHQL integration type detection."""

    def test_java_graphql_import(self) -> None:
        """Should detect GraphQLSchema usage as GRAPHQL in Java."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                "import graphql.schema.GraphQLSchema;\n"
                "GraphQLSchema schema = GraphQLSchema.newSchema().build();\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            graphql_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.GRAPHQL
                and p.confidence == Confidence.HIGH
            ]
            assert len(graphql_points) > 0
        finally:
            file_path.unlink()

    def test_python_graphene(self) -> None:
        """Should detect graphene as GRAPHQL in Python."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write(
                "import graphene\n"
                "class Query(graphene.ObjectType):\n"
                "    name = graphene.String()  # graphql field\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            graphql_points = [
                p for p in points if p.integration_type == IntegrationType.GRAPHQL
            ]
            assert len(graphql_points) > 0
        finally:
            file_path.unlink()

    def test_typescript_apollo_server(self) -> None:
        """Should detect apollo-server as GRAPHQL in TypeScript."""
        with tempfile.NamedTemporaryFile(suffix=".ts", mode="w", delete=False) as f:
            f.write(
                "import { ApolloServer } from 'apollo-server';\n"
                "const server = new ApolloServer({ typeDefs, resolvers });\n"
                "const graphql_endpoint = '/graphql';\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            graphql_points = [
                p for p in points if p.integration_type == IntegrationType.GRAPHQL
            ]
            assert len(graphql_points) > 0
        finally:
            file_path.unlink()

    def test_go_gqlgen(self) -> None:
        """Should detect gqlgen as GRAPHQL in Go."""
        with tempfile.NamedTemporaryFile(suffix=".go", mode="w", delete=False) as f:
            f.write(
                'import "github.com/99designs/gqlgen"\n'
                "srv := handler.NewDefaultServer(graphql.NewExecutableSchema(cfg))\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            graphql_points = [
                p for p in points if p.integration_type == IntegrationType.GRAPHQL
            ]
            assert len(graphql_points) > 0
        finally:
            file_path.unlink()

    def test_rust_juniper(self) -> None:
        """Should detect juniper as GRAPHQL in Rust."""
        with tempfile.NamedTemporaryFile(suffix=".rs", mode="w", delete=False) as f:
            f.write(
                "use juniper::GraphQLObject;\n"
                "let graphql = RootNode::new(query, mutation, subscription);\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            graphql_points = [
                p for p in points if p.integration_type == IntegrationType.GRAPHQL
            ]
            assert len(graphql_points) > 0
        finally:
            file_path.unlink()

    def test_csharp_hotchocolate(self) -> None:
        """Should detect HotChocolate as GRAPHQL in C#."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write(
                "using HotChocolate;\n"
                "services.AddHotChocolateServer().AddQueryType<Query>();\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            graphql_points = [
                p for p in points if p.integration_type == IntegrationType.GRAPHQL
            ]
            assert len(graphql_points) > 0
        finally:
            file_path.unlink()

    def test_spring_query_mapping(self) -> None:
        """Should detect @QueryMapping as GRAPHQL with Spring framework."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("@QueryMapping\npublic Book bookById(@Argument String id) {}\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Spring"]))
            graphql_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.GRAPHQL
                and p.confidence == Confidence.HIGH
            ]
            assert len(graphql_points) > 0
        finally:
            file_path.unlink()

    def test_nestjs_resolver(self) -> None:
        """Should detect @Resolver as GRAPHQL with NestJS framework."""
        with tempfile.NamedTemporaryFile(suffix=".ts", mode="w", delete=False) as f:
            f.write("@Resolver()\nexport class CatsResolver {}\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["NestJS"]))
            graphql_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.GRAPHQL
                and p.confidence == Confidence.HIGH
            ]
            assert len(graphql_points) > 0
        finally:
            file_path.unlink()


class TestEmailPatterns:
    """Tests for EMAIL integration type detection."""

    def test_java_javax_mail(self) -> None:
        """Should detect javax.mail as EMAIL in Java."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                "import javax.mail.Session;\n"
                "Session session = javax.mail.Session.getDefaultInstance(props);\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            email_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.EMAIL
                and p.confidence == Confidence.HIGH
            ]
            assert len(email_points) > 0
        finally:
            file_path.unlink()

    def test_python_smtplib(self) -> None:
        """Should detect smtplib as EMAIL in Python."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write(
                "import smtplib\n" "server = smtplib.SMTP('smtp.example.com', 587)\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            email_points = [
                p for p in points if p.integration_type == IntegrationType.EMAIL
            ]
            assert len(email_points) > 0
        finally:
            file_path.unlink()

    def test_typescript_nodemailer(self) -> None:
        """Should detect nodemailer as EMAIL in TypeScript."""
        with tempfile.NamedTemporaryFile(suffix=".ts", mode="w", delete=False) as f:
            f.write(
                "import nodemailer from 'nodemailer';\n"
                "const transporter = nodemailer.createTransport({ service: 'smtp' });\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            email_points = [
                p for p in points if p.integration_type == IntegrationType.EMAIL
            ]
            assert len(email_points) > 0
        finally:
            file_path.unlink()

    def test_go_net_smtp(self) -> None:
        """Should detect smtp usage as EMAIL in Go."""
        with tempfile.NamedTemporaryFile(suffix=".go", mode="w", delete=False) as f:
            f.write(
                'import "net/smtp"\n'
                'auth := smtp.PlainAuth("", user, password, host)\n'
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            email_points = [
                p for p in points if p.integration_type == IntegrationType.EMAIL
            ]
            assert len(email_points) > 0
        finally:
            file_path.unlink()

    def test_rust_lettre(self) -> None:
        """Should detect lettre SmtpTransport as EMAIL in Rust."""
        with tempfile.NamedTemporaryFile(suffix=".rs", mode="w", delete=False) as f:
            f.write(
                "use lettre::SmtpTransport;\n"
                "let transport = SmtpTransport::relay(smtp_server)?.build();\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            email_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.EMAIL
                and p.confidence == Confidence.HIGH
            ]
            assert len(email_points) > 0
        finally:
            file_path.unlink()

    def test_csharp_system_net_mail(self) -> None:
        """Should detect SmtpClient as EMAIL in C#."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write(
                "using System.Net.Mail;\n"
                "SmtpClient client = new SmtpClient(host, port);\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            email_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.EMAIL
                and p.confidence == Confidence.HIGH
            ]
            assert len(email_points) > 0
        finally:
            file_path.unlink()

    def test_django_send_mail(self) -> None:
        """Should detect django send_mail as EMAIL with Django framework."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write(
                "from django.core.mail import send_mail\n"
                "send_mail('Subject', 'Body', 'from@example.com', ['to@example.com'])\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Django"]))
            email_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.EMAIL
                and p.confidence == Confidence.HIGH
            ]
            assert len(email_points) > 0
        finally:
            file_path.unlink()


class TestCachingPatterns:
    """Tests for CACHING integration type detection."""

    def test_java_jedis(self) -> None:
        """Should detect jedis redis usage as CACHING in Java."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                "import redis.clients.jedis.Jedis;\n"
                "Jedis redis = new Jedis(host, port);\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            cache_points = [
                p for p in points if p.integration_type == IntegrationType.CACHING
            ]
            assert len(cache_points) > 0
        finally:
            file_path.unlink()

    def test_python_redis(self) -> None:
        """Should detect redis as CACHING in Python."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write("import redis\n" "client = redis.Redis(host=host, port=port)\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            cache_points = [
                p for p in points if p.integration_type == IntegrationType.CACHING
            ]
            assert len(cache_points) > 0
        finally:
            file_path.unlink()

    def test_typescript_ioredis(self) -> None:
        """Should detect ioredis as CACHING in TypeScript."""
        with tempfile.NamedTemporaryFile(suffix=".ts", mode="w", delete=False) as f:
            f.write(
                "import Redis from 'ioredis';\n"
                "const redis = new Redis({ host: redisHost });\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            cache_points = [
                p for p in points if p.integration_type == IntegrationType.CACHING
            ]
            assert len(cache_points) > 0
        finally:
            file_path.unlink()

    def test_go_redis(self) -> None:
        """Should detect go-redis as CACHING in Go."""
        with tempfile.NamedTemporaryFile(suffix=".go", mode="w", delete=False) as f:
            f.write(
                'import "github.com/go-redis/redis"\n'
                "client := redis.NewClient(&redis.Options{})\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            cache_points = [
                p for p in points if p.integration_type == IntegrationType.CACHING
            ]
            assert len(cache_points) > 0
        finally:
            file_path.unlink()

    def test_rust_redis(self) -> None:
        """Should detect redis as CACHING in Rust."""
        with tempfile.NamedTemporaryFile(suffix=".rs", mode="w", delete=False) as f:
            f.write(
                "use redis::Client;\n" "let redis = Client::open(connection_url)?;\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            cache_points = [
                p for p in points if p.integration_type == IntegrationType.CACHING
            ]
            assert len(cache_points) > 0
        finally:
            file_path.unlink()

    def test_csharp_idistributed_cache(self) -> None:
        """Should detect IDistributedCache as CACHING in C#."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write("private readonly IDistributedCache _cache;\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            cache_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.CACHING
                and p.confidence == Confidence.HIGH
            ]
            assert len(cache_points) > 0
        finally:
            file_path.unlink()

    def test_spring_cacheable(self) -> None:
        """Should detect @Cacheable as CACHING with Spring framework."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write('@Cacheable("users")\npublic User getUser(Long id) {}\n')
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Spring"]))
            cache_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.CACHING
                and p.confidence == Confidence.HIGH
            ]
            assert len(cache_points) > 0
        finally:
            file_path.unlink()


class TestSseStreamingPatterns:
    """Tests for SSE_STREAMING integration type detection."""

    def test_java_sse_emitter(self) -> None:
        """Should detect SseEmitter as SSE_STREAMING in Java."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("SseEmitter emitter = new SseEmitter();\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            sse_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SSE_STREAMING
                and p.confidence == Confidence.HIGH
            ]
            assert len(sse_points) > 0
        finally:
            file_path.unlink()

    def test_go_http_flusher(self) -> None:
        """Should detect http.Flusher as SSE_STREAMING in Go."""
        with tempfile.NamedTemporaryFile(suffix=".go", mode="w", delete=False) as f:
            f.write("flusher, ok := w.(http.Flusher)\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            sse_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SSE_STREAMING
                and p.confidence == Confidence.HIGH
            ]
            assert len(sse_points) > 0
        finally:
            file_path.unlink()

    def test_rust_sse(self) -> None:
        """Should detect Sse< as SSE_STREAMING in Rust."""
        with tempfile.NamedTemporaryFile(suffix=".rs", mode="w", delete=False) as f:
            f.write("async fn handler() -> Sse<impl Stream> {}\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            sse_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SSE_STREAMING
                and p.confidence == Confidence.HIGH
            ]
            assert len(sse_points) > 0
        finally:
            file_path.unlink()

    def test_csharp_iasync_enumerable(self) -> None:
        """Should detect IAsyncEnumerable as SSE_STREAMING in C#."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write("public async IAsyncEnumerable<int> GetStream() {}\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            sse_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SSE_STREAMING
                and "IAsyncEnumerable" in p.matched_pattern
            ]
            assert len(sse_points) > 0
        finally:
            file_path.unlink()

    def test_fastapi_streaming_response(self) -> None:
        """Should detect StreamingResponse as SSE_STREAMING with FastAPI."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write("return StreamingResponse(generate())\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["FastAPI"]))
            sse_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SSE_STREAMING
                and p.confidence == Confidence.HIGH
            ]
            assert len(sse_points) > 0
        finally:
            file_path.unlink()

    def test_axum_sse(self) -> None:
        """Should detect axum::response::sse as SSE_STREAMING with Axum."""
        with tempfile.NamedTemporaryFile(suffix=".rs", mode="w", delete=False) as f:
            f.write(
                "use axum::response::sse::Event;\n"
                "async fn handler() -> Sse<impl Stream> { todo!() }\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Axum"]))
            sse_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SSE_STREAMING
                and p.confidence == Confidence.HIGH
            ]
            assert len(sse_points) > 0
        finally:
            file_path.unlink()


class TestSchedulingPatterns:
    """Tests for SCHEDULING integration type detection."""

    def test_java_quartz(self) -> None:
        """Should detect Quartz as SCHEDULING in Java."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                "import org.quartz.Scheduler;\n"
                "ScheduledExecutorService executor = Executors.newScheduledThreadPool(1);\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            sched_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SCHEDULING
                and p.confidence == Confidence.HIGH
            ]
            assert len(sched_points) > 0
        finally:
            file_path.unlink()

    def test_python_apscheduler(self) -> None:
        """Should detect APScheduler as SCHEDULING in Python."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write(
                "from apscheduler.schedulers.background import BackgroundScheduler\n"
                "scheduler = BackgroundScheduler()\n"
                "scheduler.add_job(my_job, 'cron', hour=12)\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            sched_points = [
                p for p in points if p.integration_type == IntegrationType.SCHEDULING
            ]
            assert len(sched_points) > 0
        finally:
            file_path.unlink()

    def test_typescript_node_cron(self) -> None:
        """Should detect node-cron as SCHEDULING in TypeScript."""
        with tempfile.NamedTemporaryFile(suffix=".ts", mode="w", delete=False) as f:
            f.write(
                "import cron from 'node-cron';\n"
                "const job = cron.schedule('* * * * *', () => { runTask(); });\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            sched_points = [
                p for p in points if p.integration_type == IntegrationType.SCHEDULING
            ]
            assert len(sched_points) > 0
        finally:
            file_path.unlink()

    def test_go_robfig_cron(self) -> None:
        """Should detect robfig/cron as SCHEDULING in Go."""
        with tempfile.NamedTemporaryFile(suffix=".go", mode="w", delete=False) as f:
            f.write('import "github.com/robfig/cron"\n' "c := cron.New()\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            sched_points = [
                p for p in points if p.integration_type == IntegrationType.SCHEDULING
            ]
            assert len(sched_points) > 0
        finally:
            file_path.unlink()

    def test_rust_tokio_cron_scheduler(self) -> None:
        """Should detect tokio_cron_scheduler as SCHEDULING in Rust."""
        with tempfile.NamedTemporaryFile(suffix=".rs", mode="w", delete=False) as f:
            f.write(
                "use tokio_cron_scheduler::JobScheduler;\n"
                "let mut scheduler = JobScheduler::new().await?;\n"
                "scheduler.add(Job::new_async(cron, callback)?);\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            sched_points = [
                p for p in points if p.integration_type == IntegrationType.SCHEDULING
            ]
            assert len(sched_points) > 0
        finally:
            file_path.unlink()

    def test_csharp_hangfire(self) -> None:
        """Should detect Hangfire as SCHEDULING in C#."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write(
                "using Hangfire;\n"
                "RecurringJob.AddOrUpdate(() => DoWork(), Cron.Daily);\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            sched_points = [
                p for p in points if p.integration_type == IntegrationType.SCHEDULING
            ]
            assert len(sched_points) > 0
        finally:
            file_path.unlink()

    def test_spring_scheduled(self) -> None:
        """Should detect @Scheduled as SCHEDULING with Spring framework."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("@Scheduled(fixedRate = 5000)\npublic void doWork() {}\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Spring"]))
            sched_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SCHEDULING
                and p.confidence == Confidence.HIGH
            ]
            assert len(sched_points) > 0
        finally:
            file_path.unlink()

    def test_nestjs_cron(self) -> None:
        """Should detect @Cron as SCHEDULING with NestJS framework."""
        with tempfile.NamedTemporaryFile(suffix=".ts", mode="w", delete=False) as f:
            f.write("@Cron('45 * * * * *')\nhandleCron() {}\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["NestJS"]))
            sched_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SCHEDULING
                and p.confidence == Confidence.HIGH
            ]
            assert len(sched_points) > 0
        finally:
            file_path.unlink()


class TestDirectoryClassificationNewIntegrationTypes:
    """Tests for directory classification with new integration types."""

    def test_classify_graphql_directory(self) -> None:
        """Should classify 'graphql' as GRAPHQL."""
        matches = classify_directory("graphql")
        types = [m[0] for m in matches]
        assert IntegrationType.GRAPHQL in types

    def test_classify_email_directory(self) -> None:
        """Should classify 'email' as EMAIL."""
        matches = classify_directory("email")
        types = [m[0] for m in matches]
        assert IntegrationType.EMAIL in types

    def test_classify_mail_directory(self) -> None:
        """Should classify 'mail' as EMAIL."""
        matches = classify_directory("mail")
        types = [m[0] for m in matches]
        assert IntegrationType.EMAIL in types

    def test_classify_notifications_directory(self) -> None:
        """Should classify 'notifications' as EMAIL."""
        matches = classify_directory("notifications")
        types = [m[0] for m in matches]
        assert IntegrationType.EMAIL in types

    def test_classify_cache_directory(self) -> None:
        """Should classify 'cache' as CACHING."""
        matches = classify_directory("cache")
        types = [m[0] for m in matches]
        assert IntegrationType.CACHING in types

    def test_classify_streaming_directory(self) -> None:
        """Should classify 'streaming' as SSE_STREAMING."""
        matches = classify_directory("streaming")
        types = [m[0] for m in matches]
        assert IntegrationType.SSE_STREAMING in types

    def test_classify_sse_directory(self) -> None:
        """Should classify 'sse' as SSE_STREAMING."""
        matches = classify_directory("sse")
        types = [m[0] for m in matches]
        assert IntegrationType.SSE_STREAMING in types

    def test_classify_scheduler_directory(self) -> None:
        """Should classify 'scheduler' as SCHEDULING."""
        matches = classify_directory("scheduler")
        types = [m[0] for m in matches]
        assert IntegrationType.SCHEDULING in types

    def test_classify_jobs_directory(self) -> None:
        """Should classify 'jobs' as SCHEDULING."""
        matches = classify_directory("jobs")
        types = [m[0] for m in matches]
        assert IntegrationType.SCHEDULING in types


class TestNewFrameworkPatterns:
    """Tests for newly added framework pattern files."""

    def test_sanic_http_rest(self) -> None:
        """Should detect Sanic HTTP/REST patterns."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write("from sanic import Sanic\napp = Sanic('app')\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Sanic"]))
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()

    def test_litestar_http_rest(self) -> None:
        """Should detect Litestar HTTP/REST patterns."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write(
                "from litestar import Litestar, get\n"
                "@get('/users')\n"
                "async def list_users() -> list: pass\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(
                scan_file_for_integrations(file_path, frameworks=["Litestar"])
            )
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()

    def test_helidon_http_rest(self) -> None:
        """Should detect Helidon HTTP/REST patterns."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                "import io.helidon.webserver.WebServer;\n"
                "WebServer.builder().routing(HttpRules.builder()).build();\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Helidon"]))
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()

    def test_hono_http_rest(self) -> None:
        """Should detect Hono HTTP/REST patterns."""
        with tempfile.NamedTemporaryFile(suffix=".ts", mode="w", delete=False) as f:
            f.write("import { Hono } from 'hono';\nconst app = new Hono();\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Hono"]))
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()

    def test_koa_http_rest(self) -> None:
        """Should detect Koa HTTP/REST patterns."""
        with tempfile.NamedTemporaryFile(suffix=".js", mode="w", delete=False) as f:
            f.write("const Koa = require('koa');\nconst app = new Koa();\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Koa"]))
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()

    def test_hapi_http_rest(self) -> None:
        """Should detect Hapi HTTP/REST patterns."""
        with tempfile.NamedTemporaryFile(suffix=".js", mode="w", delete=False) as f:
            f.write(
                "const Hapi = require('@hapi/hapi');\nconst server = Hapi.server({port: 3000});\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Hapi"]))
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()

    def test_connect_grpc(self) -> None:
        """Should detect Connect gRPC patterns."""
        with tempfile.NamedTemporaryFile(suffix=".go", mode="w", delete=False) as f:
            f.write('"connectrpc.com/connect"\npath, handler := connect.NewHandler()\n')
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Connect"]))
            grpc_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.GRPC
                and p.confidence == Confidence.HIGH
            ]
            assert len(grpc_points) > 0
        finally:
            file_path.unlink()


class TestAugmentedExistingPatterns:
    """Tests for cloud service patterns added to existing integration types."""

    def test_java_sqs_client(self) -> None:
        """Should detect SqsClient as MESSAGING in Java."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("SqsClient sqsClient = SqsClient.builder().build();\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            msg_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.MESSAGING
                and p.confidence == Confidence.HIGH
            ]
            assert len(msg_points) > 0
        finally:
            file_path.unlink()

    def test_java_dynamodb_client(self) -> None:
        """Should detect DynamoDbClient as DATABASE in Java."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("DynamoDbClient ddb = DynamoDbClient.create();\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            db_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.DATABASE
                and p.confidence == Confidence.HIGH
            ]
            assert len(db_points) > 0
        finally:
            file_path.unlink()

    def test_csharp_azure_cosmos(self) -> None:
        """Should detect Microsoft.Azure.Cosmos as DATABASE in C#."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write(
                "using Microsoft.Azure.Cosmos;\n"
                "CosmosClient client = new CosmosClient(connectionString);\n"
                "Database database = client.GetDatabase(dbName);\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            db_points = [
                p for p in points if p.integration_type == IntegrationType.DATABASE
            ]
            assert len(db_points) > 0
        finally:
            file_path.unlink()

    def test_typescript_aws_sqs(self) -> None:
        """Should detect @aws-sdk/client-sqs as MESSAGING in TypeScript."""
        with tempfile.NamedTemporaryFile(suffix=".ts", mode="w", delete=False) as f:
            f.write(
                "import { SQSClient } from '@aws-sdk/client-sqs';\n"
                "const amqp = new SQSClient({ region: region });\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            msg_points = [
                p for p in points if p.integration_type == IntegrationType.MESSAGING
            ]
            assert len(msg_points) > 0
        finally:
            file_path.unlink()

    def test_python_google_cloud_storage(self) -> None:
        """Should detect google.cloud storage as FILE_IO in Python."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write(
                "from google.cloud import storage\n"
                "client = storage.Client()\n"
                "bucket = client.get_bucket(bucket_name)\n"
                "blob = bucket.blob(file)\n"
                "blob.upload_from_filename(source_file)\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            file_io_points = [
                p for p in points if p.integration_type == IntegrationType.FILE_IO
            ]
            assert len(file_io_points) > 0
        finally:
            file_path.unlink()


class TestCppExtensionMapping:
    """Tests for C and C++ language detection from file extensions."""

    def test_c_extension(self) -> None:
        """Should detect C from .c extension."""
        assert get_language_from_extension("main.c") == Language.C

    def test_h_extension(self) -> None:
        """Should detect C from .h extension."""
        assert get_language_from_extension("header.h") == Language.C

    def test_cpp_extension(self) -> None:
        """Should detect C++ from .cpp extension."""
        assert get_language_from_extension("main.cpp") == Language.CPP

    def test_hpp_extension(self) -> None:
        """Should detect C++ from .hpp extension."""
        assert get_language_from_extension("header.hpp") == Language.CPP


class TestCppBasePatterns:
    """Tests for C/C++ base integration pattern scanning."""

    def test_cpp_sqlite3_database(self) -> None:
        """Should detect sqlite3 includes as DATABASE in C++ files."""
        with tempfile.NamedTemporaryFile(suffix=".cpp", mode="w", delete=False) as f:
            f.write('#include <sqlite3.h>\nsqlite3_open("test.db", &db);\n')
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            db_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.DATABASE
                and p.confidence == Confidence.HIGH
            ]
            assert len(db_points) > 0
        finally:
            file_path.unlink()

    def test_cpp_curl_http_rest(self) -> None:
        """Should detect curl includes as HTTP_REST in C++ files."""
        with tempfile.NamedTemporaryFile(suffix=".cpp", mode="w", delete=False) as f:
            f.write(
                "#include <curl/curl.h>\nCURL *curl = curl_easy_init();\n"
                "curl_easy_perform(curl);\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()

    def test_c_and_cpp_share_patterns(self) -> None:
        """Same content in .c and .cpp should produce the same integration types."""
        content = '#include <sqlite3.h>\nsqlite3_open("test.db", &db);\n'
        results = {}

        for suffix in (".c", ".cpp"):
            with tempfile.NamedTemporaryFile(
                suffix=suffix, mode="w", delete=False
            ) as f:
                f.write(content)
                f.flush()
                file_path = Path(f.name)

            try:
                points = list(scan_file_for_integrations(file_path))
                results[suffix] = {p.integration_type for p in points}
            finally:
                file_path.unlink()

        assert IntegrationType.DATABASE in results[".c"]
        assert results[".c"] == results[".cpp"]

    def test_c_cpp_patterns_are_shared(self) -> None:
        """get_patterns_for_language should return the same patterns for C and C++ (ignoring source label)."""
        c_patterns = get_patterns_for_language(Language.C)
        cpp_patterns = get_patterns_for_language(Language.CPP)

        def strip_source(
            patterns: dict,
        ) -> dict:
            return {
                k: [(p, c) for p, c, _s, _d, _descs in v] for k, v in patterns.items()
            }

        assert strip_source(c_patterns) == strip_source(cpp_patterns)


class TestCppFrameworkPatterns:
    """Tests for C++ framework-specific pattern matching."""

    def test_qt_patterns_only_with_qt_framework(self) -> None:
        """Qt-specific patterns should only match when Qt framework is active."""
        with tempfile.NamedTemporaryFile(suffix=".cpp", mode="w", delete=False) as f:
            f.write('QSqlDatabase db = QSqlDatabase::addDatabase("QSQLITE");\n')
            f.flush()
            file_path = Path(f.name)

        try:
            # Without Qt framework
            points_without = list(scan_file_for_integrations(file_path))
            qt_points = [
                p for p in points_without if "QSqlDatabase" in p.matched_pattern
            ]
            assert len(qt_points) == 0

            # With Qt framework
            points_with = list(scan_file_for_integrations(file_path, frameworks=["Qt"]))
            qt_points = [p for p in points_with if "QSqlDatabase" in p.matched_pattern]
            assert len(qt_points) > 0
            assert qt_points[0].confidence == Confidence.HIGH
        finally:
            file_path.unlink()

    def test_boost_socket_patterns(self) -> None:
        """Boost socket patterns should match with Boost framework."""
        with tempfile.NamedTemporaryFile(suffix=".cpp", mode="w", delete=False) as f:
            f.write("boost::asio::ip::tcp::socket s(io);\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Boost"]))
            socket_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SOCKET
                and p.confidence == Confidence.HIGH
            ]
            assert len(socket_points) > 0
        finally:
            file_path.unlink()

    def test_crow_http_rest_patterns(self) -> None:
        """Crow HTTP/REST patterns should match with Crow framework."""
        with tempfile.NamedTemporaryFile(suffix=".cpp", mode="w", delete=False) as f:
            f.write(
                "crow::SimpleApp app;\n"
                'CROW_ROUTE(app, "/hello")([]{ return "Hello"; });\n'
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Crow"]))
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()

    def test_drogon_http_rest_patterns(self) -> None:
        """Drogon HTTP/REST patterns should match with Drogon framework."""
        with tempfile.NamedTemporaryFile(suffix=".cpp", mode="w", delete=False) as f:
            f.write("class MyCtrl : public HttpController<MyCtrl> {};\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Drogon"]))
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()

    def test_cpp_mailio_email(self) -> None:
        """Should detect mailio includes and types as EMAIL in C++ files."""
        with tempfile.NamedTemporaryFile(suffix=".cpp", mode="w", delete=False) as f:
            f.write(
                "#include <mailio/message.hpp>\n"
                "mailio::message msg;\n"
                'mailio::smtps conn("smtp.example.com", 587);\n'
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            email_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.EMAIL
                and p.confidence == Confidence.HIGH
            ]
            assert len(email_points) > 0
        finally:
            file_path.unlink()

    def test_cpp_vmime_email(self) -> None:
        """Should detect VMime includes and types as EMAIL in C++ files."""
        with tempfile.NamedTemporaryFile(suffix=".cpp", mode="w", delete=False) as f:
            f.write(
                "#include <vmime/vmime.hpp>\n"
                "vmime::net::smtp::SMTPTransport transport;\n"
                "vmime::messageBuilder mb;\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            email_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.EMAIL
                and p.confidence == Confidence.HIGH
            ]
            assert len(email_points) > 0
        finally:
            file_path.unlink()

    def test_poco_email_patterns(self) -> None:
        """POCO email patterns should match with POCO framework."""
        with tempfile.NamedTemporaryFile(suffix=".cpp", mode="w", delete=False) as f:
            f.write('SMTPClientSession session("smtp.example.com");\n')
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["POCO"]))
            email_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.EMAIL
                and p.confidence == Confidence.HIGH
            ]
            assert len(email_points) > 0
        finally:
            file_path.unlink()

    def test_cpp_base_patterns_with_framework(self) -> None:
        """Base C++ patterns should still match alongside framework patterns."""
        with tempfile.NamedTemporaryFile(suffix=".cpp", mode="w", delete=False) as f:
            f.write(
                "#include <curl/curl.h>\n"
                "CURL *curl = curl_easy_init();\n"
                'QSqlDatabase db = QSqlDatabase::addDatabase("QSQLITE");\n'
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Qt"]))
            http_points = [
                p for p in points if p.integration_type == IntegrationType.HTTP_REST
            ]
            db_points = [
                p for p in points if p.integration_type == IntegrationType.DATABASE
            ]
            assert len(http_points) > 0
            assert len(db_points) > 0
        finally:
            file_path.unlink()


class TestSyntaxZoneFiltering:
    """End-to-end tests for tree-sitter zone filtering in scan_file_for_integrations."""

    def test_java_comment_filtered_code_kept(self, tmp_path: Path) -> None:
        """Java comment mentioning @Entity should be filtered; code line kept."""
        java_file = tmp_path / "Service.java"
        java_file.write_text(
            "// @Entity annotation marks JPA entities\n"
            "@Entity\n"
            "class Service {}\n"
        )
        points = list(scan_file_for_integrations(java_file))
        db_points = [
            p for p in points if p.integration_type == IntegrationType.DATABASE
        ]
        # The code line (@Entity on line 2) should be detected
        assert any(p.match.line_number == 2 for p in db_points)
        # The comment line (line 1) should NOT be detected
        assert not any(p.match.line_number == 1 for p in db_points)

    def test_python_docstring_filtered_import_filtered_code_kept(
        self, tmp_path: Path
    ) -> None:
        """Python docstring and import line should be filtered; real code kept."""
        py_file = tmp_path / "client.py"
        py_file.write_text(
            "import requests\n"
            "\n"
            "def fetch(url):\n"
            '    """import requests is used here."""\n'
            "    http = requests.Session()\n"
            "    return http.get(url)\n"
        )
        points = list(scan_file_for_integrations(py_file))
        http_points = [
            p for p in points if p.integration_type == IntegrationType.HTTP_REST
        ]
        # Line 1 (import, classified as IMPORT zone) should be filtered
        assert not any(p.match.line_number == 1 for p in http_points)
        # Line 4 (docstring) should NOT be detected
        assert not any(p.match.line_number == 4 for p in http_points)
        # Line 5 (real code: http = ...) should be detected
        assert any(p.match.line_number == 5 for p in http_points)

    def test_pli_no_filtering(self, tmp_path: Path) -> None:
        """PL/I has no tree-sitter support; all lines should be scanned."""
        pli_file = tmp_path / "program.pli"
        pli_file.write_text("/* EXEC SQL SELECT */\n" "EXEC SQL SELECT * FROM TABLE;\n")
        points = list(scan_file_for_integrations(pli_file))
        db_points = [
            p for p in points if p.integration_type == IntegrationType.DATABASE
        ]
        # Both lines should be scanned (no filtering for PL/I)
        assert any(p.match.line_number == 1 for p in db_points)
        assert any(p.match.line_number == 2 for p in db_points)

    def test_java_multiline_block_comment_filtered(self, tmp_path: Path) -> None:
        """Java multi-line block comment with patterns should be fully filtered."""
        java_file = tmp_path / "Example.java"
        java_file.write_text(
            "/*\n"
            " * @Entity\n"
            " * @KafkaListener\n"
            " * @Repository\n"
            " */\n"
            "@Entity\n"
            "class Example {}\n"
        )
        points = list(scan_file_for_integrations(java_file))
        # Lines 1-5 are in a block comment and should be filtered
        comment_line_points = [p for p in points if p.match.line_number <= 5]
        assert len(comment_line_points) == 0
        # Line 6 (@Entity in code) should be detected
        code_points = [p for p in points if p.match.line_number == 6]
        assert len(code_points) > 0


class TestNeo4jIntegrationPatterns:
    """Tests for Neo4j database integration detection across languages."""

    def test_java_neo4j_driver_import(self, tmp_path: Path) -> None:
        """Should detect Neo4j driver import in Java files."""
        java_file = tmp_path / "Neo4jService.java"
        java_file.write_text(
            "import org.neo4j.driver.GraphDatabase;\n"
            "import org.neo4j.driver.Driver;\n"
            "public class Neo4jService {\n"
            '    Driver driver = GraphDatabase.driver("bolt://localhost");\n'
            "}\n"
        )
        points = list(scan_file_for_integrations(java_file))
        db_points = [
            p for p in points if p.integration_type == IntegrationType.DATABASE
        ]
        assert len(db_points) > 0
        assert all(p.confidence == Confidence.HIGH for p in db_points)

    def test_java_spring_data_neo4j(self, tmp_path: Path) -> None:
        """Should detect Spring Data Neo4j patterns."""
        java_file = tmp_path / "PersonNode.java"
        java_file.write_text(
            "@Node\n"
            "public class Person {\n"
            "    @Relationship\n"
            "    private List<Person> friends;\n"
            "}\n"
        )
        points = list(scan_file_for_integrations(java_file, frameworks=["Spring"]))
        db_points = [
            p for p in points if p.integration_type == IntegrationType.DATABASE
        ]
        patterns = [p.matched_pattern for p in db_points]
        assert any("@Node" in pat for pat in patterns)
        assert any("@Relationship" in pat for pat in patterns)

    def test_java_spring_neo4j_repository(self, tmp_path: Path) -> None:
        """Should detect Neo4jRepository in Spring."""
        java_file = tmp_path / "PersonRepository.java"
        java_file.write_text(
            "public interface PersonRepository extends Neo4jRepository<Person, Long> {}\n"
        )
        points = list(scan_file_for_integrations(java_file, frameworks=["Spring"]))
        db_points = [
            p for p in points if p.integration_type == IntegrationType.DATABASE
        ]
        patterns = [p.matched_pattern for p in db_points]
        assert any("Neo4jRepository" in pat for pat in patterns)

    def test_java_quarkus_neo4j(self, tmp_path: Path) -> None:
        """Should detect Quarkus Neo4j extension."""
        java_file = tmp_path / "QuarkusNeo4j.java"
        java_file.write_text(
            "import io.quarkus.neo4j.runtime.Neo4jConfiguration;\n"
            "GraphDatabase.driver(uri, authToken);\n"
        )
        points = list(scan_file_for_integrations(java_file, frameworks=["Quarkus"]))
        db_points = [
            p for p in points if p.integration_type == IntegrationType.DATABASE
        ]
        assert len(db_points) > 0

    def test_python_neo4j_driver(self, tmp_path: Path) -> None:
        """Should detect neo4j Python driver."""
        py_file = tmp_path / "graph_service.py"
        py_file.write_text(
            "from neo4j import GraphDatabase\n"
            "driver = GraphDatabase.driver(uri)\n"
            "session = driver.session(database=db_name)\n"
        )
        points = list(scan_file_for_integrations(py_file))
        db_points = [
            p for p in points if p.integration_type == IntegrationType.DATABASE
        ]
        assert len(db_points) > 0

    def test_python_py2neo(self, tmp_path: Path) -> None:
        """Should detect py2neo library."""
        py_file = tmp_path / "graph.py"
        py_file.write_text(
            "from py2neo import Graph\n" "graph = Graph(database=neo4j_db)\n"
        )
        points = list(scan_file_for_integrations(py_file))
        db_points = [
            p for p in points if p.integration_type == IntegrationType.DATABASE
        ]
        assert len(db_points) > 0

    def test_python_neomodel(self, tmp_path: Path) -> None:
        """Should detect neomodel library."""
        py_file = tmp_path / "models.py"
        py_file.write_text(
            "from neomodel import StructuredNode, StringProperty\n"
            "class Person(StructuredNode): # database node\n"
            "    name = StringProperty()\n"
        )
        points = list(scan_file_for_integrations(py_file))
        db_points = [
            p for p in points if p.integration_type == IntegrationType.DATABASE
        ]
        assert len(db_points) > 0

    def test_python_django_neomodel(self, tmp_path: Path) -> None:
        """Should detect neomodel Django integration."""
        py_file = tmp_path / "models.py"
        py_file.write_text(
            "from neomodel import StructuredNode, StringProperty\n"
            "class Person(StructuredNode):\n"
            "    name = StringProperty()\n"
            "neomodel.db.cypher_query('MATCH (n) RETURN n')\n"
        )
        points = list(scan_file_for_integrations(py_file, frameworks=["Django"]))
        db_points = [
            p for p in points if p.integration_type == IntegrationType.DATABASE
        ]
        patterns = [p.matched_pattern for p in db_points]
        assert any("neomodel" in pat for pat in patterns)

    def test_typescript_neo4j_driver(self, tmp_path: Path) -> None:
        """Should detect neo4j-driver in TypeScript."""
        ts_file = tmp_path / "neo4j.ts"
        ts_file.write_text(
            "import neo4j from 'neo4j-driver';\n"
            "const driver = neo4j.driver('bolt://localhost');\n"
        )
        points = list(scan_file_for_integrations(ts_file))
        db_points = [
            p for p in points if p.integration_type == IntegrationType.DATABASE
        ]
        assert len(db_points) > 0
        assert all(p.confidence == Confidence.HIGH for p in db_points)

    def test_typescript_nestjs_neo4j(self, tmp_path: Path) -> None:
        """Should detect NestJS Neo4j integration."""
        ts_file = tmp_path / "neo4j.module.ts"
        ts_file.write_text(
            "import { Neo4jService } from 'nest-neo4j';\n"
            "const neo4jService = new Neo4jService(config);\n"
        )
        points = list(scan_file_for_integrations(ts_file, frameworks=["NestJS"]))
        db_points = [
            p for p in points if p.integration_type == IntegrationType.DATABASE
        ]
        assert len(db_points) > 0

    def test_javascript_neo4j_driver(self, tmp_path: Path) -> None:
        """Should detect neo4j-driver in JavaScript."""
        js_file = tmp_path / "db.js"
        js_file.write_text(
            "const neo4j = require('neo4j-driver');\n"
            "const driver = neo4j.driver('bolt://localhost');\n"
        )
        points = list(scan_file_for_integrations(js_file))
        db_points = [
            p for p in points if p.integration_type == IntegrationType.DATABASE
        ]
        assert len(db_points) > 0

    def test_go_neo4j_driver(self, tmp_path: Path) -> None:
        """Should detect neo4j-go-driver in Go."""
        go_file = tmp_path / "main.go"
        go_file.write_text(
            'import "github.com/neo4j/neo4j-go-driver/v5/neo4j"\n'
            "func main() {\n"
            '    driver, _ := neo4j.NewDriverWithContext("bolt://localhost")\n'
            "}\n"
        )
        points = list(scan_file_for_integrations(go_file))
        db_points = [
            p for p in points if p.integration_type == IntegrationType.DATABASE
        ]
        assert len(db_points) > 0
        assert all(p.confidence == Confidence.HIGH for p in db_points)

    def test_rust_neo4rs(self, tmp_path: Path) -> None:
        """Should detect neo4rs crate in Rust."""
        rs_file = tmp_path / "main.rs"
        rs_file.write_text(
            "use neo4rs::Graph;\n"
            'let graph = neo4rs::Graph::new("bolt://localhost").await;\n'
        )
        points = list(scan_file_for_integrations(rs_file))
        db_points = [
            p for p in points if p.integration_type == IntegrationType.DATABASE
        ]
        assert len(db_points) > 0

    def test_csharp_neo4j_driver(self, tmp_path: Path) -> None:
        """Should detect Neo4j.Driver in C#."""
        cs_file = tmp_path / "Neo4jService.cs"
        cs_file.write_text(
            "using Neo4j.Driver;\n"
            "public class Neo4jService {\n"
            '    IDriver driver = GraphDatabase.Driver("bolt://localhost");\n'
            "}\n"
        )
        points = list(scan_file_for_integrations(cs_file))
        db_points = [
            p for p in points if p.integration_type == IntegrationType.DATABASE
        ]
        assert len(db_points) > 0
        assert all(p.confidence == Confidence.HIGH for p in db_points)

    def test_csharp_aspnet_neo4j(self, tmp_path: Path) -> None:
        """Should detect Neo4j DI in ASP.NET Core."""
        cs_file = tmp_path / "Startup.cs"
        cs_file.write_text("services.AddNeo4j(configuration);\n")
        points = list(scan_file_for_integrations(cs_file, frameworks=["ASP.NET Core"]))
        db_points = [
            p for p in points if p.integration_type == IntegrationType.DATABASE
        ]
        patterns = [p.matched_pattern for p in db_points]
        assert any("AddNeo4j" in pat for pat in patterns)

    def test_ruby_neo4j_driver(self, tmp_path: Path) -> None:
        """Should detect neo4j gem in Ruby."""
        rb_file = tmp_path / "graph.rb"
        rb_file.write_text(
            "require 'neo4j'\n" "driver = Neo4j::Driver.new('bolt://localhost')\n"
        )
        points = list(scan_file_for_integrations(rb_file))
        db_points = [
            p for p in points if p.integration_type == IntegrationType.DATABASE
        ]
        assert len(db_points) > 0

    def test_ruby_activegraph(self, tmp_path: Path) -> None:
        """Should detect activegraph gem in Ruby."""
        rb_file = tmp_path / "person.rb"
        rb_file.write_text("require 'activegraph'\n")
        points = list(scan_file_for_integrations(rb_file))
        db_points = [
            p for p in points if p.integration_type == IntegrationType.DATABASE
        ]
        assert len(db_points) > 0

    def test_ruby_rails_activegraph(self, tmp_path: Path) -> None:
        """Should detect ActiveGraph in Rails."""
        rb_file = tmp_path / "person.rb"
        rb_file.write_text(
            "class Person\n"
            "  include ActiveGraph::Node\n"
            "  include ActiveGraph::Relationship\n"
            "end\n"
        )
        points = list(scan_file_for_integrations(rb_file, frameworks=["Rails"]))
        db_points = [
            p for p in points if p.integration_type == IntegrationType.DATABASE
        ]
        patterns = [p.matched_pattern for p in db_points]
        assert any("ActiveGraph::Node" in pat for pat in patterns)
