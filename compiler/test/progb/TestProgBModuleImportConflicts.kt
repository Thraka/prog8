package prog8tests.progb

import com.github.michaelbull.result.getOrElse
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe
import io.kotest.matchers.string.shouldContain
import io.kotest.matchers.shouldNotBe
import prog8.ast.Program
import prog8.compiler.ModuleImporter
import prog8tests.helpers.ErrorReporterForTests
import prog8tests.helpers.DummyFunctions
import prog8tests.helpers.DummyMemsizer
import prog8tests.helpers.DummyStringEncoder
import kotlin.io.path.Path
import kotlin.io.path.absolute
import kotlin.io.path.createTempDirectory
import kotlin.io.path.invariantSeparatorsPathString
import kotlin.io.path.writeText

class TestProgBModuleImportConflicts : FunSpec({

    test("import reports conflict when both .p8 and .pb module sources exist") {
        val outputDir = createTempDirectory("progb-import-conflict-")
        val moduleName = "zz_conflict_mod"

        outputDir.resolve("$moduleName.p8").writeText(
            """
            $moduleName {
            }
            """.trimIndent()
        )
        outputDir.resolve("$moduleName.pb").writeText(
            """
            MODULE $moduleName
            END MODULE
            """.trimIndent()
        )

        val errors = ErrorReporterForTests(false)
        val src = """
            %import $moduleName

            MODULE main
                SUB start()
                END SUB
            END MODULE
        """.trimIndent()

        val mainFile = outputDir.resolve("main.pb")
        mainFile.writeText(src)

        val program = Program("main", DummyFunctions, DummyMemsizer, DummyStringEncoder)
        val importer = ModuleImporter(
            program = program,
            compilationTargetName = "virtual",
            errors = errors,
            sourceDirs = listOf(outputDir.invariantSeparatorsPathString),
            libraryDirs = emptyList(),
            cwd = Path(".").absolute(),
            quiet = true,
            nostdlib = true
        )

        importer.importMainModule(mainFile).getOrElse { throw it }

        errors.noErrors() shouldBe false
        errors.errors.singleOrNull() shouldNotBe null
        errors.errors.single() shouldContain "conflicting module sources for '$moduleName'"
        errors.errors.single() shouldContain "$moduleName.p8"
        errors.errors.single() shouldContain "$moduleName.pb"
    }
})
