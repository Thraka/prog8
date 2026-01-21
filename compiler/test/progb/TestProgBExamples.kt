package prog8tests.progb

import io.kotest.core.spec.style.FunSpec
import io.kotest.datatest.withData
import io.kotest.engine.spec.tempdir
import io.kotest.matchers.shouldNotBe
import prog8.code.target.C64Target
import prog8.code.target.Cx16Target
import prog8.code.target.VMTarget
import prog8tests.helpers.assumeDirectory
import prog8tests.helpers.compileFile
import prog8tests.helpers.workingDir

/**
 * Tests that compile all ProgB example files from the examples/progb directory.
 * This serves as integration testing to ensure real-world ProgB programs compile.
 */
class TestProgBExamples : FunSpec({
    val outputDir = tempdir().toPath()
    val progbExamplesDir = assumeDirectory(workingDir, "../examples/progb")

    context("compile ProgB examples for virtual target") {
        withData(
            nameFn = { "$it.pb" },
            listOf(
                "hello",
                "primes",
                "fibonacci",
                "numbergame",
                "bordercolor"
            )
        ) { name ->
            val result = compileFile(
                VMTarget(),
                optimize = false,
                progbExamplesDir,
                "$name.pb",
                outputDir
            )
            result shouldNotBe null
        }
    }

    context("compile ProgB examples for C64 target") {
        withData(
            nameFn = { "$it.pb (C64)" },
            listOf(
                "hello",
                "primes",
                "fibonacci",
                "numbergame",
                "bordercolor"
            )
        ) { name ->
            val result = compileFile(
                C64Target(),
                optimize = false,
                progbExamplesDir,
                "$name.pb",
                outputDir
            )
            result shouldNotBe null
        }
    }

    context("compile ProgB examples for Cx16 target") {
        withData(
            nameFn = { "$it.pb (Cx16)" },
            listOf(
                "hello",
                "primes",
                "fibonacci",
                "numbergame"
                // bordercolor might be C64-specific
            )
        ) { name ->
            val result = compileFile(
                Cx16Target(),
                optimize = false,
                progbExamplesDir,
                "$name.pb",
                outputDir
            )
            result shouldNotBe null
        }
    }

    context("compile ProgB examples with optimization") {
        withData(
            nameFn = { "$it.pb (optimized)" },
            listOf(
                "hello",
                "primes",
                "fibonacci"
            )
        ) { name ->
            val result = compileFile(
                VMTarget(),
                optimize = true,
                progbExamplesDir,
                "$name.pb",
                outputDir
            )
            result shouldNotBe null
        }
    }
})
