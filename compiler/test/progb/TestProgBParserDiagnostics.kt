package prog8tests.progb

import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.ints.shouldBeGreaterThan
import prog8.code.source.SourceCode
import prog8.parser.MultipleParseErrors
import prog8.parser.ProgBParser

class TestProgBParserDiagnostics : FunSpec({

    test("collects multiple parse errors") {
        val src = """
            MODULE main
                SUB start()
                    DIM x AS = 1
                    DIM y AS = 2
                END SUB
            END MODULE
        """.trimIndent()

        val errors = shouldThrow<MultipleParseErrors> {
            ProgBParser.parseModule(SourceCode.Text(src))
        }

        errors.errors.size shouldBeGreaterThan 1
    }
})
