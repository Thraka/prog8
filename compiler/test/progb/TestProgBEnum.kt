package prog8tests.progb

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe
import prog8.ast.Module
import prog8.ast.expressions.IdentifierReference
import prog8.ast.statements.Block
import prog8.ast.statements.Enumeration
import prog8.ast.statements.Subroutine
import prog8.ast.statements.VarDecl
import prog8.code.core.BaseDataType
import prog8.code.source.SourceCode
import prog8.parser.Prog8Parser
import prog8.parser.ProgBParser

class TestProgBEnum : FunSpec({

    fun mainBlock(module: Module): Block =
        module.statements.filterIsInstance<Block>().single { it.name == "main" }

    test("enum declaration maps to same AST shape as Prog8") {
        val prog8Src = """
            main {
                private enum Color {
                    RED=1,
                    GREEN,
                    BIG=${'$'}1234
                }

                sub start() {
                    ubyte selected = Color::GREEN
                    ubyte maxv = Color::BIG
                }
            }
        """

        val progbSrc = """
            MODULE main
                PRIVATE ENUM Color
                    RED = 1
                    GREEN
                    BIG = ${'$'}1234
                END ENUM

                SUB start()
                    DIM selected AS UBYTE = Color::GREEN
                    DIM maxv AS UBYTE = Color::BIG
                END SUB
            END MODULE
        """

        val prog8Main = mainBlock(Prog8Parser.parseModule(SourceCode.Text(prog8Src)))
        val progbMain = mainBlock(ProgBParser.parseModule(SourceCode.Text(progbSrc)))

        val prog8Enum = prog8Main.statements.filterIsInstance<Enumeration>().single { it.name == "Color" }
        val progbEnum = progbMain.statements.filterIsInstance<Enumeration>().single { it.name == "Color" }

        prog8Enum.isPrivate shouldBe true
        progbEnum.isPrivate shouldBe true

        // Explicit $1234 requires UWORD as inferred enum storage type.
        prog8Enum.type shouldBe BaseDataType.UWORD
        progbEnum.type shouldBe BaseDataType.UWORD

        prog8Enum.members.toList() shouldBe progbEnum.members.toList()
        progbEnum.members.toList() shouldBe listOf(
            "RED" to 1,
            "GREEN" to null,
            "BIG" to 0x1234
        )

        // Ensure enum members are actually used and parsed identically.
        val prog8Start = prog8Main.statements.filterIsInstance<Subroutine>().single { it.name == "start" }
        val progbStart = progbMain.statements.filterIsInstance<Subroutine>().single { it.name == "start" }

        val prog8Selected = prog8Start.statements.filterIsInstance<VarDecl>().single { it.name == "selected" }
        val progbSelected = progbStart.statements.filterIsInstance<VarDecl>().single { it.name == "selected" }
        val prog8Maxv = prog8Start.statements.filterIsInstance<VarDecl>().single { it.name == "maxv" }
        val progbMaxv = progbStart.statements.filterIsInstance<VarDecl>().single { it.name == "maxv" }

        (prog8Selected.value as IdentifierReference).nameInSource shouldBe (progbSelected.value as IdentifierReference).nameInSource
        (prog8Maxv.value as IdentifierReference).nameInSource shouldBe (progbMaxv.value as IdentifierReference).nameInSource
        (progbSelected.value as IdentifierReference).nameInSource shouldBe listOf("Color::GREEN")
        (progbMaxv.value as IdentifierReference).nameInSource shouldBe listOf("Color::BIG")
    }
})
