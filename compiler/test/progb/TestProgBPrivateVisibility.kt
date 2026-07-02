package prog8tests.progb

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe
import prog8.ast.Module
import prog8.ast.statements.Alias
import prog8.ast.statements.Block
import prog8.ast.statements.StructDecl
import prog8.ast.statements.Subroutine
import prog8.ast.statements.VarDecl
import prog8.ast.statements.VarDeclType
import prog8.code.source.SourceCode
import prog8.parser.Prog8Parser
import prog8.parser.ProgBParser

/**
 * Verifies that the PRIVATE visibility prefix in ProgB maps to the same
 * private AST flags that Prog8 produces, for every supported declaration form:
 * const, alias, struct (TYPE), sub, asmsub, extsub, and variable declarations.
 *
 * These checks are done at the raw-parse level (before semantic processing),
 * so that declarations like aliases and unused locals are still present in the AST.
 */
class TestProgBPrivateVisibility : FunSpec({

    val prog8Src = """
        main {
            private const ubyte K = 7
            private alias mainAlias = main.start
            private struct Item {
                ubyte value
            }

            sub start() {
                private ubyte local = 1
            }

            private sub helper() {
            }

            private asmsub fast() {
                %asm {{
                    nop
                }}
            }

            private extsub ${'$'}ffd2 = chrout(ubyte ch @A)
        }
    """

    val progbSrc = """
        MODULE main
            PRIVATE CONST K AS UBYTE = 7
            PRIVATE ALIAS mainAlias = main.start
            PRIVATE TYPE Item
                value AS UBYTE
            END TYPE

            SUB start()
                PRIVATE DIM local AS UBYTE = 1
            END SUB

            PRIVATE SUB helper()
            END SUB

            PRIVATE ASMSUB fast()
                ASM
                    nop
                END ASM
            END ASMSUB

            PRIVATE EXTSUB ${'$'}FFD2 = chrout(ch AS UBYTE @A)
        END MODULE
    """

    fun mainBlock(module: Module): Block =
        module.statements.filterIsInstance<Block>().single { it.name == "main" }

    test("private declarations set private flag in both Prog8 and ProgB") {
        val prog8Main = mainBlock(Prog8Parser.parseModule(SourceCode.Text(prog8Src)))
        val progbMain = mainBlock(ProgBParser.parseModule(SourceCode.Text(progbSrc)))

        // private const
        prog8Main.statements.filterIsInstance<VarDecl>().single { it.type == VarDeclType.CONST }.isPrivate shouldBe true
        progbMain.statements.filterIsInstance<VarDecl>().single { it.type == VarDeclType.CONST }.isPrivate shouldBe true

        // private alias
        prog8Main.statements.filterIsInstance<Alias>().single { it.alias == "mainAlias" }.isPrivate shouldBe true
        progbMain.statements.filterIsInstance<Alias>().single { it.alias == "mainAlias" }.isPrivate shouldBe true

        // private struct / TYPE
        prog8Main.statements.filterIsInstance<StructDecl>().single { it.name == "Item" }.isPrivate shouldBe true
        progbMain.statements.filterIsInstance<StructDecl>().single { it.name == "Item" }.isPrivate shouldBe true

        // non-private sub stays public
        val prog8Start = prog8Main.statements.filterIsInstance<Subroutine>().single { it.name == "start" }
        val progbStart = progbMain.statements.filterIsInstance<Subroutine>().single { it.name == "start" }
        prog8Start.isPrivate shouldBe false
        progbStart.isPrivate shouldBe false

        // private local variable inside sub
        prog8Start.statements.filterIsInstance<VarDecl>().single { it.name == "local" }.isPrivate shouldBe true
        progbStart.statements.filterIsInstance<VarDecl>().single { it.name == "local" }.isPrivate shouldBe true

        // private sub
        prog8Main.statements.filterIsInstance<Subroutine>().single { it.name == "helper" }.isPrivate shouldBe true
        progbMain.statements.filterIsInstance<Subroutine>().single { it.name == "helper" }.isPrivate shouldBe true

        // private asmsub
        prog8Main.statements.filterIsInstance<Subroutine>().single { it.name == "fast" }.isPrivate shouldBe true
        progbMain.statements.filterIsInstance<Subroutine>().single { it.name == "fast" }.isPrivate shouldBe true

        // private extsub (external subroutine with asm address)
        prog8Main.statements.filterIsInstance<Subroutine>().single { it.asmAddress != null }.isPrivate shouldBe true
        progbMain.statements.filterIsInstance<Subroutine>().single { it.asmAddress != null }.isPrivate shouldBe true
    }
})
