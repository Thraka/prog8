package prog8tests.progb

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe
import io.kotest.matchers.types.shouldBeInstanceOf
import prog8.ast.Module
import prog8.ast.statements.Block
import prog8.ast.statements.Subroutine
import prog8.ast.statements.Swap
import prog8.code.source.SourceCode
import prog8.parser.Prog8Parser
import prog8.parser.ProgBParser

class TestProgBSwap : FunSpec({

    fun mainBlock(module: Module): Block =
        module.statements.filterIsInstance<Block>().single { it.name == "main" }

    fun startSub(block: Block): Subroutine =
        block.statements.filterIsInstance<Subroutine>().single { it.name == "start" }

    test("swap statement maps to Swap AST node for ProgB and matches Prog8") {
        val prog8Src = """
            main {
                sub start() {
                    ubyte a=1
                    ubyte b=2
                    swap(a, b)
                }
            }
        """

        val progbSrc = """
            MODULE main
                SUB start()
                    DIM a AS UBYTE = 1
                    DIM b AS UBYTE = 2
                    SWAP(a, b)
                END SUB
            END MODULE
        """

        val prog8Start = startSub(mainBlock(Prog8Parser.parseModule(SourceCode.Text(prog8Src))))
        val progbStart = startSub(mainBlock(ProgBParser.parseModule(SourceCode.Text(progbSrc))))

        val prog8Swap = prog8Start.statements[2].shouldBeInstanceOf<Swap>()
        val progbSwap = progbStart.statements[2].shouldBeInstanceOf<Swap>()

        prog8Swap.t1.identifier?.nameInSource shouldBe progbSwap.t1.identifier?.nameInSource
        prog8Swap.t2.identifier?.nameInSource shouldBe progbSwap.t2.identifier?.nameInSource
    }
})
