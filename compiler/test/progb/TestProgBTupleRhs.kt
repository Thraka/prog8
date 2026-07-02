package prog8tests.progb

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe
import io.kotest.matchers.types.shouldBeInstanceOf
import prog8.ast.Module
import prog8.ast.expressions.ExpressionTuple
import prog8.ast.expressions.NumericLiteral
import prog8.ast.statements.Assignment
import prog8.ast.statements.AnonymousScope
import prog8.ast.statements.Block
import prog8.ast.statements.Subroutine
import prog8.ast.statements.VarDecl
import prog8.code.source.SourceCode
import prog8.parser.Prog8Parser
import prog8.parser.ProgBParser

class TestProgBTupleRhs : FunSpec({

    fun mainBlock(module: Module): Block =
        module.statements.filterIsInstance<Block>().single { it.name == "main" }

    fun startSub(block: Block): Subroutine =
        block.statements.filterIsInstance<Subroutine>().single { it.name == "start" }

    test("tuple RHS in DIM initializer and multi-target assignment maps equivalently") {
        val prog8Src = """
            main {
                sub start() {
                    ubyte a, b = 10, 20
                    a, b = 30, 40
                }
            }
        """

        val progbSrc = """
            MODULE main
                SUB start()
                    DIM a, b AS UBYTE = 10, 20
                    a, b = 30, 40
                END SUB
            END MODULE
        """

        val prog8Start = startSub(mainBlock(Prog8Parser.parseModule(SourceCode.Text(prog8Src))))
        val progbStart = startSub(mainBlock(ProgBParser.parseModule(SourceCode.Text(progbSrc))))

        val prog8Decl = prog8Start.statements.filterIsInstance<VarDecl>().single { it.name == "<multiple>" }
        val progbDecl = progbStart.statements.filterIsInstance<VarDecl>().single { it.name == "<multiple>" }

        val prog8TupleInit = prog8Decl.value.shouldBeInstanceOf<ExpressionTuple>()
        val progbTupleInit = progbDecl.value.shouldBeInstanceOf<ExpressionTuple>()
        (prog8TupleInit.expressions[0] as NumericLiteral).number.toInt() shouldBe (progbTupleInit.expressions[0] as NumericLiteral).number.toInt()
        (prog8TupleInit.expressions[1] as NumericLiteral).number.toInt() shouldBe (progbTupleInit.expressions[1] as NumericLiteral).number.toInt()

        // Tuple assignment lowers into an AnonymousScope of assignments in both visitors.
        val prog8TupleAssigns = prog8Start.statements
            .filterIsInstance<AnonymousScope>()
            .single()
            .statements
            .filterIsInstance<Assignment>()
        val progbTupleAssigns = progbStart.statements
            .filterIsInstance<AnonymousScope>()
            .single()
            .statements
            .filterIsInstance<Assignment>()

        (prog8TupleAssigns[0].value as NumericLiteral).number.toInt() shouldBe (progbTupleAssigns[0].value as NumericLiteral).number.toInt()
        (prog8TupleAssigns[1].value as NumericLiteral).number.toInt() shouldBe (progbTupleAssigns[1].value as NumericLiteral).number.toInt()
    }
})
