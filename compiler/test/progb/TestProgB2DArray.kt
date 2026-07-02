package prog8tests.progb

import io.kotest.assertions.fail
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe
import prog8.ast.Module
import prog8.ast.expressions.ArrayIndexedExpression
import prog8.ast.expressions.IdentifierReference
import prog8.ast.expressions.NumericLiteral
import prog8.ast.statements.Assignment
import prog8.ast.statements.Block
import prog8.ast.statements.Subroutine
import prog8.ast.statements.VarDecl
import prog8.code.source.SourceCode
import prog8.parser.Prog8Parser
import prog8.parser.ProgBParser

class TestProgB2DArray : FunSpec({

    fun mainBlock(module: Module): Block =
        module.statements.filterIsInstance<Block>().single { it.name == "main" }

    fun assertMatrixIndex(expr: ArrayIndexedExpression, varName: String) {
        val outerIndex = expr.indexer.indexExpr as? NumericLiteral ?: fail("outer index should be numeric")
        outerIndex.number.toInt() shouldBe 2

        val nested = expr.nestedArray ?: fail("expected nested array index expression")
        val innerIndex = nested.indexer.indexExpr as? NumericLiteral ?: fail("inner index should be numeric")
        innerIndex.number.toInt() shouldBe 1

        nested.plainarrayvar?.nameInSource shouldBe listOf(varName)
    }

    test("2D array declaration and indexing map equivalently") {
        val prog8Src = """
            main {
                ubyte[4][3] grid

                sub start() {
                    grid[1][2] = 9
                    ubyte value = grid[1][2]
                }
            }
        """

        val progbSrc = """
            MODULE main
                DIM grid[4][3] AS UBYTE

                SUB start()
                    grid[1][2] = 9
                    DIM value AS UBYTE = grid[1][2]
                END SUB
            END MODULE
        """

        val prog8Main = mainBlock(Prog8Parser.parseModule(SourceCode.Text(prog8Src)))
        val progbMain = mainBlock(ProgBParser.parseModule(SourceCode.Text(progbSrc)))

        val prog8Grid = prog8Main.statements.filterIsInstance<VarDecl>().single { it.name == "grid" }
        val progbGrid = progbMain.statements.filterIsInstance<VarDecl>().single { it.name == "grid" }

        val prog8Total = prog8Grid.arraysize?.indexExpr as? NumericLiteral ?: fail("prog8 arraysize missing")
        val progbTotal = progbGrid.arraysize?.indexExpr as? NumericLiteral ?: fail("progb arraysize missing")
        prog8Total.number.toInt() shouldBe 12
        progbTotal.number.toInt() shouldBe 12

        val prog8Cols = prog8Grid.matrixNumCols as? NumericLiteral ?: fail("prog8 matrix cols missing")
        val progbCols = progbGrid.matrixNumCols as? NumericLiteral ?: fail("progb matrix cols missing")
        prog8Cols.number.toInt() shouldBe 3
        progbCols.number.toInt() shouldBe 3

        val prog8Start = prog8Main.statements.filterIsInstance<Subroutine>().single { it.name == "start" }
        val progbStart = progbMain.statements.filterIsInstance<Subroutine>().single { it.name == "start" }

        val prog8Write = prog8Start.statements[0] as Assignment
        val progbWrite = progbStart.statements[0] as Assignment
        val prog8WriteIndex = prog8Write.target.arrayindexed ?: fail("prog8 assignment target is not array indexed")
        val progbWriteIndex = progbWrite.target.arrayindexed ?: fail("progb assignment target is not array indexed")
        assertMatrixIndex(prog8WriteIndex, "grid")
        assertMatrixIndex(progbWriteIndex, "grid")

        val prog8Value = prog8Start.statements.filterIsInstance<VarDecl>().single { it.name == "value" }
        val progbValue = progbStart.statements.filterIsInstance<VarDecl>().single { it.name == "value" }
        val prog8ReadIndex = prog8Value.value as? ArrayIndexedExpression ?: fail("prog8 read is not array indexed")
        val progbReadIndex = progbValue.value as? ArrayIndexedExpression ?: fail("progb read is not array indexed")
        assertMatrixIndex(prog8ReadIndex, "grid")
        assertMatrixIndex(progbReadIndex, "grid")

        // Both resolve to the same identifier chain for the indexed variable.
        (prog8ReadIndex.nestedArray?.plainarrayvar as IdentifierReference).nameInSource shouldBe
            (progbReadIndex.nestedArray?.plainarrayvar as IdentifierReference).nameInSource
    }
})
