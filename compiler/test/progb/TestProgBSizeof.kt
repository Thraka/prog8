package prog8tests.progb

import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe
import prog8.ast.Module
import prog8.ast.expressions.AddressOf
import prog8.ast.expressions.FunctionCallExpression
import prog8.ast.expressions.IdentifierReference
import prog8.ast.statements.Block
import prog8.ast.statements.Subroutine
import prog8.ast.statements.VarDecl
import prog8.code.source.SourceCode
import prog8.parser.MultipleParseErrors
import prog8.parser.Prog8Parser
import prog8.parser.ProgBParser

class TestProgBSizeof : FunSpec({

    fun mainBlock(module: Module): Block =
        module.statements.filterIsInstance<Block>().single { it.name == "main" }

    fun startSub(block: Block): Subroutine =
        block.statements.filterIsInstance<Subroutine>().single { it.name == "start" }

    test("sizeof accepted argument kinds map equivalently") {
        val prog8Src = """
            main {
                ubyte x
                sub start() {
                    ubyte a = sizeof(ubyte)
                    ubyte b = sizeof(x)
                    ubyte c = sizeof(&x)
                    ubyte d = sizeof(^^ubyte)
                    ubyte e = sizeof(&&x)
                }
            }
        """

        val progbSrc = """
            MODULE main
                DIM x AS UBYTE
                SUB start()
                    DIM a AS UBYTE = SIZEOF(UBYTE)
                    DIM b AS UBYTE = SIZEOF(x)
                    DIM c AS UBYTE = SIZEOF(&x)
                    DIM d AS UBYTE = SIZEOF(PTR UBYTE)
                    DIM e AS UBYTE = SIZEOF(TYPEDADDR(x))
                END SUB
            END MODULE
        """

        val prog8Start = startSub(mainBlock(Prog8Parser.parseModule(SourceCode.Text(prog8Src))))
        val progbStart = startSub(mainBlock(ProgBParser.parseModule(SourceCode.Text(progbSrc))))

        val prog8Decls = prog8Start.statements.filterIsInstance<VarDecl>().associateBy { it.name }
        val progbDecls = progbStart.statements.filterIsInstance<VarDecl>().associateBy { it.name }

        // a,b,c,e become sizeof() calls in both visitors
        for(name in listOf("a", "b", "c", "e")) {
            val p8Call = prog8Decls[name]?.value as FunctionCallExpression
            val pbCall = progbDecls[name]?.value as FunctionCallExpression
            p8Call.target.nameInSource shouldBe pbCall.target.nameInSource
        }

        val p8Addressof = prog8Decls["c"]?.value as FunctionCallExpression
        val pbAddressof = progbDecls["c"]?.value as FunctionCallExpression
        (p8Addressof.args[0] as AddressOf).typed shouldBe false
        (pbAddressof.args[0] as AddressOf).typed shouldBe false

        val p8TypedAddressof = prog8Decls["e"]?.value as FunctionCallExpression
        val pbTypedAddressof = progbDecls["e"]?.value as FunctionCallExpression
        (p8TypedAddressof.args[0] as AddressOf).typed shouldBe true
        (pbTypedAddressof.args[0] as AddressOf).typed shouldBe true

        // d maps to sys.SIZEOF_POINTER identifier in both visitors
        val p8Ptr = prog8Decls["d"]?.value as IdentifierReference
        val pbPtr = progbDecls["d"]?.value as IdentifierReference
        p8Ptr.nameInSource shouldBe pbPtr.nameInSource
        pbPtr.nameInSource shouldBe listOf("sys", "SIZEOF_POINTER")
    }

    test("sizeof rejects arbitrary expression arguments") {
        val src = """
            MODULE main
                SUB start()
                    DIM x AS UBYTE = SIZEOF(1 + 2)
                END SUB
            END MODULE
        """.trimIndent()

        shouldThrow<MultipleParseErrors> {
            ProgBParser.parseModule(SourceCode.Text(src))
        }
    }
})
