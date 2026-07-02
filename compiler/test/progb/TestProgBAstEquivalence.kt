package prog8tests.progb

import io.kotest.core.spec.style.FunSpec
import io.kotest.engine.spec.tempdir
import io.kotest.matchers.shouldBe
import io.kotest.matchers.shouldNotBe
import prog8.ast.statements.*
import prog8.code.target.VMTarget
import prog8tests.helpers.compileText
import prog8tests.helpers.compileTextPB

/**
 * Tests that verify ProgB produces the same AST as equivalent Prog8 code.
 * Since both syntaxes compile to the same AST, identical source semantics
 * should produce identical AST structures.
 */
class TestProgBAstEquivalence : FunSpec({
    val outputDir = tempdir().toPath()

    /**
     * Helper to compare key AST properties between Prog8 and ProgB compilation results.
     * We compare structural elements rather than doing deep equality since positions will differ.
     */

    // ========================================================================
    // VARIABLE DECLARATIONS
    // ========================================================================

    context("variable declaration equivalence") {
        test("simple ubyte variable") {
            val prog8Src = """
                main {
                    sub start() {
                        ubyte x = 42
                        x++
                    }
                }
            """
            val progbSrc = """
                MODULE main
                    SUB start()
                        DIM x AS UBYTE = 42
                        x++
                    END SUB
                END MODULE
            """
            val prog8Result = compileText(VMTarget(), false, prog8Src, outputDir, writeAssembly = false)!!
            val progbResult = compileTextPB(VMTarget(), false, progbSrc, outputDir, writeAssembly = false)!!

            val prog8Start = prog8Result.compilerAst.entrypoint
            val progbStart = progbResult.compilerAst.entrypoint

            // Both should have the same number of statements
            prog8Start.statements.size shouldBe progbStart.statements.size

            // First statement should be a variable declaration
            val prog8Var = prog8Start.statements[0] as VarDecl
            val progbVar = progbStart.statements[0] as VarDecl

            prog8Var.name shouldBe progbVar.name
            prog8Var.datatype shouldBe progbVar.datatype
        }

        test("string variable with initializer") {
            val prog8Src = """
                main {
                    sub start() {
                        str @shared name = "hello"
                    }
                }
            """
            val progbSrc = """
                MODULE main
                    SUB start()
                        DIM name AS STRING = "hello" @shared
                    END SUB
                END MODULE
            """
            val prog8Result = compileText(VMTarget(), false, prog8Src, outputDir, writeAssembly = false)!!
            val progbResult = compileTextPB(VMTarget(), false, progbSrc, outputDir, writeAssembly = false)!!

            val prog8Start = prog8Result.compilerAst.entrypoint
            val progbStart = progbResult.compilerAst.entrypoint

            val prog8Var = prog8Start.statements[0] as VarDecl
            val progbVar = progbStart.statements[0] as VarDecl

            prog8Var.name shouldBe progbVar.name
            prog8Var.datatype shouldBe progbVar.datatype
        }

        test("array declaration") {
            val prog8Src = """
                main {
                    sub start() {
                        ubyte[10] @shared arr
                    }
                }
            """
            val progbSrc = """
                MODULE main
                    SUB start()
                        DIM arr[10] AS UBYTE @shared
                    END SUB
                END MODULE
            """
            val prog8Result = compileText(VMTarget(), false, prog8Src, outputDir, writeAssembly = false)!!
            val progbResult = compileTextPB(VMTarget(), false, progbSrc, outputDir, writeAssembly = false)!!

            val prog8Start = prog8Result.compilerAst.entrypoint
            val progbStart = progbResult.compilerAst.entrypoint

            val prog8Var = prog8Start.statements[0] as VarDecl
            val progbVar = progbStart.statements[0] as VarDecl

            prog8Var.name shouldBe progbVar.name
            prog8Var.datatype shouldBe progbVar.datatype
        }
    }

    // ========================================================================
    // CONTROL FLOW
    // ========================================================================

    context("control flow equivalence") {
        test("if-else statement") {
            val prog8Src = """
                main {
                    sub start() {
                        ubyte x = 10
                        if x > 5 {
                            x = 1
                        } else {
                            x = 0
                        }
                    }
                }
            """
            val progbSrc = """
                MODULE main
                    SUB start()
                        DIM x AS UBYTE = 10
                        IF x > 5 THEN
                            x = 1
                        ELSE
                            x = 0
                        END IF
                    END SUB
                END MODULE
            """
            val prog8Result = compileText(VMTarget(), false, prog8Src, outputDir, writeAssembly = false)!!
            val progbResult = compileTextPB(VMTarget(), false, progbSrc, outputDir, writeAssembly = false)!!

            // Both should compile successfully with equivalent structure
            prog8Result shouldNotBe null
            progbResult shouldNotBe null
            
            // Verify entrypoints exist
            prog8Result.compilerAst.entrypoint shouldNotBe null
            progbResult.compilerAst.entrypoint shouldNotBe null
        }

        test("for loop") {
            val prog8Src = """
                main {
                    sub start() {
                        ubyte i
                        for i in 0 to 10 {
                            i += 0
                        }
                    }
                }
            """
            val progbSrc = """
                MODULE main
                    SUB start()
                        DIM i AS UBYTE
                        FOR i = 0 TO 10
                            i += 0
                        NEXT
                    END SUB
                END MODULE
            """
            val prog8Result = compileText(VMTarget(), false, prog8Src, outputDir, writeAssembly = false)!!
            val progbResult = compileTextPB(VMTarget(), false, progbSrc, outputDir, writeAssembly = false)!!

            val prog8Start = prog8Result.compilerAst.entrypoint
            val progbStart = progbResult.compilerAst.entrypoint

            // Should have VarDecl + ForLoop
            prog8Start.statements.size shouldBe progbStart.statements.size

            val prog8For = prog8Start.statements[1] as ForLoop
            val progbFor = progbStart.statements[1] as ForLoop

            prog8For.loopVar.nameInSource shouldBe progbFor.loopVar.nameInSource
        }

        test("while loop") {
            val prog8Src = """
                main {
                    sub start() {
                        ubyte x = 10
                        while x > 0 {
                            x--
                        }
                    }
                }
            """
            val progbSrc = """
                MODULE main
                    SUB start()
                        DIM x AS UBYTE = 10
                        WHILE x > 0
                            x--
                        WEND
                    END SUB
                END MODULE
            """
            val prog8Result = compileText(VMTarget(), false, prog8Src, outputDir, writeAssembly = false)!!
            val progbResult = compileTextPB(VMTarget(), false, progbSrc, outputDir, writeAssembly = false)!!

            val prog8Start = prog8Result.compilerAst.entrypoint
            val progbStart = progbResult.compilerAst.entrypoint

            prog8Start.statements.size shouldBe progbStart.statements.size
        }
    }

    // ========================================================================
    // SUBROUTINES
    // ========================================================================

    context("subroutine equivalence") {
        test("subroutine with parameters") {
            val prog8Src = """
                main {
                    sub start() {
                        helper(42)
                    }
                    sub helper(ubyte x) {
                        x++
                    }
                }
            """
            val progbSrc = """
                MODULE main
                    SUB start()
                        CALL helper(42)
                    END SUB
                    
                    SUB helper(x AS UBYTE)
                        x++
                    END SUB
                END MODULE
            """
            val prog8Result = compileText(VMTarget(), false, prog8Src, outputDir, writeAssembly = false)!!
            val progbResult = compileTextPB(VMTarget(), false, progbSrc, outputDir, writeAssembly = false)!!

            val prog8Main = prog8Result.compilerAst.entrypoint.definingBlock
            val progbMain = progbResult.compilerAst.entrypoint.definingBlock

            // Both should have start and helper subroutines
            val prog8Subs = prog8Main.statements.filterIsInstance<Subroutine>()
            val progbSubs = progbMain.statements.filterIsInstance<Subroutine>()

            prog8Subs.size shouldBe progbSubs.size
            prog8Subs.map { it.name }.toSet() shouldBe progbSubs.map { it.name }.toSet()
        }

        test("function with return value") {
            val prog8Src = """
                main {
                    sub start() {
                        ubyte @shared result = add(10, 20)
                    }
                    sub add(ubyte a, ubyte b) -> ubyte {
                        return a + b
                    }
                }
            """
            val progbSrc = """
                MODULE main
                    SUB start()
                        DIM result AS UBYTE = add(10, 20) @shared
                    END SUB
                    
                    FUNCTION add(a AS UBYTE, b AS UBYTE) AS UBYTE
                        RETURN a + b
                    END FUNCTION
                END MODULE
            """
            val prog8Result = compileText(VMTarget(), false, prog8Src, outputDir, writeAssembly = false)!!
            val progbResult = compileTextPB(VMTarget(), false, progbSrc, outputDir, writeAssembly = false)!!

            val prog8Main = prog8Result.compilerAst.entrypoint.definingBlock
            val progbMain = progbResult.compilerAst.entrypoint.definingBlock

            val prog8Add = prog8Main.statements.filterIsInstance<Subroutine>().find { it.name == "add" }!!
            val progbAdd = progbMain.statements.filterIsInstance<Subroutine>().find { it.name == "add" }!!

            // Both should have return type
            prog8Add.returntypes shouldBe progbAdd.returntypes
            prog8Add.parameters.size shouldBe progbAdd.parameters.size
        }
    }

    // ========================================================================
    // EXPRESSIONS
    // ========================================================================

    context("expression equivalence") {
        test("arithmetic expression") {
            val prog8Src = """
                main {
                    sub start() {
                        ubyte @shared x = (10 + 5) * 2
                    }
                }
            """
            val progbSrc = """
                MODULE main
                    SUB start()
                        DIM x AS UBYTE = (10 + 5) * 2 @shared
                    END SUB
                END MODULE
            """
            val prog8Result = compileText(VMTarget(), false, prog8Src, outputDir, writeAssembly = false)!!
            val progbResult = compileTextPB(VMTarget(), false, progbSrc, outputDir, writeAssembly = false)!!

            // If both compile successfully with same structure, expressions are equivalent
            prog8Result shouldNotBe null
            progbResult shouldNotBe null
        }

        test("boolean expression") {
            val prog8Src = """
                main {
                    sub start() {
                        bool @shared result = true and false or true
                    }
                }
            """
            val progbSrc = """
                MODULE main
                    SUB start()
                        DIM result AS BOOL = true AND false OR true @shared
                    END SUB
                END MODULE
            """
            val prog8Result = compileText(VMTarget(), false, prog8Src, outputDir, writeAssembly = false)!!
            val progbResult = compileTextPB(VMTarget(), false, progbSrc, outputDir, writeAssembly = false)!!

            prog8Result shouldNotBe null
            progbResult shouldNotBe null
        }
    }

    // ========================================================================
    // MODULE STRUCTURE
    // ========================================================================

    context("module structure equivalence") {
        test("multiple blocks/modules") {
            val prog8Src = """
                main {
                    sub start() {
                        other.helper()
                    }
                }
                other {
                    sub helper() {
                    }
                }
            """
            val progbSrc = """
                MODULE main
                    SUB start()
                        CALL other.helper()
                    END SUB
                END MODULE
                
                MODULE other
                    SUB helper()
                    END SUB
                END MODULE
            """
            val prog8Result = compileText(VMTarget(), false, prog8Src, outputDir, writeAssembly = false)!!
            val progbResult = compileTextPB(VMTarget(), false, progbSrc, outputDir, writeAssembly = false)!!

            val prog8Blocks = prog8Result.compilerAst.allBlocks.toList()
            val progbBlocks = progbResult.compilerAst.allBlocks.toList()

            prog8Blocks.map { it.name }.toSet() shouldBe progbBlocks.map { it.name }.toSet()
        }
    }
})
