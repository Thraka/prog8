package prog8tests.progb

import io.kotest.core.spec.style.FunSpec
import io.kotest.engine.spec.tempdir
import io.kotest.matchers.shouldBe
import io.kotest.matchers.shouldNotBe
import prog8.code.target.C64Target
import prog8.code.target.Cx16Target
import prog8.code.target.VMTarget
import prog8tests.helpers.ErrorReporterForTests
import prog8tests.helpers.compileTextPB

/**
 * Tests that verify all ProgB syntax constructs parse and compile correctly.
 * These tests ensure the ProgB grammar and AST visitor handle all language features.
 */
class TestProgBSyntax : FunSpec({
    val outputDir = tempdir().toPath()

    // ========================================================================
    // BASIC STRUCTURE
    // ========================================================================

    context("module structure") {
        test("minimal module") {
            val src = """
                MODULE main
                    SUB start()
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("module with multiple subroutines") {
            val src = """
                MODULE main
                    SUB start()
                        CALL helper()
                    END SUB
                    
                    SUB helper()
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("multiple modules") {
            val src = """
                MODULE main
                    SUB start()
                        other.foo()
                    END SUB
                END MODULE
                
                MODULE other
                    SUB foo()
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }
    }

    // ========================================================================
    // VARIABLE DECLARATIONS (DIM)
    // ========================================================================

    context("DIM statements") {
        test("basic types") {
            val src = """
                MODULE main
                    SUB start()
                        DIM a AS UBYTE
                        DIM b AS BYTE
                        DIM c AS UWORD
                        DIM d AS WORD
                        DIM e AS LONG
                        DIM f AS BOOL
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("with initialization") {
            val src = """
                MODULE main
                    SUB start()
                        DIM x AS UBYTE = 42
                        DIM y AS WORD = -100
                        DIM flag AS BOOL = true
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("string type") {
            val src = """
                MODULE main
                    SUB start()
                        DIM name AS STRING = "hello"
                        DIM buffer AS STRING = "?" * 80
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("float type") {
            val src = """
                IMPORT floats
                MODULE main
                    SUB start()
                        DIM pi AS FLOAT = 3.14159
                    END SUB
                END MODULE
            """
            compileTextPB(C64Target(), false, src, outputDir) shouldNotBe null
        }

        test("arrays") {
            val src = """
                MODULE main
                    SUB start()
                        DIM arr[10] AS UBYTE
                        DIM initialized[5] AS UBYTE = [1, 2, 3, 4, 5]
                        DIM dynamic[] AS UWORD = [100, 200, 300]
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("multiple variables in one DIM") {
            val src = """
                MODULE main
                    SUB start()
                        DIM x, y, z AS UBYTE
                        DIM a, b AS UWORD = 0
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("with tags") {
            val src = """
                MODULE main
                    DIM shared_var AS UBYTE @shared
                    DIM zp_var AS UBYTE @zp
                    SUB start()
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("with AT address") {
            val src = """
                MODULE main
                    DIM screen AS UBYTE @shared AT $0400
                    SUB start()
                    END SUB
                END MODULE
            """
            compileTextPB(C64Target(), false, src, outputDir) shouldNotBe null
        }

        test("pointers") {
            val src = """
                MODULE main
                    SUB start()
                        DIM p AS PTR UBYTE
                        DIM q AS PTR UWORD
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }
    }

    // ========================================================================
    // CONST DECLARATIONS
    // ========================================================================

    context("CONST statements") {
        test("basic constants") {
            val src = """
                MODULE main
                    CONST MAX_SIZE AS UBYTE = 100
                    CONST MAX_COUNT AS UWORD = 1000
                    SUB start()
                        DIM x AS UBYTE = MAX_SIZE
                    END SUB
                END MODULE
            """
            compileTextPB(C64Target(), false, src, outputDir) shouldNotBe null
        }
    }

    // ========================================================================
    // CONTROL FLOW
    // ========================================================================

    context("IF statements") {
        test("simple IF THEN") {
            val src = """
                MODULE main
                    SUB start()
                        DIM x AS UBYTE = 10
                        IF x > 5 THEN
                            x = 0
                        END IF
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("IF THEN ELSE") {
            val src = """
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
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("IF THEN ELSEIF ELSE") {
            val src = """
                MODULE main
                    SUB start()
                        DIM x AS UBYTE = 10
                        IF x > 10 THEN
                            x = 2
                        ELSEIF x > 5 THEN
                            x = 1
                        ELSE
                            x = 0
                        END IF
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("nested IF") {
            val src = """
                MODULE main
                    SUB start()
                        DIM x AS UBYTE = 10
                        DIM y AS UBYTE = 5
                        IF x > 5 THEN
                            IF y > 3 THEN
                                x = 0
                            END IF
                        END IF
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }
    }

    context("FOR loops") {
        test("basic FOR NEXT") {
            val src = """
                MODULE main
                    SUB start()
                        DIM i AS UBYTE
                        FOR i = 0 TO 10
                            i += 0  ' keep optimizer happy
                        NEXT
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("FOR with STEP") {
            val src = """
                MODULE main
                    SUB start()
                        DIM i AS UBYTE
                        FOR i = 0 TO 100 STEP 10
                            i += 0
                        NEXT
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("FOR DOWNTO") {
            val src = """
                MODULE main
                    SUB start()
                        DIM i AS UBYTE
                        FOR i = 10 DOWNTO 0
                            i += 0
                        NEXT
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }
    }

    context("WHILE loops") {
        test("basic WHILE WEND") {
            val src = """
                MODULE main
                    SUB start()
                        DIM x AS UBYTE = 10
                        WHILE x > 0
                            x -= 1
                        WEND
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }
    }

    context("DO loops") {
        test("DO LOOP UNTIL") {
            val src = """
                MODULE main
                    SUB start()
                        DIM x AS UBYTE = 0
                        DO
                            x += 1
                        LOOP UNTIL x = 10
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }
    }

    context("REPEAT loops") {
        test("REPEAT with count") {
            val src = """
                MODULE main
                    SUB start()
                        DIM x AS UBYTE = 0 @shared
                        REPEAT 10
                            x += 1
                        END REPEAT
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }
    }

    context("SELECT CASE") {
        test("basic SELECT CASE") {
            val src = """
                MODULE main
                    SUB start()
                        DIM x AS UBYTE = 2 @shared
                        SELECT CASE x
                            CASE 1
                                x = 10
                            CASE 2
                                x = 20
                            CASE ELSE
                                x = 0
                        END SELECT
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("SELECT CASE with ranges") {
            val src = """
                MODULE main
                    SUB start()
                        DIM x AS UBYTE = 5 @shared
                        SELECT CASE x
                            CASE 1 TO 5
                                x = 10
                            CASE 6, 7, 8
                                x = 20
                            CASE ELSE
                                x = 0
                        END SELECT
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }
    }

    // ========================================================================
    // SUBROUTINES AND FUNCTIONS
    // ========================================================================

    context("subroutines") {
        test("SUB with parameters") {
            val src = """
                MODULE main
                    SUB start()
                        CALL greet(42)
                    END SUB
                    
                    SUB greet(x AS UBYTE)
                        x += 0
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("FUNCTION with return type") {
            val src = """
                MODULE main
                    SUB start()
                        DIM result AS UBYTE @shared
                        result = add(10, 20)
                    END SUB
                    
                    FUNCTION add(a AS UBYTE, b AS UBYTE) AS UBYTE
                        RETURN a + b
                    END FUNCTION
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("optional CALL keyword") {
            val src = """
                MODULE main
                    SUB start()
                        CALL helper()   ' with CALL
                        helper()        ' without CALL
                    END SUB
                    
                    SUB helper()
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }
    }

    // ========================================================================
    // EXPRESSIONS
    // ========================================================================

    context("expressions") {
        test("arithmetic operators") {
            val src = """
                MODULE main
                    SUB start()
                        DIM x AS UBYTE @shared
                        x = 10 + 5
                        x = 10 - 5
                        x = 10 * 5
                        x = 10 / 5
                        x = 10 MOD 3
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("comparison operators") {
            val src = """
                MODULE main
                    SUB start()
                        DIM x AS UBYTE = 10
                        DIM b AS BOOL @shared
                        b = x = 10      ' equality (note: = is used for both)
                        b = x <> 5      ' not equal
                        b = x < 20
                        b = x > 5
                        b = x <= 10
                        b = x >= 10
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("logical operators") {
            val src = """
                MODULE main
                    SUB start()
                        DIM a AS BOOL = true
                        DIM b AS BOOL = false
                        DIM c AS BOOL @shared
                        c = a AND b
                        c = a OR b
                        c = a XOR b
                        c = NOT a
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("bitwise operators") {
            val src = """
                MODULE main
                    SUB start()
                        DIM x AS UBYTE = ${'$'}FF @shared
                        x = x BITAND ${'$'}0F
                        x = x BITOR ${'$'}F0
                        x = x BITXOR ${'$'}AA
                        x = BITNOT x
                        x = x SHL 2
                        x = x SHR 2
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("augmented assignment") {
            val src = """
                MODULE main
                    SUB start()
                        DIM x AS UBYTE = 10 @shared
                        x += 5
                        x -= 3
                        x *= 2
                        x /= 2
                        x &= ${'$'}0F
                        x |= ${'$'}F0
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("increment and decrement") {
            val src = """
                MODULE main
                    SUB start()
                        DIM x AS UBYTE = 10 @shared
                        x++
                        x--
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("string multiplication") {
            val src = """
                MODULE main
                    DIM buffer AS STRING = "?" * 160
                    SUB start()
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("ranges in expressions") {
            val src = """
                MODULE main
                    SUB start()
                        DIM arr[] AS UBYTE = 1 TO 10
                        DIM arr2[] AS UBYTE = 10 DOWNTO 1
                        DIM arr3[] AS UBYTE = 0 TO 100 STEP 10
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }
    }

    // ========================================================================
    // MEMORY OPERATIONS
    // ========================================================================

    context("memory operations") {
        test("PEEK and POKE") {
            val src = """
                MODULE main
                    SUB start()
                        DIM x AS UBYTE @shared
                        POKE ${'$'}D020, 0
                        x = PEEK(${'$'}D020)
                    END SUB
                END MODULE
            """
            compileTextPB(C64Target(), false, src, outputDir) shouldNotBe null
        }

        test("address-of operators") {
            val src = """
                MODULE main
                    DIM myvar AS UBYTE @shared
                    SUB start()
                        DIM addr AS UWORD @shared
                        addr = &myvar
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("direct memory access") {
            val src = """
                MODULE main
                    SUB start()
                        DIM x AS UBYTE @shared
                        @(${'$'}D020) = 0
                        x = @(${'$'}D020)
                    END SUB
                END MODULE
            """
            compileTextPB(C64Target(), false, src, outputDir) shouldNotBe null
        }
    }

    // ========================================================================
    // DIRECTIVES
    // ========================================================================

    context("directives") {
        test("IMPORT") {
            val src = """
                IMPORT textio
                MODULE main
                    SUB start()
                        txt.print("hello")
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("OPTION") {
            val src = """
                OPTION no_sysinit
                MODULE main
                    SUB start()
                    END SUB
                END MODULE
            """
            compileTextPB(C64Target(), false, src, outputDir) shouldNotBe null
        }

        test("ZEROPAGE") {
            val src = """
                ZEROPAGE basicsafe
                MODULE main
                    SUB start()
                    END SUB
                END MODULE
            """
            compileTextPB(C64Target(), false, src, outputDir) shouldNotBe null
        }
    }

    // ========================================================================
    // COMMENTS
    // ========================================================================

    context("comments") {
        test("apostrophe comments") {
            val src = """
                MODULE main
                    ' This is a comment
                    SUB start()
                        DIM x AS UBYTE = 10 ' inline comment
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("REM comments") {
            val src = """
                MODULE main
                    REM This is a REM comment
                    SUB start()
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("block comments") {
            val src = """
                MODULE main
                    /'
                    This is a
                    block comment
                    '/
                    SUB start()
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }
    }

    // ========================================================================
    // INLINE ASSEMBLY
    // ========================================================================

    context("inline assembly") {
        test("ASM block") {
            val src = """
                MODULE main
                    SUB start()
                        ASM
                            nop
                            nop
                        END ASM
                    END SUB
                END MODULE
            """
            compileTextPB(C64Target(), false, src, outputDir) shouldNotBe null
        }
    }

    // ========================================================================
    // CASE INSENSITIVITY
    // ========================================================================

    context("case insensitivity") {
        test("keywords can be any case") {
            val src = """
                module main
                    sub start()
                        dim x as ubyte = 10
                        if x > 5 then
                            x = 0
                        end if
                    end sub
                end module
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }

        test("mixed case keywords") {
            val src = """
                Module main
                    Sub start()
                        Dim x As UByte = 10
                        If x > 5 Then
                            x = 0
                        End If
                    End Sub
                End Module
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }
    }

    // ========================================================================
    // STRUCT TYPES
    // ========================================================================

    context("struct types") {
        test("TYPE definition") {
            val src = """
                MODULE main
                    TYPE Point
                        x AS UBYTE
                        y AS UBYTE
                    END TYPE
                    
                    SUB start()
                        DIM p AS PTR Point
                        p = memory("point", sizeof(Point), 0)
                        p^^.x = 10
                        p^^.y = 20
                    END SUB
                END MODULE
            """
            compileTextPB(VMTarget(), false, src, outputDir) shouldNotBe null
        }
    }
})
