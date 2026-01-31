' Test file for syntax features:
' 1. Colon statement separator
' 2. Chained assignments
' 3. Equality comparison with = and ==

IMPORT textio
OPTION enable_floats

MODULE main
    SUB start()
        ' Test 1: Colon as statement separator
        DIM a AS UBYTE
        DIM b AS UBYTE
        DIM c AS UBYTE
        a = 1
        b = 2
        c = 3
        txt.print("colon separator: ")
        txt.print_ub(a)
        txt.chrout(" "c)
        txt.print_ub(b)
        txt.chrout(" "c)
        txt.print_ub(c)
        txt.nl()
        
        ' Test 2: Chained assignment
        a = b = c = 42
        txt.print("chained assign (all 42): ")
        txt.print_ub(a)
        txt.chrout(" "c)
        txt.print_ub(b)
        txt.chrout(" "c)
        txt.print_ub(c)
        txt.nl()
        
        ' Test 3: Equality with = in IF
        a = 42
        IF a = 42 THEN
            txt.print("= equality works")
            txt.nl()
        END IF
        
        ' Test 4: Equality with == in IF
        IF a == 42 THEN
            txt.print("== equality works")
            txt.nl()
        END IF
        
        ' Test 5: Not equal with <>
        b = 10
        IF a <> b THEN
            txt.print("<> not-equal works")
            txt.nl()
        END IF
        
        ' Test 6: Combined - chained assign then compare
        a = b = 0
        IF a = 0 AND b = 0 THEN
            txt.print("chained to 0, both equal 0")
            txt.nl()
        END IF
        
        DIM test1 AS BOOL = TRUE
        DIM test2 AS BOOL = FALSE

        test1 = (test2 = TRUE)
        ' Test 7: Test boolean assignment and comparison
        IF NOT test1 THEN
            test1 = FALSE
            txt.print("boolean assign and compare works")
            txt.nl()
        END IF

        test1 = IIF test2 THEN TRUE ELSE FALSE
        ' Test 8: Test IIF assignment
        IF NOT test1 THEN
            txt.print("Checking IIF works")
            txt.nl()
        END IF

        txt.print("all tests passed!")
        txt.nl()

        ' Expressions (read from memory)
        DIM w AS UWORD = PEEKW($1000)     ' read word from address
        DIM l AS LONG = PEEKL($2000)      ' read long from address
        DIM bo AS BOOL = PEEKBOOL($3000)   ' read bool from address
        DIM f AS FLOAT = PEEKF($4000)     ' read float from address

        ' Statements (write to memory)
        POKEW $1000, w                    ' write word to address
        POKEL $2000, l                    ' write long to address
        POKEBOOL $3000, bo                 ' write bool to address
        'POKEF $4000, f   

    END SUB
END MODULE
