' Test file for syntax features:
' 1. Colon statement separator
' 2. Chained assignments
' 3. Equality comparison with = and ==

IMPORT textio

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
        
        DIM test1 AS BOOL
        DIM test2 AS BOOL = true

        test1 = (test2 = true)

        txt.print("all tests passed!")
        txt.nl()
    END SUB
END MODULE
