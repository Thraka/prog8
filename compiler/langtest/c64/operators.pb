' Operators test in ProgB
IMPORT textio
ZEROPAGE basicsafe

MODULE main
    SUB start()
        DIM a AS UBYTE = 10
        DIM b AS UBYTE = 3
        DIM c AS UBYTE
        DIM flag AS BOOL
        
        ' Arithmetic
        c = a + b
        c = a - b
        c = a * b
        c = a / b
        c = a MOD b
        
        ' Bitwise
        c = a BITAND b
        c = a BITOR b
        c = a BITXOR b
        c = BITNOT a
        c = a SHL 2
        c = a SHR 1
        
        ' Comparison
        flag = a = b
        flag = a <> b
        flag = a < b
        flag = a > b
        flag = a <= b
        flag = a >= b
        
        ' Logical
        flag = TRUE AND FALSE
        flag = TRUE OR FALSE
        flag = NOT flag
        
        ' Augmented assignment
        a += 5
        a -= 2
        a *= 2
        a &= $0F
        a |= $F0
        
        txt.print("operators test\n")
    END SUB
END MODULE
