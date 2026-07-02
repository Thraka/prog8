ZEROPAGE basicsafe

MODULE main
    DIM r1 AS UBYTE @SHARED
    DIM r2 AS UBYTE @SHARED

    SUB start()
        DIM a, b AS UBYTE = 10, 20
        a, b = 30, 40
        r1 = a
        r2 = b
    END SUB
END MODULE
