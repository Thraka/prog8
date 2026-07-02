ZEROPAGE basicsafe

MODULE main
    DIM x AS UBYTE
    DIM r1 AS UBYTE @SHARED
    DIM r2 AS UBYTE @SHARED
    DIM r3 AS UBYTE @SHARED
    DIM r4 AS UBYTE @SHARED
    DIM r5 AS UBYTE @SHARED

    SUB start()
        r1 = SIZEOF(UBYTE)
        r2 = SIZEOF(x)
        r3 = SIZEOF(&x)
        r4 = SIZEOF(PTR UBYTE)
        r5 = SIZEOF(&&x)
        r1 = SIZEOF(ADDRESSOF(x))
        r2 = SIZEOF(TYPEDADDR(x))
    END SUB
END MODULE
