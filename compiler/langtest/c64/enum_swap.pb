ZEROPAGE basicsafe

MODULE main
    ENUM Mode
        OFF = 1
        ON
    END ENUM

    DIM resultA AS UBYTE @SHARED
    DIM resultB AS UBYTE @SHARED

    SUB start()
        DIM a AS UBYTE = Mode::ON
        DIM b AS UBYTE = Mode::OFF
        SWAP(a, b)
        resultA = a
        resultB = b
    END SUB
END MODULE
