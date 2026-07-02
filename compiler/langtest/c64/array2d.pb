ZEROPAGE basicsafe

MODULE main
    DIM grid[4][3] AS UBYTE
    DIM result AS UBYTE @SHARED

    SUB start()
        grid[1][2] = 9
        result = grid[1][2]
    END SUB
END MODULE
