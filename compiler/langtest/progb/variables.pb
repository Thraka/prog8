' Variable declarations test in ProgB
IMPORT textio
ZEROPAGE basicsafe

MODULE main
    DIM counter AS UBYTE = 0
    DIM buffer[10] AS UBYTE
    CONST MAX_VALUE AS UBYTE = 100
    
    SUB start()
        DIM x AS UBYTE = 5
        DIM y AS UBYTE = 10
        DIM result AS UWORD
        
        result = x + y AS UWORD
        counter = x
        
        txt.print("variables test\n")
    END SUB
END MODULE
