' When/Select Case test in ProgB
IMPORT textio
ZEROPAGE basicsafe

MODULE main
    SUB start()
        DIM x AS UBYTE
        
        FOR x = 0 TO 5
            SELECT CASE x
                CASE 0
                    txt.print("zero\n")
                CASE 1, 2
                    txt.print("one or two\n")
                CASE 3
                    txt.print("three\n")
                CASE ELSE
                    txt.print("other\n")
            END SELECT
        NEXT
    END SUB
END MODULE
