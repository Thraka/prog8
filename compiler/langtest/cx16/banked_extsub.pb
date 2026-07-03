' Test asmsub/extsub signature parity
' Target: CX16

IMPORT textio

MODULE main
    ' Declare external ROM routines
    EXTSUB $FFD2 = chrout(c AS UBYTE @A)
    EXTSUB $FFE4 = getin() AS UBYTE @A
    
    SUB start()
        ' Test extsub calls with register parameters and return values
        CALL chrout(65)  ' Print 'A'
        txt.nl()
        
        DIM key AS UBYTE = getin()  ' Get key - has return value
        txt.print("pass: extsub signatures")
        txt.nl()
    END SUB

END MODULE
