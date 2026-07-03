' Test banked extsub with register pairs and varied signatures
' Target: CX16

ZEROPAGE basicsafe
OPTION no_sysinit

IMPORT textio
IMPORT diskio

MODULE main
    ' Banked external routines with various signatures
    EXTSUB AT BANK 4   $A000 = lib_routine1(value AS UBYTE @A) CLOBBERS(X) AS UWORD @AY
    EXTSUB AT BANK 5   $A000 = lib_routine2(value AS UBYTE @A) CLOBBERS(X) AS UWORD @AY
    EXTSUB AT BANK 10  $C09F = audio_init() AS BOOL @A

    SUB start()
        ' Test banked extsub calls with return value handling
        
        ' Call with return value in register pair
        cx16.r0 = lib_routine1(11)
        txt.print_uw(cx16.r0)
        txt.nl()

        cx16.r0 = lib_routine2(99)
        txt.print_uw(cx16.r0)
        txt.nl()
        
        ' Call with boolean return
        DIM success AS BOOL = audio_init()
        IF success THEN
            txt.print("pass: banked extsub")
            txt.nl()
        END IF
    END SUB

END MODULE
