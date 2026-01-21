' ProgB Border Color Animation with Inline Assembly
' This is a BASIC-style syntax example for Prog8

ZEROPAGE basicsafe

MODULE main

    SUB start()
        DIM color AS UBYTE = 0
        
        DO
            POKE $D020, color   ' or: @($D020) = color
            color++
            
            ' Low-level wait using assembly
            ASM
                lda #$10
            -   cmp $d012
                bne -
            END ASM
        LOOP
    END SUB

END MODULE
