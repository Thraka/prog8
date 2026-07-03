' Test const interactions with memory slabs
' Target: C64

IMPORT textio

ZEROPAGE basicsafe

MODULE main
    ' Test const with memory slab references
    CONST mem1 AS UWORD = memory("mem1", 10, 0)
    CONST mem2 AS UWORD = memory("mem2", 10, 0)
    
    ' Test array initializer with const memory references
    DIM arr[2] AS UWORD = [mem1, mem2]
    
    SUB start()
        IF arr[0] = mem1 THEN
            txt.print("pass: const memory in array")
            txt.nl()
        END IF
        
        IF arr[1] = mem2 THEN
            txt.print("pass: const memory array elem")
            txt.nl()
        END IF
    END SUB

END MODULE
