' ProgB Hello World
' The simplest example of ProgB syntax

IMPORT textio
ZEROPAGE basicsafe

MODULE main

    SUB start()
        txt.print("Hello, World from ProgB!")
        txt.nl()
        
        ' Demonstrate various ProgB features
        
        ' Variables with different types
        DIM counter AS UBYTE = 0
        DIM total AS UWORD = 0
        DIM message AS STRING = "ProgB is fun!"
        
        ' FOR loop
        txt.print("Counting: ")
        FOR counter = 1 TO 10
            txt.print_ub(counter)
            txt.spc()
            total += counter AS UWORD
        NEXT
        txt.nl()
        
        ' Show the sum
        txt.print("Sum: ")
        txt.print_uw(total)
        txt.nl()
        
        ' WHILE loop
        txt.print("Countdown: ")
        counter = 5
        WHILE counter > 0
            txt.print_ub(counter)
            txt.spc()
            counter--
        WEND
        txt.print("Liftoff!")
        txt.nl()
        
        ' IF/ELSE
        IF total > 50 THEN
            txt.print("Total is greater than 50")
        ELSE
            txt.print("Total is 50 or less")
        END IF
        txt.nl()
        
        txt.print(message)
        txt.nl()
    END SUB

END MODULE
