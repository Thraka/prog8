' Control flow test in ProgB
IMPORT textio
ZEROPAGE basicsafe

MODULE main
    SUB start()
        DIM i AS UBYTE
        DIM x AS UBYTE = 5
        
        ' For loop
        FOR i = 0 TO 9
            txt.print_ub(i)
        NEXT
        
        ' While loop
        WHILE x > 0
            x--
        WEND
        
        ' If statement
        IF x = 0 THEN
            txt.print("zero\n")
        ELSEIF x = 1 THEN
            txt.print("one\n")
        ELSE
            txt.print("other\n")
        END IF
        
        ' Do-until loop
        x = 3
        DO
            x--
        LOOP UNTIL x = 0
        
        ' Repeat loop
        REPEAT 5
            txt.chrout("*"c)
        END REPEAT
        
        txt.nl()
    END SUB
END MODULE
