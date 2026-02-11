IMPORT textio
OPTION enable_floats

MODULE main
    SUB start()
        DIM returnVal AS BOOL = isTrue(1)

        DEFER txt.print("Return value: ")
        DEFER
            txt.print("Return value: ")
            txt.print("Return value: ")
        END DEFER

        txt.print_bool(returnVal)

        returnVal = IF_CC THEN TRUE ELSE FALSE END IF

        IF_CS THEN txt.print_b(11) END IF
        IF_CS THEN txt.print_b(12) ELSE txt.print_b(13) END IF

        IF_CS THEN
            txt.print_b(14)
        END IF

        IF_CS THEN
            txt.print_b(15)
        ELSE
            txt.print_b(16)
            txt.print_b(17)
        END IF

    END SUB

    FUNCTION isTrue(val AS UBYTE) AS BOOL
        txt.print("isTrue called with value: ")
        txt.print_ub(val)
        txt.nl()

        IF val = 1 THEN RETURN FALSE
        RETURN TRUE
        
    END FUNCTION

END MODULE
