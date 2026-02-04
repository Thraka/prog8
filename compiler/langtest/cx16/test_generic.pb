IMPORT textio
OPTION enable_floats

MODULE main
    SUB start()
        DIM returnVal AS BOOL = isTrue(1)

        txt.print_bool(returnVal)
    END SUB

    FUNCTION isTrue(val AS UBYTE) AS BOOL
        txt.print("isTrue called with value: ")
        txt.print_ub(val)
        txt.nl()

        IF val = 1 THEN RETURN FALSE
        RETURN TRUE
    END FUNCTION

END MODULE
