' Test integer literal parsing
IMPORT textio

MODULE main
    SUB start()
        DIM counter AS UBYTE = 0
        DIM other AS UWORD = 123
        txt.print("counter = ")
        txt.print_ub(counter)
        txt.nl()
        txt.print("other = ")
        txt.print_uw(other)
        txt.nl()
    END SUB
END MODULE
