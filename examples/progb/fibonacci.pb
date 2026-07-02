' ProgB Fibonacci Sequence
' This is a BASIC-style syntax example for Prog8

IMPORT textio
ZEROPAGE basicsafe

MODULE main

    SUB start()
        txt.print("Fibonacci sequence")
        txt.nl()
        txt.nl()

        DIM fib_prev AS UWORD = 0
        DIM fib_curr AS UWORD = 1
        DIM fib_next AS UWORD
        DIM i AS UBYTE

        txt.print_uw(fib_prev)
        txt.nl()
        txt.print_uw(fib_curr)
        txt.nl()

        FOR i = 1 TO 23
            fib_next = fib_prev + fib_curr
            txt.print_uw(fib_next)
            txt.nl()
            fib_prev = fib_curr
            fib_curr = fib_next
        NEXT

        txt.nl()
        txt.print("Done!")
        txt.nl()
    END SUB

END MODULE
