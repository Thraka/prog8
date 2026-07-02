IMPORT textio
OPTION enable_floats

MODULE main
    PRIVATE CONST MAX AS UBYTE = 42
    PRIVATE ALIAS printer = txt.print

    PRIVATE TYPE Point
        x AS UBYTE
        y AS UBYTE
    END TYPE

    SUB start()
        PRIVATE DIM counter AS UBYTE = 0

        counter = compute(10, 20)
        printer("counter=")
        txt.print_ub(counter)
        txt.nl()

        DIM p AS PTR Point = memory("point", SIZEOF(Point), 0)
        p.x = MAX
        p.y = counter
        txt.print_ub(p.x + p.y)
        txt.nl()

        blink()
    END SUB

    PRIVATE FUNCTION compute(a AS UBYTE, b AS UBYTE) AS UBYTE
        PRIVATE DIM total AS UBYTE = a + b
        IF total > MAX THEN total = MAX
        RETURN total
    END FUNCTION

    PRIVATE ASMSUB blink() CLOBBERS(A)
        ASM
            lda  #0
            sta  $d020
            rts
        END ASM
    END ASMSUB

END MODULE
