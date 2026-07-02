%import textio
%option enable_floats

main {
    private const ubyte MAX = 42
    private alias printer = txt.print

    private struct Point {
        ubyte x
        ubyte y
    }

    sub start() {
        private ubyte counter = 0

        counter = compute(10, 20)
        printer("counter=")
        txt.print_ub(counter)
        txt.nl()

        ^^Point p = memory("point", sizeof(Point), 0)
        p.x = MAX
        p.y = counter
        txt.print_ub(p.x + p.y)
        txt.nl()

        blink()
    }

    private sub compute(ubyte a, ubyte b) -> ubyte {
        private ubyte total = a + b
        if total > MAX
            total = MAX
        return total
    }

    private asmsub blink() clobbers(A) {
        %asm {{
            lda  #0
            sta  $d020
            rts
        }}
    }
}
