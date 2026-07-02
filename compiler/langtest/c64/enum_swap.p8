%zeropage basicsafe

main {
    enum Mode {
        OFF=1,
        ON
    }

    ubyte @shared resultA
    ubyte @shared resultB

    sub start() {
        ubyte a = Mode::ON
        ubyte b = Mode::OFF
        swap(a, b)
        resultA = a
        resultB = b
    }
}
