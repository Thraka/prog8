%zeropage basicsafe

main {
    ubyte @shared r1
    ubyte @shared r2

    sub start() {
        ubyte a, b = 10, 20
        a, b = 30, 40
        r1 = a
        r2 = b
    }
}
