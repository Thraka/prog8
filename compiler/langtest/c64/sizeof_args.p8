%zeropage basicsafe

main {
    ubyte x
    ubyte @shared r1
    ubyte @shared r2
    ubyte @shared r3
    ubyte @shared r4
    ubyte @shared r5

    sub start() {
        r1 = sizeof(ubyte)
        r2 = sizeof(x)
        r3 = sizeof(&x)
        r4 = sizeof(^^ubyte)
        r5 = sizeof(&&x)
        r1 = sizeof(&x)
        r2 = sizeof(&&x)
    }
}
