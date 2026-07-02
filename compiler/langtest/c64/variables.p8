; Variable declarations test in Prog8
%import textio
%zeropage basicsafe

main {
    ubyte counter = 0
    ubyte[10] buffer
    const ubyte MAX_VALUE = 100
    
    sub start() {
        ubyte x = 5
        ubyte y = 10
        uword result
        
        result = x + y as uword
        counter = x
        
        txt.print("variables test\n")
    }
}
