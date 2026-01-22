; When/Select Case test in Prog8
%import textio
%zeropage basicsafe

main {
    sub start() {
        ubyte x
        
        for x in 0 to 5 {
            when x {
                0 -> txt.print("zero\n")
                1, 2 -> txt.print("one or two\n")
                3 -> txt.print("three\n")
                else -> txt.print("other\n")
            }
        }
    }
}
