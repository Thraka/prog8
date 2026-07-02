; Control flow test in Prog8
%import textio
%zeropage basicsafe

main {
    sub start() {
        ubyte i
        ubyte x = 5
        
        ; For loop
        for i in 0 to 9 {
            txt.print_ub(i)
        }
        
        ; While loop
        while x > 0 {
            x--
        }
        
        ; If statement
        if x == 0 {
            txt.print("zero\n")
        } else if x == 1 {
            txt.print("one\n")
        } else {
            txt.print("other\n")
        }
        
        ; Do-until loop
        x = 3
        do {
            x--
        } until x == 0
        
        ; Repeat loop
        repeat 5 {
            txt.chrout('*')
        }
        
        txt.nl()
    }
}
