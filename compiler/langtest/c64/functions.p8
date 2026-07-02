; Functions test in Prog8
%import textio
%zeropage basicsafe

main {
    sub start() {
        ubyte result
        
        greet()
        result = add(5, 3)
        txt.print_ub(result)
        txt.nl()
        
        result = double_it(10)
        txt.print_ub(result)
        txt.nl()
    }
    
    sub greet() {
        txt.print("hello from greet\n")
    }
    
    sub add(ubyte a, ubyte b) -> ubyte {
        return a + b
    }
    
    sub double_it(ubyte x) -> ubyte {
        return x << 1
    }
}
