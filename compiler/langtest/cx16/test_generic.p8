%import textio
%option enable_floats

main {
    sub start() {
        bool returnVal = isTrue(1)

        txt.print_bool(returnVal)
    }

    sub isTrue(ubyte val) -> bool {
        
        txt.print("isTrue called with value: ")
        txt.print_ub(val)
        txt.nl()

        if val == 1 return false
        return true
    }
}
