%import textio
%option enable_floats

main {
    sub start() {
        bool returnVal = isTrue(1)

        defer txt.print("Return value: ")
        defer {
            txt.print("Return value: ")
            txt.print("Return value: ")
        }

        txt.print_bool(returnVal)

        returnVal = if_cc true else false

        if_cs txt.print_b(11)
        if_cs txt.print_b(12) else txt.print_b(13)

        if_cs {
            txt.print_b(14)
        }

        if_cs {
            txt.print_b(15) 
        } else {
            txt.print_b(16)
            txt.print_b(17)
        }
    }

    sub isTrue(ubyte val) -> bool {
        
        txt.print("isTrue called with value: ")
        txt.print_ub(val)
        txt.nl()

        if val == 1 return false
        return true
    }
}
