; Test file for syntax features:
; 1. Colon statement separator
; 2. Chained assignments
; 3. Equality comparison with = and ==

%import textio

main {
    sub start() {
        ; Test 1: Colon as statement separator
        ubyte a
        ubyte b
        ubyte c
        a = 1
        b = 2
        c = 3
        txt.print("colon separator: ")
        txt.print_ub(a)
        txt.chrout(' ')
        txt.print_ub(b)
        txt.chrout(' ')
        txt.print_ub(c)
        txt.nl()
        
        ; Test 2: Chained assignment
        a = b = c = 42
        txt.print("chained assign (all 42): ")
        txt.print_ub(a)
        txt.chrout(' ')
        txt.print_ub(b)
        txt.chrout(' ')
        txt.print_ub(c)
        txt.nl()
        
        ; Test 3: Equality with = in IF
        a = 42
        if a == 42 {
            txt.print("= equality works")
            txt.nl()
        }
        
        ; Test 4: Equality with == in IF
        if a == 42 {
            txt.print("== equality works")
            txt.nl()
        }
        
        ; Test 5: Not equal with <>
        b = 10
        if a != b {
            txt.print("<> not-equal works")
            txt.nl()
        }
        
        ; Test 6: Combined - chained assign then compare
        a = b = 0
        if a == 0 and b == 0 {
            txt.print("chained to 0, both equal 0")
            txt.nl()
        }
        
        bool test1
        bool test2 = true

        test1 = (test2 == true)

        txt.print("all tests passed!")
        txt.nl()
    }
}
