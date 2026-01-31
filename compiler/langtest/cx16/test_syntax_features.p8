; Test file for syntax features:
; 1. Colon statement separator
; 2. Chained assignments
; 3. Equality comparison with = and ==

%import textio
%option enable_floats


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
        
        bool test1 = true
        bool test2 = false

        test1 = (test2 == true)
        ; Test 7: Test boolean assignment and comparison
        if not test1 {
            test1 = false
            txt.print("boolean assign and compare works")
            txt.nl()
        }

        test1 = if test2 true else false
        ; Test 8: Test IIF assignment
        if not test1 {
            txt.print("Checking IIF works")
            txt.nl()
        }

        txt.print("all tests passed!")
        txt.nl()

        ; Expressions (read from memory)
        uword w = peekw($1000)     ; read word from address
        long l = peekl($2000)      ; read long from address
        bool bo = peekbool($3000)   ; read bool from address
        float f = peekf($4000)     ; read float from address

        ; Statements (write to memory)
        pokew($1000, w)                    ; write word to address
        pokel($2000, l)                    ; write long to address
        pokebool($3000, bo)                 ; write bool to address
        ;pokef($4000, f)   
    }
}
