; Operators test in Prog8
%import textio
%zeropage basicsafe

main {
    sub start() {
        ubyte a = 10
        ubyte b = 3
        ubyte c
        bool flag
        
        ; Arithmetic
        c = a + b
        c = a - b
        c = a * b
        c = a / b
        c = a % b
        
        ; Bitwise
        c = a & b
        c = a | b
        c = a ^ b
        c = ~a
        c = a << 2
        c = a >> 1
        
        ; Comparison
        flag = a == b
        flag = a != b
        flag = a < b
        flag = a > b
        flag = a <= b
        flag = a >= b
        
        ; Logical
        flag = true and false
        flag = true or false
        flag = not flag
        
        ; Augmented assignment
        a += 5
        a -= 2
        a *= 2
        a &= $0f
        a |= $f0
        
        txt.print("operators test\n")
    }
}
