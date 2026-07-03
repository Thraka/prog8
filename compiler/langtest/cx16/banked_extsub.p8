; Test asmsub/extsub signature parity
; Target: CX16

%import textio

main {
    ; Declare external ROM routines
    extsub $FFD2 = chrout(ubyte c @A)
    extsub $FFE4 = getin() -> ubyte @A
    
    sub start() {
        ; Test extsub calls with register parameters and return values
        chrout(65)  ; Print 'A'
        txt.nl()
        
        ubyte key = getin()  ; Get key - has return value
        txt.print("pass: extsub signatures")
        txt.nl()
    }
}
