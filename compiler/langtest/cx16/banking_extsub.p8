; Test banked extsub with register pairs and varied signatures
; Target: CX16

%import textio
%import diskio

%option no_sysinit
%zeropage basicsafe

main {
    ; Banked external routines with various signatures
    extsub @bank 4   $A000 = lib_routine1(ubyte value @A) clobbers(X) -> uword @AY
    extsub @bank 5   $A000 = lib_routine2(ubyte value @A) clobbers(X) -> uword @AY
    extsub @bank 10  $C09F = audio_init() -> bool @A

    sub start() {
        ; Test banked extsub calls with return value handling
        
        ; Call with return value in register pair
        cx16.r0 = lib_routine1(11)
        txt.print_uw(cx16.r0)
        txt.nl()

        cx16.r0 = lib_routine2(99)
        txt.print_uw(cx16.r0)
        txt.nl()
        
        ; Call with boolean return
        bool success = audio_init()
        if success {
            txt.print("pass: banked extsub")
            txt.nl()
        }
    }
}
