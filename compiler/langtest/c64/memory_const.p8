; Test const interactions with memory slabs
; Target: C64

%import textio

%zeropage basicsafe

main {
    ; Test const with memory slab references
    const uword mem1 = memory("mem1", 10, 0)
    const uword mem2 = memory("mem2", 10, 0)
    
    ; Test array initializer with const memory references
    uword[2] arr = [mem1, mem2]
    
    sub start() {
        if arr[0] == mem1 {
            txt.print("pass: const memory in array")
            txt.nl()
        }
        
        if arr[1] == mem2 {
            txt.print("pass: const memory array elem")
            txt.nl()
        }
    }
}
