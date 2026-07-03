; Test memory slab allocation and references
; Target: C64

%import textio
%import conv

%zeropage basicsafe

main {
    ; Test 3-argument memory() - slab reservation and reference
    uword buffer = memory("test_buffer", 256, 0)
    
    ; Test 1-argument memory() reference to same slab
    uword buffer2 = memory("test_buffer")
    
    ; Test multiple independent slabs
    uword slab_a = memory("slab_a", 100, 0)
    uword slab_b = memory("slab_b", 200, 0)
    
    ; Test alignment parameter
    uword aligned = memory("aligned_slab", 64, 64)  ; 64-byte alignment
    
    ; Test reference after declaration with alignment
    uword aligned_ref = memory("aligned_slab")
    
    sub start() {
        ; Test assignment of memory slab reference
        @(buffer) = 42
        @(buffer + 1) = 123
        
        ; Test usage in expressions
        uword val = @(buffer)
        ubyte val2 = @(buffer + 1) as ubyte
        
        ; Verify it's the same address
        if buffer == buffer2 {
            txt.print("pass: memory refs equal")
            txt.nl()
        }
        
        ; Verify data through alternate reference
        ubyte check = @(buffer2) as ubyte
        if check == 42 {
            txt.print("pass: data via alternate ref")
            txt.nl()
        }
        
        ; Verify they're different addresses
        if slab_a != slab_b {
            txt.print("pass: different slabs different addr")
            txt.nl()
        }
        
        ; Verify aligned slab ref
        if aligned == aligned_ref {
            txt.print("pass: aligned slab ref")
            txt.nl()
        }
        
        ; Test direct memory write/read
        @(buffer) = 10
        @(buffer + 1) = 20
        
        if @(buffer) == 10 {
            txt.print("pass: memory as struct")
            txt.nl()
        }
    }
}
