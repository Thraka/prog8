' Test memory slab allocation and references
' Target: C64

IMPORT textio
IMPORT conv

ZEROPAGE basicsafe

MODULE main
    ' Test 3-argument memory() - slab reservation and reference
    DIM buffer AS UWORD = memory("test_buffer", 256, 0)
    
    ' Test 1-argument memory() reference to same slab
    DIM buffer2 AS UWORD = memory("test_buffer")
    
    ' Test multiple independent slabs
    DIM slab_a AS UWORD = memory("slab_a", 100, 0)
    DIM slab_b AS UWORD = memory("slab_b", 200, 0)
    
    ' Test alignment parameter
    DIM aligned AS UWORD = memory("aligned_slab", 64, 64)  ' 64-byte alignment
    
    ' Test reference after declaration with alignment
    DIM aligned_ref AS UWORD = memory("aligned_slab")
    
    SUB start()
        ' Test assignment of memory slab reference
        @(buffer) = 42
        @(buffer + 1) = 123
        
        ' Test usage in expressions
        DIM val AS UWORD = @(buffer)
        DIM val2 AS UBYTE = @(buffer + 1) AS UBYTE
        
        ' Verify it's the same address
        IF buffer = buffer2 THEN
            txt.print("pass: memory refs equal")
            txt.nl()
        END IF
        
        ' Verify data through alternate reference
        DIM check AS UBYTE = @(buffer2) AS UBYTE
        IF check = 42 THEN
            txt.print("pass: data via alternate ref")
            txt.nl()
        END IF
        
        ' Verify they're different addresses
        IF slab_a <> slab_b THEN
            txt.print("pass: different slabs different addr")
            txt.nl()
        END IF
        
        ' Verify aligned slab ref
        IF aligned = aligned_ref THEN
            txt.print("pass: aligned slab ref")
            txt.nl()
        END IF
        
        ' Test direct memory write/read
        @(buffer) = 10
        @(buffer + 1) = 20
        
        IF @(buffer) = 10 THEN
            txt.print("pass: memory as struct")
            txt.nl()
        END IF
    END SUB

END MODULE
