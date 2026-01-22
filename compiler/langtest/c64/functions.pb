' Functions test in ProgB
IMPORT textio
ZEROPAGE basicsafe

MODULE main
    SUB start()
        DIM result AS UBYTE
        
        greet()
        result = add(5, 3)
        txt.print_ub(result)
        txt.nl()
        
        result = double_it(10)
        txt.print_ub(result)
        txt.nl()
    END SUB
    
    SUB greet()
        txt.print("hello from greet\n")
    END SUB
    
    FUNCTION add(a AS UBYTE, b AS UBYTE) AS UBYTE
        RETURN a + b
    END FUNCTION
    
    FUNCTION double_it(x AS UBYTE) AS UBYTE
        RETURN x SHL 1
    END FUNCTION
END MODULE
