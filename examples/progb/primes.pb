' ProgB Prime Numbers with Sieve of Eratosthenes
' This is a BASIC-style syntax example for Prog8

IMPORT textio
ZEROPAGE basicsafe

' Calculate prime numbers up to 255

MODULE main
    DIM sieve[256] AS BOOL
    DIM candidate_prime AS UBYTE = 2

    SUB start()
        sys.memset(ADDRESSOF(sieve), 256, 0)
        
        txt.print("Prime numbers up to 255:")
        txt.nl()
        txt.nl()
        
        DIM amount AS UBYTE = 0
        
        DO
            DIM prime AS UBYTE = find_next_prime()
            IF prime = 0 THEN EXIT DO
            txt.print_ub(prime)
            txt.print(", ")
            amount++
        LOOP
        
        txt.nl()
        txt.print("Number of primes (expected 54): ")
        txt.print_ub(amount)
        txt.nl()
    END SUB

    FUNCTION find_next_prime() AS UBYTE
        WHILE sieve[candidate_prime]
            candidate_prime++
            IF candidate_prime = 0 THEN RETURN 0
        WEND

        sieve[candidate_prime] = TRUE
        DIM multiple AS UWORD = candidate_prime AS UWORD

        WHILE multiple < len(sieve)
            sieve[lsb(multiple)] = TRUE
            multiple += candidate_prime
        WEND

        RETURN candidate_prime
    END FUNCTION

END MODULE
