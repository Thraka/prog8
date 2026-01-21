' ProgB Number Guessing Game
' This is a BASIC-style syntax example for Prog8

IMPORT textio
IMPORT conv
IMPORT math
ZEROPAGE basicsafe

' The classic number guessing game

MODULE main

    SUB start()
        DIM name AS STRING = "????????????????????????"
        DIM input AS STRING = "??????????"
        DIM secretnumber AS UBYTE = math.rnd() MOD 99 + 1
        DIM attempts_left AS UBYTE
        DIM guess AS UBYTE

        txt.lowercase()
        txt.print("Please introduce yourself: ")
        VOID txt.input_chars(name)
        txt.print("Hello, ")
        txt.print(name)
        txt.print("! Let's play a number guessing game.")
        txt.nl()

        FOR attempts_left = 10 DOWNTO 1
            txt.print("You have ")
            txt.print_ub(attempts_left)
            txt.print(" guesses left. Your guess? ")
            VOID txt.input_chars(input)
            guess = conv.str2ubyte(input)

            IF guess = secretnumber THEN
                txt.print("You guessed it!")
                RETURN
            ELSEIF guess < secretnumber THEN
                txt.print("Too low!")
            ELSE
                txt.print("Too high!")
            END IF
            txt.nl()
        NEXT

        txt.print("Too bad! My number was: ")
        txt.print_ub(secretnumber)
        txt.nl()
    END SUB

END MODULE
