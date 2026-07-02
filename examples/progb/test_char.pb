IMPORT textio
ZEROPAGE basicsafe

MODULE main
    SUB start()
        txt.print("before")
        txt.fill_screen('a', 1)
        txt.print("after")
    END SUB
END MODULE
