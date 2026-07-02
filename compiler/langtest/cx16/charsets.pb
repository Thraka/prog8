IMPORT textio
ZEROPAGE basicsafe

MODULE main
    DIM buf AS STRING = "?" * 20

    SUB start()
        txt.print("a demonstration of the various non-petscii character sets in the x16.")
        wait()
        latin()
        wait()
        cyrillic()
        wait()
        eastern()
        wait()
        kata()
        wait()
        ibmpc()
        wait()
    END SUB

    SUB latin()
        txt.iso()
        REPEAT 3 txt.nl()
        write_screencodes(iso:"Latin: Le garçon n'a pas acheté d'œuf.")
        txt.print(iso:"Latin: Le garçon n'a pas acheté d'œuf.")
    END SUB

    SUB cyrillic()
        txt.iso5()
        REPEAT 3 txt.nl()
        write_screencodes(iso5:"Cyrillic: 'Хозяин и Работник' написана Лев Толстой.")
        txt.print(iso5:"Cyrillic: 'Хозяин и Работник' написана Лев Толстой.")
    END SUB

    SUB eastern()
        txt.iso16()
        REPEAT 3 txt.nl()
        write_screencodes(iso16:"Eastern European: zażółć gęślą jaźń")
        txt.print(iso16:"Eastern European: zażółć gęślą jaźń")
    END SUB

    SUB ibmpc()
        txt.color2(5,0)
        cx16.set_screen_mode(1)
        txt.cp437()
        REPEAT 3 txt.nl()
        write_screencodes(cp437:"≈ IBM Pc ≈ ÇüéâäàåçêëèïîìÄ ░▒▓│┤╡╢╖╕╣║╗╝╜╛┐ ☺☻♥♦♣♠•◘○◙♂♀♪♫☼ ►◄↕‼¶§▬↨↑↓→←∟↔▲▼")
        ' regular print() won't work because of control codes (<32) in this one.
        txt.print_lit(cp437:"≈ IBM Pc ≈ ÇüéâäàåçêëèïîìÄ ░▒▓│┤╡╢╖╕╣║╗╝╜╛┐ ☺☻♥♦♣♠•◘○◙♂♀♪♫☼ ►◄↕‼¶§▬↨↑↓→←∟↔▲▼")
        txt.nl()
    END SUB

    SUB kata()
        txt.kata()
        REPEAT 3 txt.nl()
        write_screencodes(kata:"Katakana: ｱﾉ ﾆﾎﾝｼﾞﾝ ﾜ ｶﾞｲｺｸｼﾞﾝ ﾉ ﾆﾎﾝｺﾞ ｶﾞ ｼﾞｮｳｽﾞ ﾀﾞｯﾃ ﾕｯﾀ｡ ## がが ## ガガ")
        txt.print(kata:"Katakana: ｱﾉ ﾆﾎﾝｼﾞﾝ ﾜ ｶﾞｲｺｸｼﾞﾝ ﾉ ﾆﾎﾝｺﾞ ｶﾞ ｼﾞｮｳｽﾞ ﾀﾞｯﾃ ﾕｯﾀ｡ ## がが ## ガガ")
    END SUB

    SUB wait()
        txt.print("\n\npress enter: ")
        VOID txt.input_chars(buf)
    END SUB

    SUB write_screencodes(message AS STRING)
        DIM column AS UBYTE = 0
        REPEAT
            IF message[column] = 0 THEN
                RETURN
            END IF
            txt.setchr(column, 1, message[column])
            column++
        END REPEAT
    END SUB
END MODULE
