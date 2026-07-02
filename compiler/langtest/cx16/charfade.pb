
' color fader that doesn't do palette fade but color attribute fade.
' NOTE: this needs ROM 49+

IMPORT textio
IMPORT math
ZEROPAGE basicsafe
OPTION no_sysinit


MODULE main
    DIM palette AS ^^UWORD = memory("palette", 256*2, 0)
    DIM fadeLUT[256] AS UBYTE      ' for each of the 256 colors in the palette, give the color index of the closest 1 step darker color.

    CONST WIDTH AS UBYTE = 40
    CONST HEIGHT AS UBYTE = 30

    SUB start()

        precalc_fade_table()

        cx16.set_screen_mode(128)
        cx16.GRAPH_set_colors(0,0,0)
        cx16.GRAPH_clear()
        txt.t256c(TRUE)         ' to allow characters to use all 256 colors in the palette instead of just the first 16

        DIM x, y AS UBYTE
        FOR y = 0 TO HEIGHT-1
            FOR x = 0 TO WIDTH-1
                txt.setcc2(x,y, math.rnd(), math.rnd())
            NEXT
        NEXT

        sys.wait(30)

        REPEAT
            REPEAT 4 sys.waitvsync()

            FOR y = 0 TO HEIGHT-1
                FOR x = 0 TO WIDTH-1
                    ' this can be done much faster using Vera's auto increment mode, but for the sake of clarity, we'll do it like this
                    txt.setclr(x,y, fadeLUT[txt.getclr(x,y)])
                NEXT

                txt.setcc2(math.rnd() MOD WIDTH, math.rnd() MOD HEIGHT, math.rnd(), math.rnd())     ' add new chars
            NEXT
        END REPEAT
    END SUB


    SUB precalc_fade_table()
        txt.print("\nprecalc color fade table, patience plz")

        fill_default_palette()

        DIM index AS UBYTE
        FOR index = 0 TO 255
            fadeLUT[index] = find_darker(index)
        NEXT

        SUB fill_default_palette()
            DIM pal_bank AS UBYTE
            DIM pal_addr AS UWORD

            pal_bank, pal_addr = cx16.get_default_palette()      ' needs ROM 49+
            cx16.push_rombank(pal_bank)
            sys.memcopy(pal_addr, palette, 256*2)
            cx16.pop_rombank()
        END SUB

        FUNCTION find_darker(color_index AS UBYTE) AS UBYTE
            DIM darker_rgb AS UWORD = darken(palette[index])
            IF darker_rgb = 0 THEN
                RETURN 0
            END IF
            DIM closest, second AS UBYTE
            closest, second = find_2_closest_colorindexes(darker_rgb)
            IF closest = color_index THEN
                RETURN second       ' to avoid stuck colors
            END IF
            RETURN closest
        END FUNCTION

        FUNCTION darken(color AS UWORD @R0) AS UWORD
            IF cx16.r0H <> 0 THEN
                cx16.r0H--
            END IF
            IF cx16.r0L & $0f <> 0 THEN
                cx16.r0L--
            END IF
            IF cx16.r0L & $f0 <> 0 THEN
                cx16.r0L -= $10
            END IF
            RETURN cx16.r0
        END FUNCTION

        FUNCTION find_2_closest_colorindexes(rgb AS UWORD @R0) AS UBYTE, UBYTE
            DIM distance AS UBYTE = 255
            DIM current AS UBYTE
            DIM second AS UBYTE = 15
            ALIAS index = cx16.r1L

            FOR index = 0 TO 255
                DIM pc AS UWORD @zp = palette[index]
                DIM d2 AS UBYTE @zp = abs(msb(pc) AS BYTE - msb(rgb) AS BYTE)                ' RED distance
                d2 += abs((lsb(pc) & $f0) AS BYTE - (lsb(rgb) & $f0) AS BYTE) SHR 4    ' GREEN distance
                d2 += abs((lsb(pc) & $0f) AS BYTE - (lsb(rgb) & $0f) AS BYTE)         ' BLUE distance
                IF d2 = 0 THEN
                    RETURN index, second
                END IF
                IF d2 < distance THEN
                    distance = d2
                    second = current
                    current = index
                END IF
            NEXT

            RETURN current, second
        END FUNCTION
    END SUB
END MODULE
