' Galencia starfield ported to Prog8. Original: https://github.com/JasonAldred/C64-Starfield
' This is for the C64 only.

OPTION no_sysinit

MODULE main
    CONST starScreenChar AS UWORD  = $0400         ' Screen address
    CONST StarScreenCols AS UWORD  = $d800         ' Character attribute address

    CONST charBase AS UWORD  = $3000         ' Address of our character set
    CONST star1Init AS UWORD = charBase+$1d0 ' Init address for each star
    CONST star2Init AS UWORD = charBase+$298
    CONST star3Init AS UWORD = charBase+$240
    CONST star4Init AS UWORD = charBase+$2e0

    CONST star1Limit AS UWORD  = charBase+$298 ' Limit for each star
    CONST star2Limit AS UWORD  = charBase+$360 ' Once limit is reached, they are reset
    CONST star3Limit AS UWORD  = charBase+$298
    CONST star4Limit AS UWORD  = charBase+$360
    CONST star1Reset AS UWORD  = charBase+$1d0 ' Reset address for each star
    CONST star2Reset AS UWORD  = charBase+$298
    CONST star3Reset AS UWORD  = charBase+$1d0
    CONST star4Reset AS UWORD  = charBase+$298
    CONST staticStar1 AS UWORD = charBase+$250 ' 2 Locations for blinking static stars
    CONST staticStar2 AS UWORD = charBase+$1e0

    CONST starColourLimit AS UBYTE = 20            ' use values 1 to 20
                                ' Galencia uses these values
                                ' 1     = mono
                                ' 2     = duo
                                ' 20    = full colour

    ' 4 x pointers for moving stars
    DIM starfieldPtr1 AS UWORD @zp
    DIM starfieldPtr2 AS UWORD @zp
    DIM starfieldPtr3 AS UWORD @zp
    DIM starfieldPtr4 AS UWORD @zp

    DIM rasterCount AS UBYTE    ' Counter that increments each frame

    SUB start ()
        sys.set_irqd()
        sys.memset(charBase, 8*256, 0)     ' clear charset data
        c64.EXTCOL = 0
        c64.BGCOL0 = 0
        c64.VMCSB = (charBase/1024) BITOR %00010000     ' Characters at $3000
        initStarfield()
        createStarScreen()

        REPEAT
            sys.waitvsync()
            rasterCount ++
            doStarfield()
        END REPEAT
    END SUB

    SUB doStarfield()
        ' This routine does 3 things:
        ' 1) Erases stars
        ' 2) Moves stars
        ' 3) Draws stars in new position

        @(starfieldPtr1) = 0
        @(starfieldPtr2) = 0
        @(starfieldPtr3) = 0
        @(starfieldPtr4) = 0

        IF rasterCount BITAND 1 <> 0 THEN
            starfieldPtr1++
            IF starfieldPtr1 = star1Limit THEN starfieldPtr1 = star1Reset
        END IF
        starfieldPtr2++
        IF starfieldPtr2 = star2Limit THEN starfieldPtr2 = star2Reset
        IF rasterCount BITAND 3 = 0 THEN
            starfieldPtr3++
            IF starfieldPtr3 = star3Limit THEN starfieldPtr3 = star3Reset
        END IF
        starfieldPtr4 += 2
        IF starfieldPtr4 = star4Limit THEN starfieldPtr4 = star4Reset

        ' 2 static stars that flicker
        IF rasterCount >= 230 THEN
            @(staticStar1) = 0
        ELSE
            @(staticStar1) = 192
        END IF
        IF rasterCount BITXOR $80 >= 230 THEN
            @(staticStar2) = 0
        ELSE
            @(staticStar2) = 192
        END IF

        ' Plot new stars
        @(starfieldPtr1) |= 3
        @(starfieldPtr2) |= 3
        @(starfieldPtr3) |= 12
        @(starfieldPtr4) |= 48
    END SUB

    SUB initStarfield()
        starfieldPtr1 = star1Init
        starfieldPtr2 = star2Init
        starfieldPtr3 = star3Init
        starfieldPtr4 = star4Init
    END SUB

    SUB createStarScreen()
        ' Creates the starfield charmap and colour charmap
        ' This routine paints vertical stripes of colour into the colourmap
        ' so the stars are different colours
        ' It also plots the correct characters to the screen, wrapping them around
        ' at the correct char count to give to the starfield effect.

        DIM ptr1 AS UWORD @zp
        DIM x AS UBYTE
        FOR x = 0 TO 39
            DIM limit AS UBYTE
            DIM char AS UBYTE = starfieldRow[x]
            IF char >= 58+25 THEN
                limit = 58+50
            ELSE
                limit = 58+25
            END IF
            DIM start AS UBYTE = limit - 25
            ptr1 = starScreenChar
            REPEAT 25
                ptr1[x] = char
                ptr1 += 40
                char ++
                IF char = limit THEN char = start
            END REPEAT
        NEXT

        ' Fill colour map with vertical stripes of colour for starfield
        ptr1 = StarScreenCols
        DIM ci AS UBYTE = 0
        REPEAT 25
            FOR x = 0 TO 39
                ptr1[x] = starfieldCols[ci]
                ci ++
                IF ci = starColourLimit THEN ci = 0
            NEXT
            ptr1 += 40
        END REPEAT
    END SUB

    ' Dark starfield so it doesnt distract from bullets and text
    DIM starfieldCols[20] AS UBYTE = [
        14,10,12,15,14,13,12,11,10,14,
        14,10,14,15,14,13,12,11,10,12
        ]

    ' Star positions, 40 X positions, range 58-107
    DIM starfieldRow[40] AS UBYTE = [
        058,092,073,064,091,062,093,081,066,094,
        086,059,079,087,080,071,076,067,082,095,
        100,078,099,060,075,063,084,065,083,096,
        068,088,074,061,090,098,085,101,097,077
        ]
END MODULE
