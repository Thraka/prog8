IMPORT textio
OPTION no_sysinit
ZEROPAGE basicsafe

' simple test program for the "VTUI" text user interface library
' see:  https://github.com/JimmyDansbo/VTUIlib

MODULE main
    SUB start()
        vtui.initialize()
        store_logo()            ' capture logo before boxes are drawn

        txt.lowercase()
        vtui.screen_set(0)
        vtui.clr_scr("%"c, $50)
        vtui.gotoxy(5,5)
        vtui.fill_box(":"c, 70, 50, $c6)

        store_where_logo_was()  ' after vtui draws boxes, initialize replacement screen values as logo moves

        vtui.gotoxy(10,10)
        vtui.border(1, 40, 6, $47)
        vtui.gotoxy(12,12)
        vtui.print_str2(sc:"Hello, world! vtui from Prog8!", $f2, FALSE)
        vtui.gotoxy(12,13)
        vtui.print_str2("Hello, world! vtui from Prog8!", $f2, TRUE)

        DIM inputbuffer AS STRING = "?" * 20

'        txt.print_uwhex(inputbuffer, 1)
'        txt.chrout(":"c)
'        txt.print(inputbuffer)
'        txt.chrout('\n')

        vtui.gotoxy(5,20)
        vtui.print_str2(sc:"Enter your name: ", $e3, FALSE)
        DIM length AS UBYTE = vtui.input_str(inputbuffer, len(inputbuffer), $21)

        vtui.gotoxy(8,22)
        vtui.print_str2(sc:"Your name is: ", $e3, FALSE)
        'vtui.print_str2(inputbuffer, $67, $00)
        vtui.print_str(inputbuffer, length, $67, $00)

        ' txt.uppercase()   ' kills vtui?
        logo_mover()
    END SUB

    SUB store_logo()
        vtui.gotoxy(0, 0)
        vtui.save_rect($80, TRUE, $0000, 7, 7)
    END SUB

    SUB store_where_logo_was()
        vtui.gotoxy(0, 0)
        vtui.save_rect($80, TRUE, $0100, 7, 7)
    END SUB

    SUB logo_mover()
        DIM xcoord AS UBYTE = 0
        DIM ycoord AS UBYTE = 0
        DIM newx AS UBYTE = 0
        DIM newy AS UBYTE = 0

        'vtui.screen_set(2)
        vtui.gotoxy(30, 32)
        vtui.print_str2("arrow keys to move!", $61, TRUE)

char_loop:
        DIM char AS UBYTE
        VOID, char = cbm.GETIN()
        IF char = 0 THEN GOTO char_loop

        SELECT CASE char
            CASE $91
                IF newy <> 0 THEN
                    newy--
                    move_logo()
                END IF
            CASE $11
                IF newy < 53 THEN
                    newy++
                    move_logo()
                END IF
            CASE $9d
                IF newx <> 0 THEN
                    newx--
                    move_logo()
                END IF
            CASE $1d
                IF newx < 70 THEN
                    newx++
                    move_logo()
                END IF
        END SELECT

        GOTO char_loop

        SUB move_logo()
            vtui.gotoxy(xcoord, ycoord)
            vtui.rest_rect($80, TRUE, $0100, 7, 7)
            vtui.gotoxy(newx, newy)
            vtui.save_rect($80, TRUE, $0100, 7, 7)
            vtui.gotoxy(newx, newy)
            vtui.rest_rect($80, TRUE, $0000, 7, 7)
            xcoord = newx
            ycoord = newy
        END SUB
    END SUB

END MODULE


MODULE vtui AT $1000
    OPTION no_symbol_prefixing
    ASMBINARY "VTUI1.2.BIN", 2     ' skip the 2 dummy load address bytes

    ' NOTE: base address $1000 here must be the same as the block's memory address, for obvious reasons!
    ' The routines below are for VTUI 1.0
    CONST vtjmp AS UWORD = $1002
    EXTSUB vtjmp - 2   = initialize() CLOBBERS(A, X, Y)
    EXTSUB vtjmp + 0*3 = screen_set(mode AS UBYTE @A) CLOBBERS(A, X, Y)
    EXTSUB vtjmp + 1*3 = set_bank(bank1 AS BOOL @Pc) CLOBBERS(A)
    EXTSUB vtjmp + 2*3 = set_stride(stride AS UBYTE @A) CLOBBERS(A)
    EXTSUB vtjmp + 3*3 = set_decr(incrdecr AS BOOL @Pc) CLOBBERS(A)
    EXTSUB vtjmp + 4*3 = clr_scr(char AS UBYTE @A, colors AS UBYTE @X) CLOBBERS(Y)
    EXTSUB vtjmp + 5*3 = gotoxy(column AS UBYTE @A, row AS UBYTE @Y)
    EXTSUB vtjmp + 6*3 = plot_char(char AS UBYTE @A, colors AS UBYTE @X)
    EXTSUB vtjmp + 7*3 = scan_char() AS UBYTE @A, UBYTE @X
    EXTSUB vtjmp + 8*3 = hline(char AS UBYTE @A, length AS UBYTE @Y, colors AS UBYTE @X) CLOBBERS(A)
    EXTSUB vtjmp + 9*3 = vline(char AS UBYTE @A, height AS UBYTE @Y, colors AS UBYTE @X) CLOBBERS(A)
    EXTSUB vtjmp + 10*3 = print_str(txtstring AS STRING @R0, length AS UBYTE @Y, colors AS UBYTE @X, convertchars AS UBYTE @A) CLOBBERS(A, Y)
    EXTSUB vtjmp + 11*3 = fill_box(char AS UBYTE @A, width AS UBYTE @R1, height AS UBYTE @R2, colors AS UBYTE @X) CLOBBERS(A, Y)
    EXTSUB vtjmp + 12*3 = pet2scr(char AS UBYTE @A) AS UBYTE @A
    EXTSUB vtjmp + 13*3 = scr2pet(char AS UBYTE @A) AS UBYTE @A
    EXTSUB vtjmp + 14*3 = border(mode AS UBYTE @A, width AS UBYTE @R1, height AS UBYTE @R2, colors AS UBYTE @X) CLOBBERS(Y)
    EXTSUB vtjmp + 15*3 = save_rect(ramtype AS UBYTE @A, vbank1 AS BOOL @Pc, address AS UWORD @R0, width AS UBYTE @R1, height AS UBYTE @R2) CLOBBERS(A, X, Y)
    EXTSUB vtjmp + 16*3 = rest_rect(ramtype AS UBYTE @A, vbank1 AS BOOL @Pc, address AS UWORD @R0, width AS UBYTE @R1, height AS UBYTE @R2) CLOBBERS(A, X, Y)
    EXTSUB vtjmp + 17*3 = input_str(buffer AS UWORD @R0, buflen AS UBYTE @Y, colors AS UBYTE @X) CLOBBERS(A) AS UBYTE @Y
    EXTSUB vtjmp + 18*3 = get_bank() CLOBBERS(A) AS BOOL @Pc
    EXTSUB vtjmp + 19*3 = get_stride() AS UBYTE @A
    EXTSUB vtjmp + 20*3 = get_decr() CLOBBERS(A) AS BOOL @Pc

    ' -- helper function to do string length counting for you internally, and turn the convertchars flag into a boolean again
    ASMSUB print_str2(txtstring AS STRING @R0, colors AS UBYTE @X, convertchars AS BOOL @Pc) CLOBBERS(A, Y)
        ASM
            lda  #0
            bcs  +
            lda  #$80
+           pha
            lda  cx16.r0
            ldy  cx16.r0+1
            jsr  prog8_lib.strlen
            pla
            jmp  print_str
        END ASM
    END ASMSUB
END MODULE
