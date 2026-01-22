IMPORT textio
IMPORT palette
IMPORT strings
IMPORT gfx_hires

' Mockup of a classic Amiga Workbench screen.

MODULE main

    SUB start()
        gfx_hires.graphics_mode()             ' select 640*480 mode, 4 colors
        mouse.set_pointer_image()
        cx16.mouse_config(-1, 640/8, 240/8)
        palette.set_rgb_nosplit([$aaa, $000, $fff, $68c], 4, 0)    ' set Amiga's workbench 2.0 gray, black, white, lightblue colors

        cx16.VERA_DC_VSCALE = 64        ' have the vertical resolution so it is 640*240 - more or less Amiga's default non interlaced mode
        gfx_hires.text_charset(1)

        screen_titlebar()
        window_workbench()
        window_system()
        window_shell()
        gfx_hires.text(240, 210, 1, iso:"640x480(240) 4 colors")
        gfx_hires.text(240, 220, 1, iso:"Mockup drawn using Prog8 gfx_hires library")

        REPEAT
        END REPEAT
    END SUB

    SUB screen_titlebar()
        gfx_hires.fillrect(0, 0, gfx_hires.WIDTH, 10, 2)
        gfx_hires.text(8,1, 1, iso:"AmigaOS 3.1    2,002,448 graphics mem  16,504,384 other mem")
        gfx_hires.horizontal_line(0, 10, gfx_hires.WIDTH, 1)
        widget.window_order_icon(gfx_hires.WIDTH-widget.window_order_icon.width, 0, FALSE)
    END SUB


    SUB window_workbench()
        CONST win_x AS UWORD = 10
        CONST win_y AS UWORD = 16
        CONST width AS UWORD = 600
        CONST height AS UWORD = 220

        widget.window_titlebar(win_x, win_y, width, iso:"Workbench", FALSE)
        ' gfx_hires.fillrect(win_x+3, win_y+11, width-4, height-11-2,0)    ' clear window pane
        widget.window_leftborder(win_x, win_y, height, FALSE)
        widget.window_bottomborder(win_x, win_y, width, height)
        widget.window_rightborder(win_x, win_y, width, height, FALSE)

        vector_v(win_x+width - 430, win_y+height-20)
        vector_v(win_x+width - 430 -14, win_y+height-20)

        widget.icon(45,40, iso:"Ram Disk")
        widget.icon(45,90, iso:"Workbench3.1")
    END SUB

    SUB vector_v(x AS UWORD, y AS UWORD)
        gfx_hires.horizontal_line(x, y, 12, 1)
        gfx_hires.horizontal_line(x+16, y+16, 11,1)
        gfx_hires.line(x,y,x+16,y+16,1)
        gfx_hires.line(x+11,y,x+16+5,y+10,1)
        gfx_hires.line(x+16+5,y+10,x+47,y-16,1)
        gfx_hires.line(x+16+10,y+16,x+46+12,y-16,1)
    END SUB

    SUB window_system()
        CONST width AS UWORD = 300
        CONST height AS UWORD = 120
        CONST win_x AS UWORD = 320
        CONST win_y AS UWORD = 40

        widget.window_titlebar(win_x, win_y, width, iso:"System", FALSE)
        gfx_hires.fillrect(win_x+3, win_y+11, width-4, height-11-2, 0)    ' clear window pane
        widget.window_leftborder(win_x, win_y, height, FALSE)
        widget.window_bottomborder(win_x, win_y, width, height)
        widget.window_rightborder(win_x, win_y, width, height, FALSE)

        widget.icon(win_x+16, win_y+14, iso:"FixFonts")
        widget.icon(win_x+16+80, win_y+14, iso:"NoFastMem")
        widget.icon(win_x+16, win_y+56, iso:"Format")
        widget.icon(win_x+16+80, win_y+56, iso:"RexxMast")
        widget.icon(win_x+16+160, win_y+56, iso:"Shell")
    END SUB

    SUB window_shell()
        CONST win_x AS UWORD = 64-4
        CONST win_y AS UWORD = 140
        CONST width AS UWORD = 500
        CONST height AS UWORD = 65

        widget.window_titlebar(win_x, win_y, width, iso:"AmigaShell", TRUE)
        gfx_hires.fillrect(win_x+3, win_y+11, width-4, height-11-2,0)    ' clear window pane
        widget.window_leftborder(win_x, win_y, height, TRUE)
        widget.window_bottomborder(win_x, win_y, width, height)
        widget.window_rightborder(win_x, win_y, width, height, TRUE)

        gfx_hires.text(win_x+5, win_y+12, 1, iso:"New Shell process 3")
        gfx_hires.text(win_x+5, win_y+12+8, 1, iso:"3.Workbench3.1:>")
        gfx_hires.fillrect(win_x+5+17*8, win_y+12+8, 8, 8, 1)        ' cursor
    END SUB
END MODULE

MODULE mouse
    SUB set_pointer_image()
        CONST sprite_data_addr AS UWORD = $a000
        CONST palette_offset AS UBYTE = 16
        ' sprite registers base in VRAM:  $1fc00
        '        Sprite 0:          $1FC00 - $1FC07     ' used by the kernal for mouse pointer
        cx16.vpoke(1, $fc00, lsb(sprite_data_addr >> 5))        ' sprite data ptr bits 5-12
        cx16.vpoke(1, $fc01, msb(sprite_data_addr >> 5))      ' mode bit (16 colors = 4 bpp) and sprite dataptr bits 13-16
        cx16.vpoke(1, $fc07, %01100000 BITOR palette_offset>>4)   ' 32x16 pixels (non-square...), palette offset

        DIM ix AS UBYTE
        FOR ix = 0 TO 255
            cx16.vpoke(0, sprite_data_addr+ix, mousecursor[ix])
        NEXT

        palette.set_color(palette_offset + %1111, $f00)
        palette.set_color(palette_offset + %1010, $fff)
        palette.set_color(palette_offset + %0101, $000)

        DIM mousecursor[256] AS UBYTE = [
            ' The Amiga Workbench 3.0 mouse cursor sprite image.
            ' note that the sprite resolution is 32x16 (non-square pixels) because it follows the bitmap screen resolution
            ' %1111 = red, %1010 = white, %0101 = black, %0000 = transparent
            %11111111,%10101010,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,
            %01010101,%11111111,%10101010,%10101010,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,
            %00000000,%01010101,%11111111,%11111111,%10101010,%10101010,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,
            %00000000,%01010101,%11111111,%11111111,%11111111,%11111111,%10101010,%10101010,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,
            %00000000,%00000000,%01010101,%11111111,%11111111,%11111111,%11111111,%11111111,%10101010,%10101010,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,
            %00000000,%00000000,%01010101,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,
            %00000000,%00000000,%00000000,%01010101,%11111111,%11111111,%11111111,%10101010,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,
            %00000000,%00000000,%00000000,%01010101,%11111111,%11111111,%01010101,%11111111,%10101010,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,
            %00000000,%00000000,%00000000,%00000000,%01010101,%11111111,%00000000,%01010101,%11111111,%10101010,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,
            %00000000,%00000000,%00000000,%00000000,%01010101,%11111111,%00000000,%00000000,%01010101,%11111111,%10101010,%00000000,%00000000,%00000000,%00000000,%00000000,
            %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%01010101,%11111111,%00000000,%00000000,%00000000,%00000000,%00000000,
            %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,
            %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,
            %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,
            %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,
            %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
        ]
    END SUB
END MODULE

MODULE widget

    SUB highlightedrect(x AS UWORD, y AS UWORD, width AS UWORD, height AS UWORD, fill AS BOOL, active AS BOOL)
        gfx_hires.horizontal_line(x, y, width, 2)
        gfx_hires.vertical_line(x, y+1, height-1, 2)
        gfx_hires.vertical_line(x+width-1, y+1, height-1, 1)
        gfx_hires.horizontal_line(x+1, y+height-1, width-2, 1)
        IF fill THEN
            IF active THEN
                gfx_hires.fillrect(x+1,y+1,width-2,height-2, 3)
            ELSE
                gfx_hires.fillrect(x+1,y+1,width-2,height-2, 0)
            END IF
        END IF
    END SUB

    SUB icon(x AS UWORD, y AS UWORD, caption AS STRING)
        CONST width AS UBYTE = 56
        CONST height AS UBYTE = 28
        highlightedrect(x, y, width, height, FALSE, FALSE)
        DIM middlex AS UWORD = x+width/2+1
        DIM halfstring AS UBYTE = strings.length(caption) * 4
        gfx_hires.text(middlex-halfstring,y+height+1,1,caption)
        gfx_hires.disc(x+width/4+4, y+height/2, height/2-3, 1)
        gfx_hires.fillrect(x+20,y+12,width/2,height/2-4,3)
    END SUB


    SUB window_titlebar(x AS UWORD, y AS UWORD, width AS UWORD, titlestr AS STRING, active AS BOOL)
        CONST height AS UBYTE = 11
        widget.highlightedrect(x+widget.window_close_icon.width, y, width-64, height, TRUE, active)
        gfx_hires.plot(x+widget.window_close_icon.width, y+height-1, 1) ' correct bottom left corner
        gfx_hires.text(x+26, y+1, 1, titlestr)
        widget.window_close_icon(x, y, active)
        widget.window_order_icon(x+width-22, y, active)
        widget.window_flipsize_icon(x+width-44, y, active)
    END SUB

    SUB window_flipsize_icon(x AS UWORD, y AS UWORD, active AS BOOL)
        CONST width AS UWORD = 22
        CONST height AS UWORD = 11
        highlightedrect(x, y, width, height, TRUE, active)
        gfx_hires.plot(x, y+height-1, 1) ' correct bottom left corner
        gfx_hires.rect(x+5, y+2, width-9, height-4, 1)
        gfx_hires.rect(x+5, y+2, 7, 4, 1)
        gfx_hires.fillrect(x+6, y+3, 5, 2, 2)
    END SUB

    SUB window_order_icon(x AS UWORD, y AS UWORD, active AS BOOL)
        CONST width AS UWORD = 22
        CONST height AS UWORD = 11
        highlightedrect(x, y, width, height, TRUE, active)
        gfx_hires.plot(x, y+height-1, 1) ' correct bottom left corner
        gfx_hires.rect(x+4, y+2, 10, 5, 1)       ' back
        gfx_hires.fillrect(x+9, y+5, 8, 3, 2)       ' white front
        gfx_hires.rect(x+8, y+4, 10, 5, 1)       ' front
    END SUB

    SUB window_close_icon(x AS UWORD, y AS UWORD, active AS BOOL)
        CONST width AS UWORD = 20
        CONST height AS UWORD = 11
        highlightedrect(x, y, width, height, TRUE, active)
        gfx_hires.plot(x, y+height-1, 1) ' correct bottom left corner
        gfx_hires.rect(x+7, y+3, 5, 5, 1)
        gfx_hires.fillrect(x+8, y+4, 3, 3, 2)
    END SUB

    SUB window_leftborder(x AS UWORD, y AS UWORD, height AS UWORD, active AS BOOL)
        gfx_hires.vertical_line(x, y, height, 2)
        DIM color AS UBYTE = 0
        IF active THEN
            color = 3
        END IF
        gfx_hires.vertical_line(x+1, y+11, height-11, color)
        gfx_hires.vertical_line(x+2, y+11, height-11, 1)
    END SUB

    SUB window_bottomborder(x AS UWORD, y AS UWORD, width AS UWORD, height AS UWORD)
        gfx_hires.horizontal_line(x+3, y+height-2, width-3, 2)
        gfx_hires.horizontal_line(x, y+height-1, width, 1)
    END SUB

    SUB window_rightborder(x AS UWORD, y AS UWORD, width AS UWORD, height AS UWORD, active AS BOOL)
        gfx_hires.vertical_line(x+width-1-16, y+11, height-13,2)
        gfx_hires.vertical_line(x+width-1, y+11, height-11,1)
        DIM color AS UBYTE = 0
        IF active THEN
            color = 3
        END IF
        gfx_hires.fillrect(x+width-1-15, y+11, 15, height-12, color)

        gfx_hires.horizontal_line(x+width-1-13, y+height-3, 11, 1)
        gfx_hires.vertical_line(x+width-1-3, y+height-3-5, 5, 1)
        gfx_hires.line(x+width-1-13,y+height-3, x+width-1-3, y+height-3-5, 1)
        gfx_hires.horizontal_line(x+width-1-16, y+height-10, 16, 2)

        highlightedrect(x+width-13, y+12, 10, height-43, FALSE, FALSE)
        gfx_hires.horizontal_line(x+width-1-16, y+height-11, 16, 1)
        gfx_hires.horizontal_line(x+width-1-16, y+height-20, 16, 2)
        gfx_hires.horizontal_line(x+width-1-16, y+height-21, 16, 1)
        gfx_hires.horizontal_line(x+width-1-16, y+height-30, 16, 2)
        gfx_hires.line(x+width-1-13, y+height-23, x+width-9, y+height-28, 1)
        gfx_hires.line(x+width-1-3, y+height-23, x+width-9, y+height-28, 1)
        gfx_hires.line(x+width-1-13, y+height-18, x+width-9, y+height-13, 1)
        gfx_hires.line(x+width-1-3, y+height-18, x+width-9, y+height-13, 1)
    END SUB
END MODULE
