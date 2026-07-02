IMPORT syslib
IMPORT textio
IMPORT math


MODULE main

    ' vertices
    DIM xcoor[] AS WORD = [ -40, -40, -40, -40,  40,  40,  40, 40 ]
    DIM ycoor[] AS WORD = [ -40, -40,  40,  40, -40, -40,  40, 40 ]
    DIM zcoor[] AS WORD = [ -40,  40, -40,  40, -40,  40, -40, 40 ]

    ' storage for rotated coordinates
    DIM rotatedx[len(xcoor)] AS WORD
    DIM rotatedy[len(ycoor)] AS WORD
    DIM rotatedz[len(zcoor)] AS WORD

    SUB start()

        DIM anglex AS UWORD
        DIM angley AS UWORD
        DIM anglez AS UWORD
        REPEAT
            rotate_vertices(msb(anglex), msb(angley), msb(anglez))
            txt.clear_screenchars(32)
            draw_edges()
            anglex+=1000
            angley+=433
            anglez+=907
            txt.plot(0,0)
            txt.print("3d cube! ")
            txt.print_ub(cbm.TIME_LO)
            txt.print(" jiffies/fr = ")
            txt.print_ub(60/cbm.TIME_LO)
            txt.print(" fps")
            cbm.TIME_LO=0
        END REPEAT
    END SUB

    SUB rotate_vertices(ax AS UBYTE, ay AS UBYTE, az AS UBYTE)
        ' rotate around origin (0,0,0)

        ' set up the 3d rotation matrix values
        DIM wcosa AS WORD = math.cos8(ax)
        DIM wsina AS WORD = math.sin8(ax)
        DIM wcosb AS WORD = math.cos8(ay)
        DIM wsinb AS WORD = math.sin8(ay)
        DIM wcosc AS WORD = math.cos8(az)
        DIM wsinc AS WORD = math.sin8(az)

        DIM wcosa_sinb AS WORD = wcosa*wsinb / 128
        DIM wsina_sinb AS WORD = wsina*wsinb / 128

        DIM Axx AS WORD = wcosa*wcosb / 128
        DIM Axy AS WORD = (wcosa_sinb*wsinc - wsina*wcosc) / 128
        DIM Axz AS WORD = (wcosa_sinb*wcosc + wsina*wsinc) / 128
        DIM Ayx AS WORD = wsina*wcosb / 128
        DIM Ayy AS WORD = (wsina_sinb*wsinc + wcosa*wcosc) / 128
        DIM Ayz AS WORD = (wsina_sinb*wcosc - wcosa*wsinc) / 128
        DIM Azx AS WORD = -wsinb
        DIM Azy AS WORD = wcosb*wsinc / 128
        DIM Azz AS WORD = wcosb*wcosc / 128

        DIM i AS UBYTE @zp
        FOR i = 0 TO len(xcoor)-1
            ' don't normalize by dividing by 128, instead keep some precision for perspective calc later
            rotatedx[i] = Axx*xcoor[i] + Axy*ycoor[i] + Axz*zcoor[i]
            rotatedy[i] = Ayx*xcoor[i] + Ayy*ycoor[i] + Ayz*zcoor[i]
            rotatedz[i] = Azx*xcoor[i] + Azy*ycoor[i] + Azz*zcoor[i]
        NEXT
    END SUB

    SUB draw_edges()

        ' plot the points of the 3d cube
        ' first the points on the back, then the points on the front (painter algorithm)

        DIM i AS UBYTE @zp
        DIM rz AS WORD @zp
        DIM persp AS WORD @zp
        DIM sx AS BYTE
        DIM sy AS BYTE

        FOR i = 0 TO len(xcoor)-1
            rz = rotatedz[i]
            IF rz >= 10 THEN
                persp = 900 + rz/32
                sx = rotatedx[i] / persp AS BYTE + txt.DEFAULT_WIDTH/2
                sy = rotatedy[i] / persp AS BYTE + txt.DEFAULT_HEIGHT/2
                txt.setcc(sx AS UBYTE, sy AS UBYTE, 46, 7)
            END IF
        NEXT

        FOR i = 0 TO len(xcoor)-1
            rz = rotatedz[i]
            IF rz < 10 THEN
                persp = 900 + rz/32
                sx = rotatedx[i] / persp AS BYTE + txt.DEFAULT_WIDTH/2
                sy = rotatedy[i] / persp AS BYTE + txt.DEFAULT_HEIGHT/2
                txt.setcc(sx AS UBYTE, sy AS UBYTE, 81, 7)
            END IF
        NEXT
    END SUB
END MODULE
