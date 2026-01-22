IMPORT syslib
IMPORT conv
IMPORT math
IMPORT monogfx
IMPORT verafx
IMPORT floats

' TODO add FPS counter
' TODO add all other Elite's ships, show their name, advance to next ship on keypress

MODULE main
    SUB start()
        DIM anglex AS UWORD
        DIM angley AS UWORD
        DIM anglez AS UWORD

        monogfx.lores()
        monogfx.text_charset(1)
        monogfx.clear_screen(FALSE)
        print_ship_name()
        monogfx.enable_doublebuffer()
        monogfx.clear_screen(FALSE)
        print_ship_name()

        REPEAT
            matrix_math.rotate_vertices(msb(anglex), msb(angley), msb(anglez))

            ' We use verafx to clear the screen during animation, instead of
            ' the regular routine. This speeds up the frame rate a bit.
            verafx.clear(0, monogfx.buffer_back + 320*16/8, 0, 320/8*220/4)
            ' monogfx.clear_screen(FALSE)

            draw_lines_hiddenremoval()
            ' draw_lines()
            monogfx.swap_buffers(TRUE)

            anglex += 317
            angley -= 505
            anglez += 452
        END REPEAT
    END SUB

    SUB print_ship_name()
        monogfx.text(20, 0, TRUE, "3d ship model: ")
        monogfx.text(140, 0, TRUE, shipdata.shipName)

        monogfx.text(60, 8, TRUE, conv.str_ub(shipdata.totalNumberOfPoints))
        monogfx.text(80, 8, TRUE, "vertices,")

        monogfx.text(160, 8, TRUE, conv.str_ub(shipdata.totalNumberOfEdges))
        monogfx.text(180, 8, TRUE, "edges,")

        monogfx.text(240, 8, TRUE, conv.str_ub(shipdata.totalNumberOfFaces))
        monogfx.text(260, 8, TRUE, "faces")
    END SUB


    CONST screen_width AS UWORD = 320
    CONST screen_height AS UBYTE = 240

    SUB draw_lines()
        ' simple routine that draw all edges, exactly once, but no hidden line removal.
        DIM i AS UBYTE @zp
        FOR i = shipdata.totalNumberOfEdges - 1 DOWNTO 0
            DIM vFrom AS UBYTE @zp = shipdata.edgesFrom[i]
            DIM vTo AS UBYTE @zp = shipdata.edgesTo[i]
            DIM persp1 AS WORD = 200 + matrix_math.rotatedz[vFrom]/256
            DIM persp2 AS WORD = 200 + matrix_math.rotatedz[vTo]/256
            monogfx.line(matrix_math.rotatedx[vFrom] / persp1 + screen_width/2 AS UWORD,
                matrix_math.rotatedy[vFrom] / persp1 + screen_height/2 AS UWORD,
                matrix_math.rotatedx[vTo] / persp2 + screen_width/2 AS UWORD,
                matrix_math.rotatedy[vTo] / persp2 + screen_height/2 AS UWORD,
                TRUE)
        NEXT
    END SUB

    SUB draw_lines_hiddenremoval()
        ' complex drawing routine that draws the ship model based on its faces,
        ' where it uses the surface normals to determine visibility.
        sys.memset(edgestodraw, shipdata.totalNumberOfEdges, 1)
        DIM edgeIdx AS UBYTE @zp = 0
        DIM pointIdx AS UBYTE @zp = 0
        DIM faceNumber AS UBYTE
        FOR faceNumber = shipdata.totalNumberOfFaces - 1 DOWNTO 0
            IF matrix_math.facing_away_fast_but_imprecise(pointIdx) THEN
                ' don't draw this face, fast-forward over the edges and points
                edgeIdx += 3    ' every face hast at least 3 edges
                WHILE shipdata.facesEdges[edgeIdx] <> 255
                    edgeIdx++
                WEND
                edgeIdx++
                pointIdx += 3    ' every face has at least 3 points
                WHILE shipdata.facesPoints[pointIdx] <> 255
                    pointIdx++
                WEND
                pointIdx++
            ELSE
                ' draw this face
                DIM e1 AS UBYTE @zp = shipdata.facesEdges[edgeIdx]
                edgeIdx++
                DIM e2 AS UBYTE @zp = shipdata.facesEdges[edgeIdx]
                edgeIdx++
                DIM e3 AS UBYTE @zp = shipdata.facesEdges[edgeIdx]
                edgeIdx++
                IF edgestodraw[e1] THEN draw_edge(e1)
                IF edgestodraw[e2] THEN draw_edge(e2)
                WHILE e3 <> 255
                    IF edgestodraw[e3] THEN draw_edge(e3)
                    e3 = shipdata.facesEdges[edgeIdx]
                    edgeIdx++
                WEND
                ' skip the rest of the facesPoints, we don't need them here anymore
                pointIdx += 3    ' every face has at least 3 points
                WHILE shipdata.facesPoints[pointIdx] <> 255
                    pointIdx++
                WEND
                pointIdx++
            END IF
        NEXT
    END SUB

    DIM edgestodraw[shipdata.totalNumberOfEdges] AS BOOL

    SUB draw_edge(edgeidx AS UBYTE)
        edgestodraw[edgeidx] = FALSE
        DIM vFrom AS UBYTE = shipdata.edgesFrom[edgeidx]
        DIM vTo AS UBYTE = shipdata.edgesTo[edgeidx]
        DIM persp1 AS WORD = 170 + matrix_math.rotatedz[vFrom]/256
        DIM persp2 AS WORD = 170 + matrix_math.rotatedz[vTo]/256
        monogfx.line(matrix_math.rotatedx[vFrom] / persp1 + screen_width/2 AS UWORD,
            matrix_math.rotatedy[vFrom] / persp1 + screen_height/2 AS UWORD,
            matrix_math.rotatedx[vTo] / persp2 + screen_width/2 AS UWORD,
            matrix_math.rotatedy[vTo] / persp2 + screen_height/2 AS UWORD,
            TRUE)
    END SUB
END MODULE

MODULE matrix_math
    VERAFXMULS      ' accellerate all word-multiplications in this block using Vera FX hardware muls

    ' storage for rotated coordinates
    DIM rotatedx[shipdata.totalNumberOfPoints] AS WORD
    DIM rotatedy[shipdata.totalNumberOfPoints] AS WORD
    DIM rotatedz[shipdata.totalNumberOfPoints] AS WORD

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
        FOR i = 0 TO shipdata.totalNumberOfPoints - 1
            ' don't normalize by dividing by 128, instead keep some precision for perspective calc later
            rotatedx[i] = Axx*shipdata.xcoor[i] + Axy*shipdata.ycoor[i] + Axz*shipdata.zcoor[i]
            rotatedy[i] = Ayx*shipdata.xcoor[i] + Ayy*shipdata.ycoor[i] + Ayz*shipdata.zcoor[i]
            rotatedz[i] = Azx*shipdata.xcoor[i] + Azy*shipdata.ycoor[i] + Azz*shipdata.zcoor[i]
        NEXT
    END SUB

    FUNCTION facing_away_fast_but_imprecise(edgePointsIdx AS UBYTE) AS BOOL
        ' simplistic visibility determination by checking the Z component of the surface normal
        ' this only compares the surface normal to the screen space vector which doesn't yield the proper perspective correct result, but is fast
        DIM p1 AS UBYTE = shipdata.facesPoints[edgePointsIdx]
        edgePointsIdx++
        DIM p2 AS UBYTE = shipdata.facesPoints[edgePointsIdx]
        edgePointsIdx++
        DIM p3 AS UBYTE = shipdata.facesPoints[edgePointsIdx]

        DIM p1x AS WORD = rotatedx[p1] / 128
        DIM p1y AS WORD = rotatedy[p1] / 128
        DIM p2x AS WORD = rotatedx[p2] / 128
        DIM p2y AS WORD = rotatedy[p2] / 128
        DIM p3x AS WORD = rotatedx[p3] / 128
        DIM p3y AS WORD = rotatedy[p3] / 128
        RETURN (p2x - p3x)*(p1y - p3y) - (p2y - p3y)*(p1x - p3x) > 0
    END FUNCTION

    FUNCTION facing_away_slow_but_precise(edgePointsIdx AS UBYTE) AS BOOL
        ' determine visibility by calculating the dot product of surface normal and view vector
        DIM p1 AS UBYTE = shipdata.facesPoints[edgePointsIdx]
        edgePointsIdx++
        DIM p2 AS UBYTE = shipdata.facesPoints[edgePointsIdx]
        edgePointsIdx++
        DIM p3 AS UBYTE = shipdata.facesPoints[edgePointsIdx]

        ' Calculate two edge vectors of the triangle  (scaled by 2)
        DIM v1x AS WORD = (rotatedx[p2] - rotatedx[p1])/128
        DIM v1y AS WORD = (rotatedy[p2] - rotatedy[p1])/128
        DIM v1z AS WORD = (rotatedz[p2] - rotatedz[p1])/128
        DIM v2x AS WORD = (rotatedx[p3] - rotatedx[p1])/128
        DIM v2y AS WORD = (rotatedy[p3] - rotatedy[p1])/128
        DIM v2z AS WORD = (rotatedz[p3] - rotatedz[p1])/128

        ' Calculate surface normal using cross product: N = V1 x V2     (scaled by 4)
        ' Note: because of lack of precision in the 16 bit word math, we need to use floating point math here.... :-(
        ' Elite had a more optimized version of this algorithm that still used fixed point integer math only...
        DIM normalx AS FLOAT = (v1y * v2z - v1z * v2y) AS FLOAT
        DIM normaly AS FLOAT = (v1z * v2x - v1x * v2z) AS FLOAT
        DIM normalz AS FLOAT = (v1x * v2y - v1y * v2x) AS FLOAT

        ' Calculate view vector from camera (0,0,-170) to point p1   (scaled by 4)
        DIM viewx AS FLOAT = rotatedx[p1]/(256/4) - 0          AS FLOAT        ' from camera x to point x
        DIM viewy AS FLOAT = rotatedy[p1]/(256/4) - 0          AS FLOAT        ' from camera y to point y
        DIM viewz AS FLOAT = rotatedz[p1]/(256/4) - (-170*4)   AS FLOAT        ' from camera z to point z

        ' Calculate dot product of normal and view vector
        ' If dot product is negative, the face is pointing away from the camera
        RETURN normalx * viewx + normaly * viewy + normalz * viewz < 0
    END FUNCTION
END MODULE

MODULE shipdata
    ' Ship model data converted from BBC Elite's Cobra MK 3
    ' downloaded from http://www.elitehomepage.org/archive/index.htm

    CONST totalNumberOfEdges AS UBYTE = 51
    CONST totalNumberOfFaces AS UBYTE = 22
    CONST totalNumberOfPoints AS UBYTE = 34
    DIM shipName AS STRING = "cobra-mk3"
    ' vertices
    DIM xcoor[totalNumberOfPoints] AS WORD = [ 32,-32,0,-120,120,-88,88,128,-128,0,-32,32,-36,-8,8,36,36,8,-8,-36,-1,-1,-80,-80,-88,80,88,80,1,1,1,1,-1,-1 ]
    DIM ycoor[totalNumberOfPoints] AS WORD = [ 0,0,26,-3,-3,16,16,-8,-8,26,-24,-24,8,12,12,8,-12,-16,-16,-12,-1,-1,-6,6,0,6,0,-6,-1,-1,1,1,1,1 ]
    DIM zcoor[totalNumberOfPoints] AS WORD = [ 76,76,24,-8,-8,-40,-40,-40,-40,-40,-40,-40,-40,-40,-40,-40,-40,-40,-40,-40,76,90,-40,-40,-40,-40,-40,-40,76,90,76,90,76,90 ]
    ' edges and faces
    DIM edgesFrom[totalNumberOfEdges] AS UBYTE = [ 0,1,0,10,1,0,2,0,4,0,4,7,2,1,1,3,8,3,2,5,6,5,6,16,15,14,14,18,13,12,12,26,25,25,22,23,22,20,28,21,20,28,29,30,31,30,32,20,21,20,20 ]
    DIM edgesTo[totalNumberOfEdges] AS UBYTE = [ 1,2,2,11,10,11,6,6,6,4,7,11,5,5,3,5,10,8,9,9,9,8,7,17,16,15,17,19,18,13,19,27,26,27,23,24,24,28,29,29,21,30,31,31,33,32,33,32,33,33,29 ]
    DIM facesPoints[] AS UBYTE = [
         0,1,2 ,255,
         11,10,1,0 ,255,
         0,2,6 ,255,
         6,4,0 ,255,
         4,7,11,0 ,255,
         5,2,1 ,255,
         1,3,5 ,255,
         10,8,3,1 ,255,
         9,2,5 ,255,
         9,6,2 ,255,
         3,8,5 ,255,
         4,6,7 ,255,
         5,8,10,11,7,6,9 ,255,
         17,16,15,14 ,255,
         19,18,13,12 ,255,
         27,26,25 ,255,
         22,23,24 ,255,
         20,28,29,21 ,255,
         30,28,29,31 ,255,
         33,31,30,32 ,255,
         20,32,33,21 ,255,
         29,31,33,20 ,255
    ]
    DIM facesEdges[] AS UBYTE = [
         0,1,2 ,255,
         3,4,0,5 ,255,
         2,6,7 ,255,
         8,9,7 ,255,
         10,11,5,9 ,255,
         12,1,13 ,255,
         14,15,13 ,255,
         16,17,14,4 ,255,
         18,12,19 ,255,
         20,6,18 ,255,
         17,21,15 ,255,
         8,22,10 ,255,
         21,16,3,11,22,20,19 ,255,
         23,24,25,26 ,255,
         27,28,29,30 ,255,
         31,32,33 ,255,
         34,35,36 ,255,
         37,38,39,40 ,255,
         41,38,42,43 ,255,
         44,43,45,46 ,255,
         47,46,48,40 ,255,
         42,44,49,50 ,255
    ]

END MODULE
