OPTION no_sysinit
IMPORT textio
IMPORT conv
IMPORT diskio
IMPORT strings
IMPORT sprites
IMPORT floats
IMPORT verafx
ENCODING iso
ZEROPAGE basicsafe
ZPRESERVED $22, $60

MODULE VideoHeader
  DIM ID[4] AS UBYTE AT $500
  DIM VTYPE AS UBYTE AT $504
  DIM BPP AS UBYTE AT $505
  DIM FPS AS UBYTE AT $506
  DIM SpriteWidth AS UBYTE AT $507
  DIM SpriteHeight AS UBYTE AT $508
  DIM FrameWidth AS UWORD AT $509
  DIM FrameHeight AS UWORD AT $50B
  DIM NumFramesLO AS UWORD AT $50D
  DIM NumFramesHI AS UWORD AT $50F
  DIM SHint AS UBYTE AT $511
  DIM VERARate AS UBYTE AT $512
  DIM AudioFrameSize AS UWORD AT $513
  DIM XAUD_FLAG AS UBYTE AT $515
  DIM XCFG_BYTE AS UBYTE AT $516
  DIM X_F_FPS AS UWORD AT $517
  DIM Extra[7] AS UBYTE AT $519
END MODULE

MODULE WAVHeader
   DIM RIFFID[4] AS UBYTE AT $522
   DIM WAVDATASIZExL AS UWORD AT $526
   DIM WAVDATASIZExH AS UWORD AT $528
   DIM WAVID[4] AS UBYTE AT $52A
   DIM SUBCHKID1[4] AS UBYTE AT $52E
   DIM SUBCHK1SIZExL AS UWORD AT $532
   DIM SUBCHK1SIZExH AS UWORD AT $534
   DIM AUDIO_FORMAT AS UWORD AT $536
   DIM AUDIO_CHANNELS AS UWORD AT $538
   DIM SAMPLE_RATExL AS UWORD AT $53A
   DIM SAMPLE_RATExH AS UWORD AT $53C
   DIM BYTE_RATExL AS UWORD AT $53E
   DIM BYTE_RATExH AS UWORD AT $540
   DIM BLOCK_ALIGN AS UWORD AT $542
   DIM SAMPLE_BITS AS UWORD AT $544
   DIM SUBCHKID2[4] AS UBYTE AT $546
   DIM SUBCHK2SIZExL AS UWORD AT $54A
   DIM SUBCHK2SIZExH AS UWORD AT $54C
END MODULE

MODULE ZCMHeader
    DIM lptr AS UWORD AT $522   ' $FFFF identify's HUGE ZCM
    DIM banknum AS UBYTE AT $524   ' for standard ZCM
    DIM SizeLow32 AS UWORD AT $524   ' for HUGE ZCM
    DIM SizeLow24 AS UBYTE AT $525   ' for standard ZCM
    DIM SizeHigh AS UWORD AT $526   ' Multiply by 256 for Standard ZCM, Multiply by 65536 for HUGE ZCM
    DIM ZCFG_BYTE AS UBYTE AT $528   ' LOW Nibble of ZCFG_BYTE is requested VOLUME and is masked out/ignored by this code
    DIM ZRATE_BYTE AS UBYTE AT $529   ' This and ZCFG are copied directly to the VERA PCM control registers (sans the Volume nibble)
END MODULE

MODULE ROM
      DIM VERSION AS UBYTE
      DIM IsPreRelease AS BOOL
END MODULE


MODULE PENV
           DIM HOST AS STRING = "?"*5
           DIM PETSCII_VERSTR AS STRING ="RC 1.0 \x1E(\x9B4TH REV\x1E)"
           DIM ISOASC_VERSTR AS STRING ="\x05 RC \x991.0 \x9B(\x054th rev\x9B)"
           DIM CPU AS STRING ="\x9B65\x99C\x9B02"
           DIM cpu AS UWORD =6502
           DIM IS_HOST AS BOOL
           DIM ON_HARDWARE AS BOOL
           DIM ON_EMU AS BOOL

           SUB FIND_CPU()
               IF sys.cpu_is_65816() THEN
                strings.copy("\x9B65\x99816",CPU) : cpu=816
               END IF
           END SUB

           FUNCTION IS_HOST_FS() AS BOOL
               cbm.SETNAM(11, "$")
               cbm.SETLFS(11, 8, 0)
               cbm.OPEN()
               cbm.CHKIN(11)
               DIM ctr AS UBYTE = 0
               WHILE ctr<2
                   IF cbm.GETIN2()=34 THEN ctr++
               WEND
               VOID cbm.GETIN2()
               FOR ctr = 0 TO 3
                   HOST[ctr] = cbm.GETIN2()
               NEXT
               HOST[4] = 0
               cbm.CLRCHN()
               cbm.CLOSE(11)
               RETURN ((HOST="HOST") AND ON_EMU)
           END FUNCTION

DIM EMUIndicator1 AS UBYTE AT $9FBE
DIM EMUIndicator2 AS UBYTE AT $9FBF

SUB GetRunEnvironment()
         FIND_CPU()
         ROM.VERSION, ROM.IsPreRelease = cx16.rom_version()
         ON_EMU = ( (EMUIndicator1=49) AND (EMUIndicator2=54) )
         IS_HOST=IS_HOST_FS()
         ON_HARDWARE = NOT ON_EMU
     END SUB
END MODULE



MODULE main
DIM vHeader[32] AS UBYTE AT $500
DIM vHdrNUMFRAMES[3] AS UBYTE AT $50D
DIM Volume AS UBYTE AT $520

DIM zHeader[8] AS UBYTE AT $522
DIM wHeader[44] AS UBYTE AT $522
DIM AUD_Header[44] AS UBYTE AT $522

DIM AUDIO_CTRL AS UBYTE AT $9F3B
CONST joystick_get AS UWORD = $FF56

boingsnd:
ASMBINARY "dull.raw"
endboing:

spritelogo1:
ASMBINARY "XHD.Z"

spritelogo2:
ASMBINARY "X16-64.Z"

spritelogo3:
ASMBINARY "AUD64.Z"

reelsprites:
ASMBINARY "REEL.SPRS.Z"
reelend:

defpal:

playenginez:
ASMBINARY "VID.ENGINE.Z"
endenginez:

playengineSPI:
ASMBINARY "SPI.ENGINE.Z"
endengineSPI:


' STD ENGINE ROUTINES
EXTSUB $A000 = PlayVideo_STD(PCMParms AS UWORD @R0) CLOBBERS(A, X, Y)

' Clears TextBuffer at 1:$B000, GraphBuffer at 0:$0000 and the Sprite Data Buffer at 1:$3000
' (fills with 0)
EXTSUB $A003 = ClearAllBuffers() CLOBBERS(A, X, Y)

EXTSUB $A006 = Engine_SetAudioCode(AudParms1 AS UWORD @R0, AudParms2 AS UWORD @R1, AbuffRem AS UBYTE @R2) CLOBBERS(A)
EXTSUB $A009 = SetSpriteMovie8bit(BuffSizes AS UWORD @R3, SpriteCount AS UBYTE @R4, EndSprAttrs AS UWORD @R5, EndSprData AS UWORD @R6) CLOBBERS(A, X)
EXTSUB $A00C = SetSpriteMovie4bit(BuffSizes AS UWORD @R3, SpriteCount AS UBYTE @R4, EndSprAttrs AS UWORD @R5, EndSprData AS UWORD @R6) CLOBBERS(A, X)
EXTSUB $A00F = SetBitMapMovie(BuffSizes AS UWORD @R3, WidthByte AS UBYTE @R2, PaletteSize AS UBYTE @R4) CLOBBERS(A, X)
EXTSUB $A012 = SET_FRAME_DRIVER(DriverAddr AS UWORD @XY)

' Clears the Graphics Buffer at 0:0000 for 76800 bytes.  (execution time app. 2.7 jiffies)
EXTSUB $A015 = ClearGraphBuffer() CLOBBERS(A, X, Y)
' basically CLS (but always to color 0).  Not sure there's an advantage here but it is also
' called by the Above ClearAllBuffers.
EXTSUB $A018 = ClearTextBuffer() CLOBBERS(A, X, Y)

' Clears the Sprite Buffer at 1:$3000
EXTSUB $A01B = ClearSpriteBuffer() CLOBBERS(A, X, Y)

EXTSUB $A021 = SET_PCM_ONLY() CLOBBERS(A, X)

' Only valid value to pass at the moment is 4 which aligns with the Video LFN for
' Video type 6 & 8 (with embedded Audio).
' eventually both LFN's will be assignable.   For NOW video LFN is 4
' Audio LFN if a seperate file is 6 (same as Vid LFN if embedded)
EXTSUB $A01E = SetAudioLFN(AudLFN AS UBYTE @R0) CLOBBERS(A)

EXTSUB $A024 = HIDEALLSPRITES() CLOBBERS(A, X)

' Decrements all RGB Values in the VERA Palette towards 0 individually by 1.
' If DOBLUE is true then the B value is instead shifted towards $F by 1
EXTSUB $A027 = PALSHIFT(DOBLUE AS BOOL @X) CLOBBERS(A, X, Y)

EXTSUB $A02A = SET_DEFAULT_PALETTE() CLOBBERS(A, X, Y)
EXTSUB $A02D = SETEXTERNAL_CONTROLS() CLOBBERS(X, Y)
EXTSUB $A030 = TURN_OFF_PALETTE_HANDLING() CLOBBERS(A)        'SYS $A030
EXTSUB $A033 = TURN_ON_PALETTE_HANDLING() CLOBBERS(A)         'SYS $A033
' END STANDARD ENGINE ROUTINES


' SPI ENGINE ROUTINES
EXTSUB $A000 = PlayVideo_SPI() CLOBBERS(A, X, Y)
EXTSUB $A003 = CLEARALLBUFFERS() CLOBBERS(A, X, Y)
EXTSUB $A006 = CLEARGRAPH() CLOBBERS(A, X, Y)
EXTSUB $A00F = SETVIDREAD_BITMAP() CLOBBERS(A, X, Y)
EXTSUB $A012 = SETVIDREAD_BITMAP_LOW() CLOBBERS(A, X, Y)
EXTSUB $A015 = SETAUDIOREAD_SPI() CLOBBERS(A, X)
EXTSUB $A018 = SET_FRAME_38K() CLOBBERS(A)
EXTSUB $A01B = SET_FRAME_51K() CLOBBERS(A)
EXTSUB $A01E = SET_FRAME_62K() CLOBBERS(A)
EXTSUB $A021 = RESETPCM() CLOBBERS(A, X)
' END SPI ENGINE ROUTINES


' will be set to $FF if Video is exited by the user
' in the Bank where the Video Engine code is located
DIM MANEXIT AS UBYTE AT $BFFF

DIM AUD_LEAD_START[4] AS UBYTE AT $BFA0

DIM DO_BUFFER_AUDIO AS BOOL AT $27
DIM G_WIDTH AS UBYTE AT $28

DIM VERA_CFG_BYTE AS BYTE AT $23

DIM FRAME_CURRENT[3] AS UBYTE AT $34
DIM FRAME_TARGET[3] AS UBYTE AT $37

DIM VID_SECTOR_START[4] AS UBYTE AT $3A
DIM VID_SECTOR_STARTw[2] AS UWORD @nosplit AT $3A

DIM VID_SECTOR_CURRENT[4] AS UBYTE AT $3E
DIM VID_SECTOR_CURRENTw[2] AS UWORD @nosplit AT $3E

DIM AUD_SECTOR_START[4] AS UBYTE AT $42
DIM AUD_SECTOR_STARTw[2] AS UWORD @nosplit AT $42

DIM AUD_SECTOR_CURRENT[4] AS UBYTE AT $46
DIM AUD_SECTOR_CURRENTw[2] AS UWORD @nosplit AT $46

DIM AUD_SECTOR_ADDER AS UBYTE AT $50
DIM VID_SECTOR_ADDER AS UBYTE AT $51

DIM AUD_FRAME_NUMSECTORS AS UBYTE AT $52
DIM VID_FRAME_NUMSECTORS AS UBYTE AT $53

DIM fsector[4] AS UBYTE
DIM MAKE_WIDE_SCREEN AS BOOL
DIM DISABLE_WIDE_SCREEN AS BOOL = FALSE

ALIAS f_size_low = cx16.r2
ALIAS f_size_high = cx16.r3

DIM butterfly[85] AS UBYTE = [$8F,$93,$9C,$12,$DF,$92,$20,$20,$20,$20,$20,$12,$A9,$0D,$9A,$12,$A5,$DF,$92,$20,$20,$20,$12,$A9,$A7,
                       $92,$0D,$9F,$12,$B5,$20,$DF,$92,$20,$12,$A9,$20,$B6,$0D,$1E,$20,$B7,$12,$BB,$92,$20,$12,$AC,$92,$B7,
                       $0D,$9E,$20,$AF,$12,$BE,$92,$20,$12,$BC,$92,$AF,$0D,$81,$AA,$12,$20,$92,$A9,$20,$DF,$12,$20,$92,$B4,
                       $0D,$1C,$B6,$A9,$20,$20,$20,$DF,$B5,0]

DIM STASHVEC AS UBYTE AT $03B2
DIM AUDIO_FIFO AS UBYTE AT $9F3D
DIM AUDIO_FIFOs AS BYTE AT $9F3D
DIM VERA_AUDIORATE AS UBYTE AT $9F3C

DIM IS_VIDEO AS BOOL
DIM IS_BITMAP_VIDEO AS BOOL
DIM IS_SPRITE_VIDEO AS BOOL
DIM IS_SUPPORTED_VIDEO AS BOOL
DIM VID_HAS_PALETTE AS BOOL
DIM IS_STD_VIDEO AS BOOL
DIM IS_SPI_VIDEO AS BOOL
DIM AUDIO_TRACK_IN_SEPERATEFILE AS BOOL
DIM VID_FILE_FOUND AS BOOL
DIM IS_Z_FILE AS BOOL
DIM IS_WAV_FILE AS BOOL
DIM PCM_FILE_FOUND AS BOOL
DIM PAL_FILE_FOUND AS BOOL
DIM IS_PCM_ONLY AS BOOL

DIM Load_816_SPICode AS BOOL
DIM Load_816_STDCode AS BOOL

DIM UseStandAlonePlayer AS BOOL
DIM ShowLoadMessages AS BOOL

DIM PCM_IS_STEREO AS BOOL
DIM PCM_IS_16BIT AS BOOL

DIM VERAcfgvar AS UBYTE = 0
DIM drivenumber AS UBYTE = 8
DIM OB AS UBYTE

DIM FILEID AS STRING = iso:"SPRV"
DIM BASENAME AS STRING = "?"*96
DIM PAD AS STRING = " "*4
DIM VIDFILE AS STRING = "MOVIE"+"?"*96
DIM PAD3 AS STRING = " "*4
DIM AUDFILE AS STRING = "?"*96
DIM PALFILE AS STRING = "?"*96
DIM PAD4 AS STRING = " "*4
DIM PRGFILE AS STRING = "?"*96
DIM PAD5 AS STRING = " "*4
DIM TMPFILE AS STRING = "?"*96
DIM VEXT AS STRING = iso:".SPV"
DIM AEXT AS STRING = iso:".SVA"
DIM PEXT AS STRING = iso:".PRG"
DIM ZEXT AS STRING = iso:".ZCM"
DIM WEXT AS STRING = iso:".WAV"
DIM FOUND_EXT AS STRING = ".XXX" + "?"*6
DIM AnyMessage AS STRING = iso:"\x9BAny \x05KEY\x9B to continue\x99."
DIM SPI816 AS STRING = "ASSETS/SPI.816.BIN"
DIM STD816 AS STRING = "ASSETS/STD.816.BIN"
DIM ENABLE65816 AS STRING = "ENABLE65816CODE"


DIM i AS UBYTE = 0
DIM j AS UBYTE = 0
DIM tmpw AS UWORD
DIM tmpw2 AS UWORD

DIM VideoLFN AS UBYTE = 4
DIM AudioLFN AS UBYTE = 6
DIM STD_ENGINE_BANK AS UBYTE = 10
DIM SPI_ENGINE_BANK AS UBYTE = 11
DIM HIRAMBANK AS UBYTE = 30

DIM FrameBufferSize AS UWORD
DIM PalBufferSize AS UWORD
DIM NumSprites AS UWORD
DIM SpriteAttr_EndAddress AS UWORD
DIM SpriteBlock2_DataAddress AS UWORD
DIM VBuff_255 AS UBYTE
DIM VBuff_Remainder AS UBYTE
DIM ABuff_255 AS UBYTE
DIM ABuff_Remainder AS UBYTE

DIM success AS BOOL = FALSE
DIM UseBookMark AS BOOL = FALSE

DIM SpriteOrigAddress AS UWORD
DIM SpriteBuffSize AS UWORD

DIM XOrg AS WORD = 0
DIM YOrg AS WORD = 0

ALIAS NumFramesHI = cx16.r1
ALIAS NumFramesLO = cx16.r2

DIM Frame AS FLOAT = 0.0
DIM tmpf AS FLOAT = 0.0
DIM tmpf2 AS FLOAT = 0.0
DIM play_total AS FLOAT
DIM play_left AS FLOAT
DIM PHours AS FLOAT
DIM PMins AS FLOAT
DIM PSecs AS FLOAT
DIM NumFrames AS FLOAT = 0.0
DIM AudFileSize AS FLOAT = 0.0
DIM vidFilePosition AS FLOAT
DIM audFilePosition AS FLOAT
DIM BFrame AS FLOAT = 0.0

DIM VidFile_SECTOR_Position AS FLOAT
DIM AudFile_SECTOR_Position AS FLOAT
DIM VidFileStartSector AS FLOAT
DIM AudFileStartSector AS FLOAT
DIM Seeker AS FLOAT

DIM F_FPS AS FLOAT
DIM TRUE_FPS AS FLOAT


ASMSUB float2long(FPtr AS UWORD @AY, lptr AS UWORD @R1)
   ASM
                MOVFM = $fe63
                QINT  = $fe8d
                FAC   = $c3
                FACHO = FAC + 1

float2long:     ldx $01
                phx
                ldx #$04
                stx $01
                jsr MOVFM
                jsr QINT
                ldx #3
                ldy #0
:               lda FACHO,x
                sta (cx16.r1),y
                iny
                dex
                bpl :-
                rts
   END ASM
END ASMSUB

SUB PrintINT32(i32[] AS UBYTE)
   FOR i = 3 TO 0 STEP -1
       txt.print(conv.str_ubhex(i32[i]))
   NEXT
END SUB

SUB RESTORE_LINES()
    poke $9F25, 2
    poke $9F2B, 0
    poke $9F2C, 120
    poke $9F25, 0
END SUB


SUB Boing()
    VERA_AUDIORATE = 0
    IF Volume < 8 THEN
        AUDIO_CTRL = 8
    ELSE
        AUDIO_CTRL = Volume
    END IF
    cx16.memory_copy(&boingsnd,&AUDIO_FIFO, &endboing - &boingsnd)
    VERA_AUDIORATE = 26
    sys.wait(22)
    VERA_AUDIORATE = 0
    ASM
kflush: jsr cbm.GETIN
        bne kflush
    END ASM
END SUB

SUB LoadHD_Logo(lX AS WORD, lY AS WORD, ZsprPtr AS UWORD, sprn AS UBYTE, sOffset AS UBYTE, ShowIt AS BOOL)
    cx16.VERA_ADDR_H = %00010001
    cx16.VERA_ADDR_M = sOffset
    cx16.VERA_ADDR_L = $00
    cx16.memory_decompress(ZsprPtr,&cx16.VERA_DATA0)
    spr_init(sprn, 1, mkword(sOffset,0), sprites.SIZE_64, sprites.SIZE_64, 128, 0,ShowIt)
    sprites.pos(sprn, lX,lY)
    IF ShowIt THEN sprites.show(sprn)
END SUB

SUB Load_FilmReel_Image()
    DIM x AS WORD
    DIM y AS WORD
    cx16.VERA_ADDR_H = %00010000
    cx16.VERA_ADDR_M = 0
    cx16.VERA_ADDR_L = 0
    cx16.memory_decompress(&reelsprites,&cx16.VERA_DATA0)
    i = 5
    tmpw = 0
    FOR y = 143 TO 207 STEP 32
     FOR x = 373 TO 565 STEP 64
        spr_init(i,0,tmpw,sprites.SIZE_64, sprites.SIZE_32, 128, 0,FALSE)
        sprites.pos(i,x,y)
        i++
        tmpw = tmpw + 2048
     NEXT
    NEXT
    FOR i = 5 TO 17
        sprites.show(i)
    NEXT
END SUB

SUB bload(filename AS STRING, bank1 AS UBYTE, addr AS UWORD @R0)
    ASM
           ldx 2
           ldy 3
           phy
           phx
    END ASM
    cbm.SETNAM(strings.length(filename), filename)
    ASM
           lda #0
           ldx p8v_drivenumber
           ldy #2
           jsr cbm.SETLFS
    END ASM
    OB = peek(0)
    cx16.rambank(bank1)
    ASM
            plx
            ply
            lda #0
            jsr cbm.LOAD
    END ASM
     cx16.rambank(OB)
END SUB

DIM banks[26] AS UBYTE
DIM sImages[26] AS UWORD
DIM curImageIdx AS UBYTE
SUB NextSpriteFrame()
    OB = peek(0)
    IF curImageIdx = 24 THEN
        curImageIdx=0
    ELSE
        curImageIdx = curImageIdx + 2
    END IF
    cx16.rambank(banks[curImageIdx])
    cx16.VERA_ADDR_L = 0
    cx16.VERA_ADDR_M = $32
    cx16.VERA_ADDR_H = %00010001
    cx16.memory_copy(sImages[curImageIdx], &cx16.VERA_DATA0, 2048)
    cx16.rambank(OB)
END SUB

SUB InitTapeAnimSprites()
   sprites.init(10, 1, $3200, sprites.SIZE_32, sprites.SIZE_32, 128, 0)
   sprites.init(11, 1, $3600, sprites.SIZE_32, sprites.SIZE_32, 128, 0)
   sprites.pos(10,92,74)
   sprites.pos(11,196,74)
   sprites.show(10)
   sprites.show(11)
   NextSpriteFrame()
END SUB

SUB LoadTapeImage()
    LoadVERAImage("ASSETS/CASSETTE.IMG")
END SUB

SUB LoadAudioCDImage()
    LoadVERAImage("ASSETS/CDROM.IMG")
END SUB

SUB DO_VLOAD()
    WHILE cbm.READST() = 0
      ASM
             sec
             lda #0
             ldx #$23
             ldy #$9F
             jsr cx16.MACPTR
      END ASM
    WEND
   cbm.CLRCHN()
   cbm.CLOSE(7)
END SUB

SUB DO_VOPEN(VFILE AS STRING)
    fopen(7,drivenumber,7,VFILE)
    cbm.CHKIN(7)
END SUB

SUB LoadVERAImage(IMAGEFILE AS STRING)
    DO_VOPEN(IMAGEFILE)
    cx16.VERA_ADDR_L = 0
    cx16.VERA_ADDR_M = 0
    cx16.VERA_ADDR_H = %00010000
    DO_VLOAD()
    cx16.memory_fill($9F23,15040,0)
END SUB

SUB LoadVERAPalette(PFILE AS STRING, StartIndx AS UBYTE)
   DIM VAddr AS UWORD
   VAddr = $FA00 + ((StartIndx AS UWORD) * 2)
   DO_VOPEN(PFILE)
   cx16.VERA_ADDR_L = lsb(VAddr)
   cx16.VERA_ADDR_M = msb(VAddr)
   cx16.VERA_ADDR_H = %00010001
   DO_VLOAD()
END SUB

SUB PrintPlayerControls(StartRow AS UBYTE, PrintTitle AS BOOL)
    MyPlot(StartRow,24)
    txt.print(" \x1E[\x05\x80\x18\x80\x19\x1E]\x99-\x9BVolume")
    MyPlot(StartRow+1,0)
    txt.print(" \x1E[\x05SPC\x1E]\x99-\x9BPause Play")
    txt.print("       \x1E[\x05ESC\x1E]\x99-\x9BExit Play")
    IF strings.length(AUDFILE) > 36 THEN
        strings.slice(AUDFILE,0,36,TMPFILE)
    ELSE
        strings.copy(AUDFILE,TMPFILE)
    END IF
    IF PrintTitle THEN
        MyPlot(1,3)
        txt.print(TMPFILE)
    END IF
END SUB

SUB SetSpritePointers(startbank AS UBYTE)
  DIM imgctr AS UBYTE
  OB = startbank
  curImageIdx = 0
  tmpw = $A000
  imgctr = 0
  WHILE curImageIdx < 26
        banks[curImageIdx] = OB
        banks[curImageIdx+1] = OB
        sImages[curImageIdx] = tmpw
        sImages[curImageIdx+1] = tmpw + 1024
        imgctr = imgctr + 2
        curImageIdx = curImageIdx + 2
        tmpw = tmpw + 2048
        IF imgctr = 8 THEN
            imgctr=0
            tmpw=$A000
            OB++
        END IF
  WEND
  curImageIdx = 24
END SUB

SUB TapeAnimation_INIT()
    bload("ASSETS/TAPESPRITES.BIN", 14, $A000)
    SetSpritePointers(14)
    cx16.screen_mode($80,FALSE)
    HideBothLayers()
    LoadTapeImage()
    InitTapeAnimSprites()
    txt.color2(11,0)
    PrintPlayerControls(28,FALSE)
    txt.color2(1,0)
    MyPlot(25,0)
    PrintPCMFormat()
    cx16.GRAPH_set_colors($10,4,6)
    Graph_CHAR(TMPFILE,57,45)
    SetDefaultPalette()
    ShowBothLayers()
END SUB

FUNCTION TapeAnimationAvailable() AS BOOL
    RETURN FExists("ASSETS/CASSETTE.IMG",19) AND FExists("ASSETS/TAPESPRITES.BIN",22)
END FUNCTION

SUB MAKE_ALL_FALSE()
  success = FALSE
  IS_VIDEO = FALSE
  IS_SUPPORTED_VIDEO = FALSE
  IS_SPI_VIDEO = FALSE
  IS_STD_VIDEO = FALSE
  IS_Z_FILE = FALSE
  IS_WAV_FILE = FALSE
  PCM_IS_16BIT = FALSE
  PCM_IS_STEREO = FALSE
END SUB

SUB PrintTime(seconds AS FLOAT)
    PHours = floats.floor(seconds/3600.0)
    tmpf = seconds - (PHours * 3600.0)
    PMins = floats.floor(tmpf/60.0)
    PSecs = floats.round(tmpf - (PMins*60))

      txt.print("\x05")
      IF PHours < 10 THEN cbm.CHROUT("0"c)
      floats.print(PHours)

      txt.print("\x99:\x05")
      IF PMins < 10 THEN cbm.CHROUT("0"c)
      floats.print(PMins)

      txt.print("\x99:\x05")
      IF PSecs < 10 THEN cbm.CHROUT("0"c)
      floats.print(PSecs)
END SUB

ASMSUB Flush15() CLOBBERS(A,X,Y)
  ASM
          ldx #15
          jsr cbm.CHKIN
          jmp p8s_FExists.flushchars
  END ASM
END ASMSUB


SUB Woop()
    VERA_AUDIORATE = 0
    DIM freint AS UBYTE = 100
    DIM Amplitude AS BYTE = 30

  REPEAT 20
   REPEAT 2
    REPEAT freint
       AUDIO_FIFOs = Amplitude
    END REPEAT
    REPEAT freint
       AUDIO_FIFOs = (0 - Amplitude)
    END REPEAT
   END REPEAT
   freint = freint - 5
   Amplitude = Amplitude + 4
  END REPEAT
    AUDIO_CTRL = Volume
    VERA_AUDIORATE = 35
    sys.wait(10)
END SUB


ASMSUB FExists(FName AS UWORD @XY, NameLength AS UBYTE @A) CLOBBERS(A,X,Y) AS BOOL @Pc
   ASM
             jsr cbm.SETNAM
             lda #11
             ldx p8v_drivenumber
             ldy #0
             jsr cbm.SETLFS
             jsr cbm.OPEN
             lda #11
             jsr cbm.CLOSE
             lda #0
             ldx #<dummy
             ldy #>dummy
             jsr cbm.SETNAM
             lda #15
             ldx p8v_drivenumber
             ldy #15
             jsr cbm.SETLFS
             jsr cbm.OPEN
             ldx #15
             jsr cbm.CHKIN
             jsr cbm.GETIN
             cmp #"0"
             bne NotFound
             jsr cbm.GETIN
             cmp #"0"
             beq Found
NotFound:    jsr flushchars
             clc
             rts
Found:       jsr flushchars
             sec
             rts
flushchars:  jsr cbm.GETIN
             cmp #13
             bne flushchars
             lda #15
             jsr cbm.CLOSE
             jsr cbm.CLRCHN
             jmp cbm.READST
dummy        .byte 0
; !notreached!
   END ASM
END ASMSUB

SUB ClearUpperRows()
   txt.color2(1,0)
   MyPlot(0,0)
   REPEAT 5
       REPEAT 80 : cbm.CHROUT(32) : END REPEAT
   END REPEAT
   MyPlot(4,0)
   REPEAT 80 : cbm.CHROUT(95) : END REPEAT
   MyPlot(0,0)
END SUB

SUB Graph_CHAR(s AS STRING, X AS UWORD, Y AS UWORD)
  j = strings.length(s)
  IF j>0 THEN
     j--
     cx16.GRAPH_put_char(X,Y, $06)
     FOR i = 0 TO j
       cx16.GRAPH_put_next_char(s[i])
     NEXT
  END IF
END SUB

SUB PrepareRun(PNAME AS STRING)
    sprites.reset(1,126)
    WHILE cbm.GETIN2() <> 0 : WEND
    SetDefaultPalette()
    cx16.set_screen_mode(1)
    txt.color2(6,6)
    txt.clear_screen()
    showTextLayer()
    txt.row(2)
    txt.print("^") : txt.print(PNAME)
    cx16.kbdbuf_put(13)
    FOR i = 0 TO 2 : cx16.kbdbuf_put(145) : NEXT
    cx16.kbdbuf_put(13)
END SUB


FUNCTION VERA2Hz() AS FLOAT
    RETURN (VideoHeader.VERARate AS FLOAT / 128.0) * 48828.15
END FUNCTION

SUB GrabBaseName()
    cx16.r2 = $BF00
    FOR cx16.r12L = 0 TO 94
        i = cx16.fetch(cx16.r2L, 0, cx16.r12L)
        IF i=0 THEN BREAK
        BASENAME[cx16.r12L] = i
    NEXT
    BASENAME[cx16.r12L+1]=0
END SUB

FUNCTION check_A_Ext(extension AS STRING, TARGET AS STRING) AS BOOL
    strings.copy(BASENAME,TARGET)
    strings.append(TARGET,extension)
    tmpB2 = FExists(TARGET, strings.length(TARGET))

    IF tmpB2 THEN strings.copy(extension, FOUND_EXT) ELSE FOUND_EXT[0] = 0
    RETURN tmpB2
END FUNCTION

SUB FIND_AUDIO_FILE()
    PCM_FILE_FOUND = FALSE
    IS_Z_FILE = FALSE
    IS_WAV_FILE = FALSE
    PCM_FILE_FOUND = check_A_Ext(ZEXT,AUDFILE)
    IF PCM_FILE_FOUND THEN
        IS_Z_FILE = TRUE
        GOTO DoneAudioSearch
    END IF
    PCM_FILE_FOUND = check_A_Ext(WEXT,AUDFILE)
    IF PCM_FILE_FOUND THEN IS_WAV_FILE = TRUE
DoneAudioSearch:
    IS_PCM_ONLY = PCM_FILE_FOUND
    RETURN
END SUB

SUB FIND_PAL_FILE()
    PAL_FILE_FOUND = check_A_Ext(".PAL",PALFILE)
    RETURN
END SUB


FUNCTION directory() AS BOOL
        DIM lastchar AS UBYTE @zp = 1
        DIM inquote AS BOOL = FALSE

        ' -- Prints the directory contents to the screen. Returns success.

        cbm.SETNAM(11, "$:*=P")
internal_dir:
        cbm.SETLFS(11, drivenumber, 0)
        DIM status AS UBYTE = 1
        VOID cbm.OPEN()          ' open 11,8,0,"$"
        IF_CS
            GOTO io_error
        END IF

        ASM
                ldx #11
                jsr cbm.CHKIN
        END ASM

        REPEAT 4
            VOID cbm.CHRIN()     ' skip the 4 prologue bytes
        END REPEAT

        ' while not stop key pressed / EOF encountered, read data.
        status = cbm.READST()
        IF status<>0 THEN
            status = 1
            GOTO io_error
        END IF
        DIM LineCount AS UBYTE = 0
        WHILE status=0
            DIM low AS UBYTE = cbm.CHRIN()
            DIM high AS UBYTE = cbm.CHRIN()
            IF LineCount = 27 THEN
                txt.print("\x0D\x99...\x96more  \x99[\x9ELeft\x9F-\x9ECtrl\x99]     [\x9EENTER\x99] \x9F- \x05Done")
                ASM
getnextbut:                      lda #0
                                 jsr p8c_joystick_get
                                 and #%00010000
                                 beq p8l_io_error
                                 txa
                                 and #128
                                 bne getnextbut
donewaitlist:                    nop
                END ASM
                txt.print("\x0D")
                LineCount=0
            END IF
            DIM character AS UBYTE @zp
            i = 0
            REPEAT
                character = cbm.CHRIN()
                IF character=0 THEN
                    BREAK
                END IF
                IF character = 34 THEN inquote = NOT inquote
                IF inquote AND character <> 34 THEN
                    AUDFILE[i] = character
                    i++
                End If
            END REPEAT
            AUDFILE[i] = 0
            ISOUpper(AUDFILE)
            DIM IsAud AS BOOL = (strings.compare(&AUDFILE + (i-3) AS UWORD,"ZCM") = 0) OR (strings.compare(&AUDFILE + (i-3) AS UWORD,"WAV")=0)
            IF strings.compare(&AUDFILE + (i-3) AS UWORD,"SPV") = 0 OR IsAud THEN
               txt.print(" \x9B") : txt.print_uw(mkword(high, low)) : txt.column(10)
               IF IsAud THEN txt.print("\x99") ELSE txt.print("\x05")
               txt.print(AUDFILE) : LineCount++ : txt.nl()
            END IF

            VOID cbm.CHRIN()     ' skip 2 bytes
            VOID cbm.CHRIN()
            status = cbm.READST()
            VOID cbm.STOP()
breaker:
            IF_Z
                BREAK
            END IF
        WEND
        status = cbm.READST()

io_error:
        ASM {{                                 ; restore default i/o devices
               jsr cbm.CLRCHN
               lda #11
               jsr cbm.CLOSE
        }}
        IF status<>0 AND status & $40 = 0 THEN            ' bit 6=end of file
            txt.print("\ni/o error, status: ")
            txt.print_ub(status)
            txt.nl()
            RETURN FALSE
        END IF
        RETURN TRUE
END FUNCTION

SUB ShowDirectory_LIST()
       txt.clear_screen()
       txt.print("\x0D\x05")
       directory()
       ASM
gl:            jsr cbm.GETIN
               bne gl
       END ASM
       txt.print("\x0D\x0D")
       WaitForKey_RestoreColor()
END SUB

DIM WarnCount AS UBYTE = 0
SUB SPI_WARN()
    IF WarnCount > 2 THEN RETURN
    WarnCount++
    ClearUpperRows()
    MyPlot(1,1)
    txt.print(" \x9BYou are running a \x96prerelease \x99r49 \x05ROM.")
    MyPlot(2,1)
    txt.print(" \x96This \x99MAY\x96 work \x1E!\x1F....\x9B[\x05Recommend \x99UPGRADE\x9B]\x05")
    MyPlot(3,2)
    WaitForKey_RestoreColor()
END SUB

SUB MakeStandAlonePlayer()
    Seeker = $90 AS FLOAT
    IF IS_SPI_VIDEO THEN strings.copy("ASSETS/SPI-TEMPLATE.PRG",TMPFILE) ELSE strings.copy("ASSETS/STD-TEMPLATE.PRG",TMPFILE)
    ClearUpperRows()
    MyPlot(1,1)
    IF FExists(TMPFILE,strings.length(TMPFILE)) THEN
       txt.print("\x96 CREATING\x1C: \x9E")
       strings.copy("@:", AUDFILE)
       strings.append(AUDFILE,BASENAME)
       strings.append(AUDFILE,".PRG,S,W")
       txt.print(BASENAME) : txt.print(".PRG")
       strings.append(TMPFILE,",S,R")
       fopen(7,drivenumber,7,TMPFILE)
       fopen(6,drivenumber,6,AUDFILE)
       cx16.rambank(HIRAMBANK)
       txt.print(" \x99")
       ASM
              stz $BFFE
cloop:        ldx #7
              jsr cbm.CHKIN
              ldx #0
              ldy #$A0
              lda #240
              clc
              jsr cx16.MACPTR
              phx             ; Put bytes read on the stack for the write
              jsr cbm.READST
              sta $BFFF
              ldx #6
              jsr cbm.CHKOUT
              pla             ; write out as many bytes as we read in
              ldx #0
              ldy #$A0
              jsr cx16.MCIOUT
              inc $BFFE
              lda $BFFE
              cmp #4          ; print a "." every 4 chunks
              bne checkEOF
              jsr cbm.CLRCHN
              lda #"."
              jsr cbm.CHROUT
              stz $BFFE
checkEOF:     lda $BFFF
              beq cloop
       END ASM
       cbm.CLRCHN()
       cbm.CLOSE(7)
       f_seek(6, Seeker)
       cbm.CHKOUT(6)
       i = 0
       WHILE BASENAME[i] <> 0 : cbm.CHROUT(BASENAME[i]) : i+=1 : WEND
       cbm.CHROUT(0)
       cbm.CLRCHN()
       cbm.CLOSE(6)
       txt.print("\x99 Done \x05!")
    ELSE
       txt.print("TEMPLATE PROGRAM NOT FOUND\x0D\x0D")
    END IF
END SUB

SUB MyPlot(y AS UBYTE, x AS UBYTE)
    txt.row(y)
    txt.column(x)
END SUB


SUB ProgramBanner()
    i=0
    REPEAT 85 : cbm.CHROUT(butterfly[i]) : i +=1 : END REPEAT
    MyPlot(1,10)
    txt.print ("\x9C*\x9A*\x99* \x05X16 STREAMING MEDIA PLAYER ") : txt.print(PENV.PETSCII_VERSTR) : txt.print(" \x9E*\x81*\x1C*\x05")
    MyPlot(2,14)
    IF PENV.ON_EMU THEN txt.print("EMULATOR") ELSE txt.print("HARDWARE")
    MyPlot(3,10)
    txt.print ("\x9BAUTHOR\x1E: \x05ANTHONY W. HENRY")
    MyPlot(5,10)
    txt.print("\x9ETHIS SOFTWARE IS UNDER THE MIT LICENSE")
    MyPlot(6,10)
    txt.print("    \x05(\x9BPERMISSIVE\x05)")
END SUB

SUB ISOUpper(s AS STRING)
  i=0
  WHILE s[i] <> 0
      IF s[i] > 96 AND s[i]<123 THEN s[i] -= 32
      i+=1
  WEND
END SUB

SUB BlankPAL_LAST32()
    cx16.VERA_ADDR_L = $C0
    cx16.VERA_ADDR_M = $FB
    cx16.VERA_ADDR_H = %00010001
    REPEAT 16 : UNROLL 4 : cx16.VERA_DATA0 = 0 : END UNROLL : END REPEAT
END SUB

SUB SetDefaultPalette()
    OB = peek(0)
    cx16.rambank(STD_ENGINE_BANK)
    SET_DEFAULT_PALETTE()
    cx16.rambank(OB)
END SUB

SUB pal2black()
    cx16.VERA_ADDR_L = 0
    cx16.VERA_ADDR_M = $FA
    cx16.VERA_ADDR_H = %00010001
    REPEAT 128
      UNROLL 4 : cx16.VERA_DATA0 = 0 : END UNROLL
    END REPEAT
END SUB

SUB pal2blue()
    cx16.VERA_ADDR_L = 0
    cx16.VERA_ADDR_M = $FA
    cx16.VERA_ADDR_H = %00010001
    REPEAT 128
      UNROLL 2 : cx16.VERA_DATA0=$0F : cx16.VERA_DATA0=0 : END UNROLL
    END REPEAT
END SUB

SUB hideGraphicsLayer()
    cx16.VERA_DC_VIDEO = cx16.VERA_DC_VIDEO & 239
END SUB

SUB showGraphicsLayer()
    cx16.VERA_DC_VIDEO = cx16.VERA_DC_VIDEO | 16
END SUB

SUB hideTextLayer()
    cx16.VERA_DC_VIDEO = cx16.VERA_DC_VIDEO & 223
END SUB

SUB showTextLayer()
    cx16.VERA_DC_VIDEO = cx16.VERA_DC_VIDEO | 32
END SUB

SUB HideBothLayers()
    hideGraphicsLayer()
    hideTextLayer()
END SUB

SUB ShowBothLayers()
    showGraphicsLayer()
    showTextLayer()
END SUB

SUB setMovieVideoMode()
    IF IS_SPRITE_VIDEO THEN RETURN
    IF VideoHeader.FrameWidth=320 THEN
      cx16.set_screen_mode($80)
    ELSE
      cx16.set_screen_mode(0)
    END IF
END SUB

SUB spr_init(spritenum AS UBYTE,
             databank AS UBYTE, dataaddr AS UWORD,
             width_flag AS UBYTE, height_flag AS UBYTE,
             colors_flag AS UBYTE, palette_offset AS UBYTE, show AS BOOL)
        sprites.pos(spritenum, -64, -64)                    ' move sprite off-screen initially
        IF NOT show THEN cx16.vpoke(1, sprite_reg+6, %00000000)                     ' z depth %11 = in front of both layers, no flips
        cx16.VERA_DC_VIDEO |= %01000000             ' enable sprites globally
        dataaddr >>= 5
        dataaddr |= (databank AS UWORD)<<11
        DIM sprite_reg AS UWORD = sprites.VERA_SPRITEREGS + spritenum*$0008
        cx16.vpoke(1, sprite_reg, lsb(dataaddr))                    ' address 12:5
        cx16.vpoke(1, sprite_reg+1, colors_flag | msb(dataaddr))    ' 4 bpp + address 16:13
        IF show THEN cx16.vpoke(1, sprite_reg+6, %00001100)          ' z depth %11 = in front of both layers, no flips
        cx16.vpoke(1, sprite_reg+7, height_flag<<6 | width_flag<<4 | palette_offset&15) ' 64x64 pixels, palette offset
END SUB

SUB setup_SpriteGrid(X AS WORD @R7, Y AS WORD @R9)
    ALIAS X1 = cx16.r5
    ALIAS Y1 = cx16.r6
    DIM sprPal1 AS UBYTE = 0
    DIM sprPal2 AS UBYTE = 0
    DIM sprDepth AS UBYTE = 128
    DIM sprWidByte AS UBYTE = $FF
    DIM sprHeiByte AS UBYTE = $FF
    DIM sprNum AS UBYTE = 1
    DIM sprBank AS UBYTE = 0
    DIM sprBank2 AS UBYTE = 0
    X1  = X AS UWORD
    Y1  = Y AS UWORD
    DIM XLimit AS WORD = (X + VideoHeader.FrameWidth) AS WORD
    DIM sprBlock1Addr AS UWORD = SpriteOrigAddress
    DIM sprBlock2Addr AS UWORD = SpriteBlock2_DataAddress
    DIM BuffAdd AS UWORD = (VideoHeader.SpriteWidth AS UWORD * VideoHeader.SpriteHeight AS UWORD)

    IF VideoHeader.BPP = 4 THEN
       sprDepth = 0
       IF VID_HAS_PALETTE THEN
          sprPal1 = 14
          sprPal2 = 15
       ELSE
          IF PAL_FILE_FOUND THEN sprPal1 = 15 ELSE sprPal1 = 1
          sprPal2 = sprPal1
       END IF
       sprBank=1
       BuffAdd=BuffAdd/2
    END IF

   IF VideoHeader.SpriteWidth = 64 THEN sprWidByte=sprites.SIZE_64
   IF VideoHeader.SpriteWidth = 32 THEN sprWidByte=sprites.SIZE_32
   IF VideoHeader.SpriteWidth = 16 THEN sprWidByte=sprites.SIZE_16
   IF VideoHeader.SpriteWidth = 8 THEN sprWidByte= sprites.SIZE_8

   IF VideoHeader.SpriteHeight = 64 THEN sprHeiByte=sprites.SIZE_64
   IF VideoHeader.SpriteHeight = 32 THEN sprHeiByte=sprites.SIZE_32
   IF VideoHeader.SpriteHeight = 16 THEN sprHeiByte=sprites.SIZE_16
   IF VideoHeader.SpriteHeight = 8 THEN sprHeiByte=sprites.SIZE_8

   sprBank2 = sprBank

   REPEAT NumSprites
         sprites.init(sprNum, sprBank, sprBlock1Addr, sprWidByte, sprHeiByte, sprDepth, sprPal1)
         sprites.pos(sprNum, X1 AS WORD,Y1 AS WORD)
         sprites.init((sprNum+NumSprites) AS UBYTE, sprBank2, sprBlock2Addr, sprWidByte, sprHeiByte, sprDepth, sprPal2)
         sprites.pos((sprNum+NumSprites) AS UBYTE, X1 AS WORD,Y1 AS WORD)
         sprNum = sprNum+1
         X1 = X1 + VideoHeader.SpriteWidth
         IF X1>=XLimit THEN
            X1=X AS UWORD
            Y1=(Y1+VideoHeader.SpriteHeight) AS WORD
         END IF
         sprBlock1Addr = sprBlock1Addr + BuffAdd
         IF sprBlock2Addr >= (65535 - BuffAdd) THEN
              sprBlock2Addr = 512
              sprBank2 = 1
         ELSE
              sprBlock2Addr = sprBlock2Addr + BuffAdd
         END IF
   END REPEAT
END SUB

SUB fopen(lfn AS UBYTE, device AS UBYTE, sa AS UBYTE, filename AS STRING)
    cbm.SETLFS(lfn, device, sa)
    cbm.SETNAM(strings.length(filename), filename)
    VOID cbm.OPEN()
END SUB

FUNCTION numFrameSectors() AS UBYTE
    tmpw = FrameBufferSize / 512
    IF FrameBufferSize MOD 512 > 0 THEN tmpw = tmpw + 1
    RETURN tmpw AS UBYTE
END FUNCTION

SUB FLOAT_TO_INT32(src AS FLOAT, target[] AS UBYTE)
    tmpw = src/65536.0 AS UWORD
    target[3] = msb(tmpw)
    target[2] = lsb(tmpw)
    tmpw2 = (src - (tmpw AS FLOAT * 65536.0)) AS UWORD
    target[1] = msb(tmpw2)
    target[0] = lsb(tmpw2)
END SUB

FUNCTION INT32_TO_FLOAT(src[] AS UBYTE) AS FLOAT
    tmpw = src[2] + (src[3] AS UWORD * 256)
    RETURN (src[0] AS FLOAT) + (src[1] AS FLOAT * 256.0) + (tmpw AS FLOAT * 65536)
END FUNCTION

SUB CURFRAMEINT_TOFLOAT()
    Frame = (FRAME_CURRENT[0] AS FLOAT) + (256.0 * FRAME_CURRENT[1] AS FLOAT) + (65536.0 * FRAME_CURRENT[2] AS FLOAT)
END SUB

SUB CURFRAMEFLOAT_TOINT()
    tmpf = floats.floor((Frame / 65536.0))
    FRAME_CURRENT[2] = tmpf AS UBYTE
    tmpw = ( Frame - (tmpf * 65536.0) ) AS UWORD
    FRAME_CURRENT[1] = msb(tmpw)
    FRAME_CURRENT[0] = lsb(tmpw)
END SUB

SUB CURFRAME_ZERO()
    Frame = 0
    FRAME_CURRENT[0] = 0
    FRAME_CURRENT[1] = 0
    FRAME_CURRENT[2] = 0
END SUB

SUB SET_VID_STARTSECTOR()
    FOR i = 0 TO 3 : VID_SECTOR_START[i] = fsector[i] : NEXT
END SUB

SUB SET_AUD_STARTSECTOR()
    FOR i = 0 TO 3 : AUD_SECTOR_START[i] = fsector[i] : NEXT
END SUB

FUNCTION VIDSTART_TOFLOAT() AS FLOAT
    RETURN (VID_SECTOR_STARTw[0] AS FLOAT) + ((VID_SECTOR_STARTw[1] AS FLOAT)*65536.0)
END FUNCTION

SUB FLOATTO_VIDCURRENT(FStart AS FLOAT)
    VID_SECTOR_CURRENTw[1] = (FStart/65536.0) AS UWORD
    VID_SECTOR_CURRENTw[0] = ( FStart - (AUD_SECTOR_STARTw[1] AS FLOAT * 65536.0) ) AS UWORD
END SUB

FUNCTION AUDSTART_TOFLOAT() AS FLOAT
    RETURN (AUD_SECTOR_STARTw[0] AS FLOAT) + ((AUD_SECTOR_STARTw[1] AS FLOAT)*65536.0)
END FUNCTION

SUB FLOATTO_AUDCURRENT(FStart AS FLOAT)
    AUD_SECTOR_CURRENTw[1] = (FStart/65536.0) AS UWORD
    AUD_SECTOR_CURRENTw[0] = ( FStart - (AUD_SECTOR_STARTw[1] AS FLOAT * 65536.0) ) AS UWORD
END SUB

SUB CALC_FILE_STARTUP_SECTORS()
    VID_FRAME_NUMSECTORS = numFrameSectors()
    IF VideoHeader.VTYPE = 23 THEN
       VID_SECTOR_ADDER = VID_FRAME_NUMSECTORS + 1
       AUD_SECTOR_ADDER = AUD_FRAME_NUMSECTORS

       VidFile_SECTOR_Position = VidFileStartSector + 1
       AudFile_SECTOR_Position = AudFileStartSector

       IF AUD_FRAME_NUMSECTORS < 5 THEN
          AudFile_SECTOR_Position = AudFile_SECTOR_Position + (AUD_FRAME_NUMSECTORS AS FLOAT)
          FLOAT_TO_INT32(AudFileStartSector, AUD_LEAD_START)
       END IF
    END IF

    IF VideoHeader.VTYPE = 28 THEN
       VID_SECTOR_ADDER = VID_FRAME_NUMSECTORS + AUD_FRAME_NUMSECTORS + 1
       AUD_SECTOR_ADDER = VID_SECTOR_ADDER

       VidFile_SECTOR_Position = (VidFileStartSector + 1.0) + (AUD_FRAME_NUMSECTORS AS FLOAT)
       AudFile_SECTOR_Position = (VidFileStartSector + 1.0)

       IF AUD_FRAME_NUMSECTORS < 5 THEN
          VidFile_SECTOR_Position = VidFile_SECTOR_Position + (AUD_FRAME_NUMSECTORS AS FLOAT)
          FLOAT_TO_INT32(AudFile_SECTOR_Position, AUD_LEAD_START)
          AudFile_SECTOR_Position = AudFile_SECTOR_Position + (AUD_FRAME_NUMSECTORS AS FLOAT)
       END IF
    END IF
    IF UseBookMark THEN
        IF VideoHeader.BPP < 8 THEN
         IF floats.floor(BFrame / 2) <> (BFrame / 2) THEN BFrame = BFrame - 1
        END IF
        VidFile_SECTOR_Position = VidFile_SECTOR_Position + (BFrame * VID_SECTOR_ADDER AS FLOAT)
        AudFile_SECTOR_Position = AudFile_SECTOR_Position + (BFrame * AUD_SECTOR_ADDER AS FLOAT)
        Frame = BFrame
        DO_BUFFER_AUDIO = FALSE
    END IF
    FLOAT_TO_INT32(VidFile_SECTOR_Position, VID_SECTOR_CURRENT)
    FLOAT_TO_INT32(AudFile_SECTOR_Position, AUD_SECTOR_CURRENT)
END SUB

SUB SETUP_SPI_ENGINECODE()
    cx16.rambank(SPI_ENGINE_BANK)
    SETAUDIOREAD_SPI()
    IF FrameBufferSize < 38401.00 THEN
          SET_FRAME_38K()
    ELSE
       IF FrameBufferSize < 51201.00 THEN
          SET_FRAME_51K()
       ELSE
          SET_FRAME_62K()
       END IF
    END IF
    IF VideoHeader.BPP = 8 THEN SETVIDREAD_BITMAP()
    IF VideoHeader.BPP < 8 THEN SETVIDREAD_BITMAP_LOW()
END SUB

SUB CalcFrameSize()
    DIM adjust AS UBYTE
    IF VideoHeader.BPP = 1 THEN
        adjust=8
        PalBufferSize = 4
    END IF
    IF VideoHeader.BPP = 2 THEN 
        adjust=4
        PalBufferSize = 8
    END IF
    IF VideoHeader.BPP = 4 THEN 
        adjust=2
        PalBufferSize = 32
    END IF
    IF VideoHeader.BPP = 8 THEN 
        adjust=1
        PalBufferSize = 512
    END IF
    IF NOT VID_HAS_PALETTE THEN PalBufferSize=0
    FrameBufferSize = ((VideoHeader.FrameWidth AS FLOAT * VideoHeader.FrameHeight AS FLOAT) / adjust AS FLOAT) AS UWORD
END SUB

SUB CalculateSprites()
    NumSprites = (VideoHeader.FrameWidth/VideoHeader.SpriteWidth) * (VideoHeader.FrameHeight/VideoHeader.SpriteHeight)
    SpriteOrigAddress = 0
    SpriteBuffSize = VideoHeader.SpriteHeight AS UWORD * VideoHeader.SpriteWidth AS UWORD
    IF VideoHeader.BPP = 4 THEN
        SpriteBuffSize = SpriteBuffSize/2
        SpriteOrigAddress = $3000
    END IF
    SpriteAttr_EndAddress = $FC06 + (NumSprites*16)
    SpriteBlock2_DataAddress = SpriteOrigAddress + FrameBufferSize
    IF VideoHeader.BPP = 8 THEN SpriteBlock2_DataAddress = SpriteBlock2_DataAddress + 512
END SUB

FUNCTION calc_asize(filesize AS FLOAT, numframes AS FLOAT) AS UWORD
    tmpf = floats.round(filesize / numframes)
    IF IS_STD_VIDEO THEN
        IF PCM_IS_16BIT OR PCM_IS_STEREO THEN
           IF floats.floor(tmpf/2.0) <> (tmpf/2.0) THEN
              tmpf = tmpf - 1
           END IF
        END IF
        IF PCM_IS_16BIT AND PCM_IS_STEREO THEN
           WHILE (floats.floor(tmpf/4.0) <> (tmpf/4.0)) : tmpf = tmpf + 1 : WEND
        END IF
    END IF
  RETURN tmpf AS UWORD
END FUNCTION

SUB CalcTrueFPS()
    tmpf = (VideoHeader.VERARate AS FLOAT / 128.0) * 48828.15  ' ByteRate == PCM Rate for 8 bit mono PCM
    IF PCM_IS_16BIT THEN tmpf = tmpf * 2.0
    IF PCM_IS_STEREO THEN tmpf = tmpf * 2.0
    TRUE_FPS = tmpf / VideoHeader.AudioFrameSize AS FLOAT
END SUB

FUNCTION CalculateVERARate() AS UBYTE
        tmpf = VideoHeader.AudioFrameSize AS FLOAT * F_FPS
        IF PCM_IS_16BIT THEN tmpf = tmpf / 2
        IF PCM_IS_STEREO THEN tmpf = tmpf / 2
        RETURN floats.floor((tmpf / 48828.15) * 128) AS UBYTE
END FUNCTION

FUNCTION Seek_VideoFrame_STD(FrameNumber AS FLOAT) AS FLOAT
      IF FrameNumber < 2.0 OR FrameNumber > (NumFrames-1) THEN
        success=FALSE
        RETURN 0
      END IF
      DIM LeadSize AS UWORD = 32
      DIM FrameTotalSize AS UWORD = FrameBufferSize + PalBufferSize
      IF VideoHeader.VTYPE = 6 OR VideoHeader.VTYPE = 8 OR VideoHeader.VTYPE = 7 THEN
         FrameTotalSize += VideoHeader.AudioFrameSize
         ' also include the extra Audio Frame inserted if AudioFrameSize <= 50% of the Audio FIFO
         IF VideoHeader.AudioFrameSize < 2049 THEN LeadSize += VideoHeader.AudioFrameSize
      END IF
      success = TRUE
      RETURN (FrameNumber * FrameTotalSize AS FLOAT) + LeadSize AS FLOAT
END FUNCTION

FUNCTION Get_VidFrameNumber_STD(FilePos AS FLOAT) AS FLOAT
  DIM HeaderSize AS UWORD = 32
  DIM FrameTotalSize AS UWORD = FrameBufferSize + PalBufferSize
   IF VideoHeader.VTYPE = 6 OR VideoHeader.VTYPE = 8 OR VideoHeader.VTYPE = 7 THEN
      FrameTotalSize += VideoHeader.AudioFrameSize
      IF VideoHeader.AudioFrameSize < 2049 THEN HeaderSize += VideoHeader.AudioFrameSize
   END IF
   RETURN floats.floor((FilePos - HeaderSize AS FLOAT)/FrameTotalSize AS FLOAT) - 1.0
END FUNCTION

SUB GetVideoPosition()
    my_f_tell(VideoLFN)
    vidFilePosition = (65536.0 * cx16.r1 AS FLOAT) + cx16.r0 AS FLOAT
END SUB

SUB GetAudioPosition()
    my_f_tell(AudioLFN)
    audFilePosition = (65536.0 * cx16.r1 AS FLOAT) + cx16.r0 AS FLOAT
END SUB

SUB AdjustAudioPosition()
  DIM Divisor AS FLOAT = 1
  IF PCM_IS_16BIT THEN Divisor = Divisor * 2
  IF PCM_IS_STEREO THEN Divisor = Divisor * 2
  IF Divisor > 1 THEN WHILE (floats.floor(audFilePosition/Divisor) * Divisor) <> audFilePosition : audFilePosition = audFilePosition + 1 : WEND
END SUB

FUNCTION hFullScale(HRes AS UWORD) AS UBYTE
  DIM hScale AS FLOAT = floats.floor((HRes AS FLOAT/640.0) * 128.0)
  RETURN hScale AS UBYTE
END FUNCTION

FUNCTION vFullScale(VRes AS UWORD) AS UBYTE
  DIM FDiv AS FLOAT
  IF MAKE_WIDE_SCREEN THEN FDiv = 360.0 ELSE FDiv = 480.0
  DIM vScale AS FLOAT = floats.floor((VRes AS FLOAT/FDiv) * 128.0)
  RETURN vScale AS UBYTE
END FUNCTION

SUB SetFullScreen()
    DIM vScale AS UBYTE = vFullScale(VideoHeader.FrameHeight)
    DIM hScale AS UBYTE = hFullScale(VideoHeader.FrameWidth)
    IF (VideoHeader.FrameWidth AS FLOAT <= (VideoHeader.FrameHeight AS FLOAT/1.5)) AND IS_SPRITE_VIDEO THEN hScale = vScale - 1
    cx16.VERA_DC_HSCALE = hScale
    cx16.VERA_DC_VSCALE = vScale
    IF MAKE_WIDE_SCREEN THEN
       poke $9F25, 2
       poke $9F2B, 30
       poke $9F2C, 210
       poke $9F25, 0
    END IF
END SUB

SUB setPlayerScreen()
    IF VideoHeader.BPP<8 THEN
       cx16.VERA_ADDR_L = $C0
       cx16.VERA_ADDR_M = $FB
       cx16.VERA_ADDR_H = %00010001
       REPEAT 32 : UNROLL 2 : cx16.VERA_DATA0=0 : END UNROLL : END REPEAT
    END IF
    DIM smode AS UBYTE = $80
    SELECT CASE VideoHeader.VTYPE
        CASE 1,2,6,7
            IF VideoHeader.BPP = 8 THEN smode=3
        CASE 3, 8,23,28
            IF VideoHeader.FrameWidth = 640 THEN smode=0
    END SELECT
     cx16.set_screen_mode(smode)
     IF IS_BITMAP_VIDEO AND VideoHeader.FrameWidth = 640 THEN showGraphicsLayer()
     IF IS_SPRITE_VIDEO AND (VideoHeader.BPP = 4 AND FrameBufferSize>16384) THEN hideTextLayer()
     cx16.rambank(STD_ENGINE_BANK)
     ClearAllBuffers()

     ' This little bit of code centers TALL videos.
     IF VideoHeader.FrameWidth AS FLOAT <= (VideoHeader.FrameHeight AS FLOAT/1.5) THEN
       DIM VisPixels AS WORD = floats.round(((vFullScale(VideoHeader.FrameHeight) - 1) AS FLOAT / 128.0) * 640) AS WORD
       XOrg = (VisPixels - VideoHeader.FrameWidth) / 2
     END IF

     IF IS_SPRITE_VIDEO THEN setup_SpriteGrid(XOrg,YOrg)

     IF IS_BITMAP_VIDEO THEN
         SELECT CASE VideoHeader.BPP
            CASE 1
                cx16.VERA_L0_CONFIG = %00000100
            CASE 2
                cx16.VERA_L0_CONFIG = %00000101
            CASE 4
                cx16.VERA_L0_CONFIG = %00000110
         END SELECT
     END IF
     SetFullScreen()
END SUB

FUNCTION read4hex() AS UWORD
    DIM hex AS STRING = "0000"
    FOR cx16.r4L = 0 TO 3
        hex[cx16.r4L] = cbm.CHRIN()
    NEXT
    RETURN conv.hex2uword(hex)
END FUNCTION


SUB my_f_tell(channel AS UBYTE)
        ' gets the (32 bits) position + file size of the opened read file channel
        DIM command[2] AS UBYTE = ["T"c,0]
        command[1] = channel       ' f_open uses this secondary address
        cbm.SETNAM(SIZEOF(command), &command)
        cbm.SETLFS(15, drivenumber, 15)
        VOID cbm.OPEN()
        VOID cbm.CHKIN(15)        ' use #15 as input channel
        success=FALSE
        ' valid response starts with "07," followed by hex notations of the position and filesize
        IF cbm.CHRIN()="0"c AND cbm.CHRIN()="7"c AND cbm.CHRIN()=","c THEN
            cx16.r1 = read4hex()
            cx16.r0 = read4hex()        ' position in R1:R0
            VOID cbm.CHRIN()            ' separator space
            cx16.r3 = read4hex()
            cx16.r2 = read4hex()        ' filesize in R3:R2
            success = TRUE
        END IF
        ASM {{ jmp p8s_Flush15 }}
END SUB

SUB f_seek(channel AS UBYTE, seekpos AS FLOAT)
        ' gets the (32 bits) position + file size of the opened read file channel
        cx16.r2 = floats.floor(seekpos/65536.0) AS UWORD
        cx16.r1 = (seekpos - (cx16.r2 AS FLOAT * 65536.0)) AS UWORD
        cx16.r0 = mkword(channel,"P"c)   ' complete building the P command
        cbm.SETNAM(6, &cx16.r0)
        cbm.SETLFS(15, drivenumber, 15)
        VOID cbm.OPEN()
        VOID cbm.CHKIN(15)        ' use #15 as input channel
        ASM {{
              jmp p8s_Flush15
            }}
END SUB

DIM tmpB AS BOOL
DIM tmpB2 AS BOOL

DIM printchar AS UBYTE
DIM printlen AS UBYTE
DIM ppos AS UBYTE

SUB PrintPlayBar()
    cbm.CLRCHN()
    cbm.CHROUT(printchar)
    ppos++
    IF ppos>printlen THEN
       ppos = 0
       cbm.CHROUT(1)
       txt.column(2)
    END IF
END SUB

SUB PrintAudioPlayScreen()
    txt.color2(7,0)
    MenuBox()
    MyPlot(8,48)
    txt.print("\x05Playing Audio File")
    txt.color2(7,0)
    MyPlot(15,47)
    txt.print(" \x1E[\x05\x80\x18\x80\x19\x1E]\x99 - \x9BVolume")
    MyPlot(17,47)
    txt.print("\x1E[\x05SPC\x1E]\x99 - \x9BPause Play")
    MyPlot(19,47)
    txt.print("\x1E[\x05ESC\x1E]\x99 - \x9BExit Play")
END SUB


SUB SetUpPlayBar(Row AS UBYTE, character AS UBYTE, length AS UBYTE)
    txt.color2(2,11)
    printchar = character
    printlen = length
    MyPlot(Row,2)
    ppos = 0
END SUB


DIM AudioByteRate AS FLOAT
DIM AudioHeaderSize AS FLOAT
SUB CalculateAudioByteRate()
    AudioByteRate = (VideoHeader.VERARate/128.0) * 48828.15
    IF PCM_IS_16BIT THEN AudioByteRate = AudioByteRate * 2
    IF PCM_IS_STEREO THEN AudioByteRate = AudioByteRate * 2
    AudioByteRate = floats.floor(AudioByteRate)
END SUB


SUB PARSE_WAV_HEADER()
    tmpB = WAVHeader.RIFFID[0]=="R"c AND WAVHeader.RIFFID[1]=="I"c AND WAVHeader.RIFFID[2]=="F"c AND WAVHeader.RIFFID[3]=="F"c

    success = WAVHeader.WAVID[0]=="W"c AND WAVHeader.WAVID[1]=="A"c AND WAVHeader.WAVID[2]=="V"c AND WAVHeader.WAVID[3]=="E"c

    success = tmpB AND success

    IF success THEN success = WAVHeader.AUDIO_FORMAT==1 AND (WAVHeader.AUDIO_CHANNELS==1 OR WAVHeader.AUDIO_CHANNELS==2) AND (WAVHeader.SAMPLE_BITS==16)

    IF success THEN success = (WAVHeader.SAMPLE_RATExL < 48829) AND (WAVHeader.SAMPLE_RATExL > 0) AND (WAVHeader.SAMPLE_RATExH==0)

    IF success THEN
       VideoHeader.XAUD_FLAG = "X"c
       VideoHeader.XCFG_BYTE = %00100000
       PCM_IS_16BIT = TRUE
       PCM_IS_STEREO = (WAVHeader.AUDIO_CHANNELS==2)
       IF PCM_IS_STEREO THEN VideoHeader.XCFG_BYTE = %00110000
       VERAcfgvar = VideoHeader.XCFG_BYTE
       VideoHeader.VERARate = ( (WAVHeader.SAMPLE_RATExL AS FLOAT/48828.15) * 128.0 ) AS UBYTE
       AudioHeaderSize = 44
       CalculateAudioByteRate()
       IS_WAV_FILE = success
    END IF
END SUB

FUNCTION VERIFY_ZCM_HEADER() AS BOOL
     ZCMHeader.ZCFG_BYTE = ZCMHeader.ZCFG_BYTE & $F0  ' mask out the volume value.

     ' Sanity checks to ensure this is actually a proper ZCM file. (No way to be 100% on this)
     tmpB = (ZCMHeader.ZCFG_BYTE == 0) OR (ZCMHeader.ZCFG_BYTE == %00100000) OR (ZCMHeader.ZCFG_BYTE == %00110000) OR (ZCMHeader.ZCFG_BYTE == %00010000)
     tmpB = tmpB AND ((ZCMHeader.ZRATE_BYTE < 129) AND (ZCMHeader.ZRATE_BYTE > 0))

    IF tmpB THEN
      VideoHeader.VERARate = ZCMHeader.ZRATE_BYTE
      VideoHeader.XCFG_BYTE = ZCMHeader.ZCFG_BYTE
      VERAcfgvar = ZCMHeader.ZCFG_BYTE
      VideoHeader.XAUD_FLAG = "X"c
       PCM_IS_16BIT = (( VERAcfgvar & %00100000 ) == %00100000 )
      PCM_IS_STEREO = (( VERAcfgvar & %00010000 ) == %00010000 )
      AudioHeaderSize = 8
      CalculateAudioByteRate()
    END IF
    success = tmpB
    RETURN tmpB
END FUNCTION

SUB READ_AUDIO_HEADER(ALFN AS UBYTE, hcount AS UBYTE)
    cbm.CHKIN(ALFN)
    FOR i = 0 TO hcount : AUD_Header[i]=cbm.GETIN2() : NEXT
    cbm.CLRCHN()
    audFilePosition = hcount + 1
    UseBookMark = CheckBookMark()
    f_seek(ALFN, audFilePosition)
END SUB

SUB Read_ZCM_HEADER(ALFN AS UBYTE)
    READ_AUDIO_HEADER(ALFN, 7)
END SUB

SUB Read_WAV_HEADER(ALFN AS UBYTE)
    READ_AUDIO_HEADER(ALFN, 43)
END SUB

SUB OpenAudioFile(AUDEXT AS STRING)
    strings.copy(BASENAME, AUDFILE)
    strings.append(AUDFILE,AUDEXT)
    IF FExists(AUDFILE,strings.length(AUDFILE)) THEN
       fopen(AudioLFN, drivenumber,AudioLFN, AUDFILE)
       my_f_tell(AudioLFN)
       AudFileSize = (65536.0 * f_size_high AS FLOAT) + f_size_low AS FLOAT
       success = TRUE
       RETURN
    ELSE
       ' open failed
       success = FALSE
       RETURN
    END IF
END SUB

FUNCTION CheckBookMark() AS BOOL
    strings.copy(BASENAME, TMPFILE)
    strings.append(TMPFILE,".MARK")
    IF FExists(TMPFILE,strings.length(TMPFILE)) THEN
       fopen(7,drivenumber,7,TMPFILE)
       ASM
               ldx #7
               jsr cbm.CHKIN
               ldx #<p8v_Frame
               ldy #>p8v_Frame
               lda #5
               jsr cx16.MACPTR
       END ASM
       cbm.CLRCHN()
       cbm.CLOSE(7)
       IF IS_PCM_ONLY THEN
          audFilePosition = Frame
          Frame = 0.0
          RETURN TRUE
       END IF
       BFrame = Frame
       tmpf = NumFrames - 10.0
       IF ( BFrame > 5.0 ) AND ( BFrame < tmpf ) THEN
          RETURN TRUE
       ELSE
          Frame = 0.0 : BFrame = 0.0
       END IF
    END IF
   RETURN FALSE
END FUNCTION

SUB getStartSector1()
    cx16.r0 = $B7E1    ' This is valid for r48 only !!!
    FOR i = 0 TO 3
        fsector[i] = cx16.fetch(&cx16.r0,0,i)
    NEXT
    cbm.CLOSE(1)
END SUB


SUB getStartSector2(device AS UBYTE, filename AS STRING)
    fopen(1,device,2,filename)
    IF ROM.VERSION = 48 THEN
        getStartSector1()
        RETURN
    END IF
    File_FL(2)
    fsector[0] = cx16.r0L
    fsector[1] = cx16.r0H
    fsector[2] = cx16.r1L
    fsector[3] = cx16.r1H
    cbm.CLOSE(1)
END SUB

SUB File_FL(channel AS UBYTE)
    ' gets the (32 bits) position + file size of the opened read file channel
    DIM command[3] AS UBYTE = ["F"c,"L"c,0]
    command[2] = channel       ' f_open uses this secondary address
    cbm.SETNAM(SIZEOF(command), &command)
    cbm.SETLFS(15, drivenumber, 15)
    VOID cbm.OPEN()
    VOID cbm.CHKIN(15)        ' use #15 as input channel
    success=FALSE
    ' valid response starts with "07," followed by hex notations of the position and filesize
    IF cbm.CHRIN()="0"c AND cbm.CHRIN()="7"c AND cbm.CHRIN()=","c THEN
        cx16.r1 = read4hex()
        cx16.r0 = read4hex()        ' position in R1:R0
        VOID cbm.CHRIN()            ' separator space
        cx16.r3 = read4hex()
        cx16.r2 = read4hex()        ' filesize in R3:R2
        success = TRUE
    END IF
    ASM {{ jmp p8s_Flush15 }}
END SUB


SUB SetVideoAttributes_SPI()
    cbm.CLOSE(VideoLFN)
    getStartSector2(drivenumber, VIDFILE)
    SET_VID_STARTSECTOR()
    VidFileStartSector = VIDSTART_TOFLOAT()
    IF VideoHeader.VTYPE = 23 THEN
        OpenAudioFile(AEXT)
        VideoHeader.AudioFrameSize = calc_asize(AudFileSize, NumFrames)
        cbm.CLOSE(AudioLFN)
        getStartSector2(drivenumber,AUDFILE)
        SET_AUD_STARTSECTOR()
        AudFileStartSector = AUDSTART_TOFLOAT()
        AUD_FRAME_NUMSECTORS = ((VideoHeader.AudioFrameSize + 256)/512) AS UBYTE
        VideoHeader.AudioFrameSize = (AUD_FRAME_NUMSECTORS AS UWORD * 512)
        VideoHeader.VERARate = CalculateVERARate()
    ELSE
        AUD_FRAME_NUMSECTORS = ((VideoHeader.AudioFrameSize/512) AS UBYTE)
        AudFileStartSector = 0
    END IF
    CalcTrueFPS()
    G_WIDTH = 0
    IF VideoHeader.FrameWidth = 640 THEN G_WIDTH=1
    cx16.rambank(SPI_ENGINE_BANK)
    CalcFrameSize()
    DO_BUFFER_AUDIO = ( AUD_FRAME_NUMSECTORS < 5)
    UseBookMark = CheckBookMark()
    CALC_FILE_STARTUP_SECTORS()
    SETUP_SPI_ENGINECODE()
END SUB

SUB FADE2BLACK()
    OB = peek(0)
    cx16.rambank(STD_ENGINE_BANK)
    REPEAT 15 : sys.wait(1) : PALSHIFT(FALSE) : END REPEAT
    cx16.rambank(OB)
END SUB

SUB FADE2BLUE()
    OB = peek(0)
    cx16.rambank(STD_ENGINE_BANK)
    REPEAT 15 : sys.wait(1) : PALSHIFT(TRUE) : END REPEAT
    cx16.VERA_ADDR_L=0
    cx16.VERA_ADDR_M=$FA
    cx16.VERA_ADDR_H=%00100001
    REPEAT 64 : UNROLL 4 : cx16.VERA_DATA0 = $F : END UNROLL : END REPEAT
    cx16.rambank(OB)
END SUB

SUB SetVideoAttributes_STD()
    UseBookMark = CheckBookMark()
    IF VideoHeader.VTYPE < 4 THEN
       OpenAudioFile(AEXT)
       VideoHeader.AudioFrameSize=calc_asize(AudFileSize, NumFrames)
       VideoHeader.VERARate = CalculateVERARate()
    END IF
    CalcTrueFPS()
    XOrg = 0
    YOrg = 0

    ABuff_255 = (VideoHeader.AudioFrameSize / 255 ) AS UBYTE
    ABuff_Remainder = (VideoHeader.AudioFrameSize - (ABuff_255 AS UWORD * 255)) AS UBYTE

    IF ABuff_Remainder = 0 THEN
        ABuff_255 -= 1
        ABuff_Remainder=255  ' adjust for rare case where AFrame is evenly divisible by 255
    END IF

    CalcFrameSize()
    VBuff_255 = (FrameBufferSize / 255 ) AS UBYTE
    VBuff_Remainder = (FrameBufferSize - (VBuff_255 AS UWORD * 255)) AS UBYTE

    IF IS_SPRITE_VIDEO THEN CalculateSprites()

    DIM APreLoops AS UBYTE = 1 : IF (VideoHeader.AudioFrameSize<2048 AND (NOT UseBookMark)) THEN APreLoops +=1

    IF IS_SUPPORTED_VIDEO THEN
      cx16.rambank(STD_ENGINE_BANK)
      Engine_SetAudioCode(mkword(ABuff_255,APreLoops), mkword(ABuff_255, ABuff_Remainder), ABuff_Remainder)

      IF VideoHeader.VTYPE = 6 OR VideoHeader.VTYPE = 7 OR VideoHeader.VTYPE = 8 THEN SetAudioLFN(VideoLFN)

      IF IS_SPRITE_VIDEO THEN
         IF VideoHeader.BPP = 4 THEN SetSpriteMovie4bit(mkword(VBuff_Remainder,VBuff_255), NumSprites AS UBYTE, SpriteAttr_EndAddress, SpriteBlock2_DataAddress)
         IF VideoHeader.BPP = 8 THEN SetSpriteMovie8bit(mkword(VBuff_Remainder,VBuff_255), NumSprites AS UBYTE, SpriteAttr_EndAddress, SpriteBlock2_DataAddress)
      END IF

       IF VideoHeader.VTYPE = 3 OR VideoHeader.VTYPE = 8 THEN
          DIM WidthVal AS UBYTE = 0 : IF VideoHeader.FrameWidth = 640 THEN WidthVal=1
          SetBitMapMovie(mkword(VBuff_Remainder,VBuff_255), WidthVal, PalBufferSize AS UBYTE)
       END IF

      IF UseBookMark THEN
         f_seek(VideoLFN, Seek_VideoFrame_STD(Frame))
         IF VideoHeader.VTYPE < 4 THEN f_seek(AudioLFN, ((Frame-2) * VideoHeader.AudioFrameSize AS FLOAT) )
      END IF
    END IF
END SUB

SUB SetVideoAttributes()
  FOR i = 0 TO 2 : FRAME_TARGET[i] = vHdrNUMFRAMES[i] : NEXT
  CURFRAME_ZERO()
  IF IS_STD_VIDEO THEN SetVideoAttributes_STD()
  IF IS_SPI_VIDEO THEN SetVideoAttributes_SPI()
  MAKE_WIDE_SCREEN = (VideoHeader.SHint="W"c)
  IF UseBookMark THEN CURFRAMEFLOAT_TOINT()
END SUB

SUB ShowVolume()
    txt.color2(7,0)
    MyPlot(17,47)
    txt.print(" \x1E[\x05\x80\x18\x80\x19\x1E]\x99 - \x9BVolume\x05: \x96")

    SELECT CASE Volume
        CASE 0 : txt.print("00")
        CASE 1 : txt.print("07")
        CASE 2 : txt.print("13")
        CASE 3 : txt.print("20")
        CASE 4 : txt.print("27")
        CASE 5 : txt.print("34")
        CASE 6 : txt.print("40")
        CASE 7 : txt.print("47")
        CASE 8 : txt.print("53")
        CASE 9 : txt.print("60")
        CASE 10 : txt.print("67")
        CASE 11 : txt.print("73")
        CASE 12 : txt.print("80")
        CASE 13 : txt.print("87")
        CASE 14 : txt.print("93")
        CASE 15 : txt.print("100")
    END SELECT
    txt.print("\x05% ")
    txt.color2(1,6)
END SUB

SUB MenuBox()
    i=6
    MyPlot(i,45) : cbm.CHROUT(201) : REPEAT 28 : cbm.CHROUT(205) : END REPEAT : cbm.CHROUT(187)
    REPEAT 13 : i++ : MyPlot(i,45) : cbm.CHROUT(186) : REPEAT 28 : cbm.CHROUT(32) : END REPEAT : cbm.CHROUT(186) : END REPEAT
    i++
    MyPlot(i,45) : cbm.CHROUT(200) : REPEAT 28 : cbm.CHROUT(205) : END REPEAT : cbm.CHROUT(188)
END SUB

DIM CAN_PLAY AS BOOL
SUB MenuBar()
    IF Load_816_SPICode OR Load_816_STDCode THEN
     MyPlot(0,47) : txt.color2(1,0)
     txt.print("\x1E[\x05F12\x1E]\x99 - \x9BDisable 65816 Driver ")
     txt.color2(1,6)
    END IF
    i=5
    CAN_PLAY = IS_SUPPORTED_VIDEO OR IS_PCM_ONLY
    IF CAN_PLAY THEN CAN_PLAY = ((NOT PENV.IS_HOST)) OR (NOT IS_SPI_VIDEO)

    txt.color2(1,0)
    REPEAT 17
      MyPlot(i,43)
      REPEAT 34 : cbm.CHROUT(176) : END REPEAT
      i++
    END REPEAT
    txt.color(8)
    MenuBox()
    MyPlot(7,48)
    txt.print("\x1E[")
    IF CAN_PLAY THEN
        txt.print("\x05")
        GOTO SkipBrightP
    END IF
    txt.print("\x98")
SkipBrightP:
    txt.print("F1\x1E]\x99 - ")
    IF CAN_PLAY THEN
        txt.print("\x9B")
        GOTO SkipBrightP2
    END IF
    txt.print("\x97")
SkipBrightP2:
    txt.print("Play ")
    IF IS_VIDEO THEN txt.print("Movie")
    IF IS_PCM_ONLY THEN txt.print("Audio File")

    IF IS_VIDEO THEN
      MyPlot(9,48)
      txt.print("\x1E[\x05F2\x1E]\x99 - \x9BCreate Stand Alone")
      MyPlot(10,55) : txt.print("Player")
    END IF


    MyPlot(12,48)
    txt.print("\x1E[\x05F3\x1E]\x99 - \x9BSelect Another")
    MyPlot(13,55) : txt.print("Media File")
    MyPlot(15,48)
    txt.print("\x1E[")
    IF UseBookMark THEN
        txt.print("\x05")
        GOTO SkipBright
    END IF
    txt.print("\x98")
SkipBright:
    txt.print("F4\x1E]")
    txt.print("\x99 - ")
    IF UseBookMark THEN
        txt.print("\x9B")
        GOTO SkipBright2
    END IF
    txt.print("\x97")
SkipBright2:
    txt.print("Scratch BookMark")
    MyPlot(19,47)
    txt.print("\x1E[\x05ESC\x1E]\x99 - \x9BExit")
    txt.color2(1,6)
END SUB

SUB ResetAllLFNs()
    FOR i = 0 TO 15 : cbm.CLOSE(i) : NEXT
END SUB

SUB CleanUpMarkFile()
    strings.copy(BASENAME,AUDFILE)
    strings.append(AUDFILE,".MARK")
    IF FExists(AUDFILE,strings.length(AUDFILE)) THEN diskio.delete(AUDFILE)
END SUB

SUB CheckStandAlonePlayer()
    UseStandAlonePlayer = FExists(PRGFILE, strings.length(PRGFILE))
END SUB

SUB PrintFrameRate()
    txt.print("\x0D \x81") : cbm.CHROUT(175) : txt.print("\x05Frames Per Second\x81") : cbm.CHROUT(174)
    txt.print("\x0D\x9B (\x96byte\x9B)\x99") : txt.print(conv.str_ub(VideoHeader.FPS))
    txt.print("  \x9B(\x96encode\x9B)\x99") : floats.print(F_FPS)
    txt.print("  \x9B(\x96play\x9B)\x99") : floats.print(TRUE_FPS) : txt.nl()
END SUB

SUB PrintFrameInfo_STD()
    txt.print("\x0D\x05             Color Depth: \x99") : txt.print(conv.str_ub(VideoHeader.BPP)) : txt.print(" \x9Ebpp\x99")
    txt.print("\x0D\x05      Frame X Resolution: \x9E") : txt.print(conv.str_uw(VideoHeader.FrameWidth))
    txt.print("\x0D\x05      Frame Y Resolution: \x9E") : txt.print(conv.str_uw(VideoHeader.FrameHeight))
    txt.print("\x0D\x05       Frame Buffer Size: \x9E") : txt.print(conv.str_uw(FrameBufferSize))
END SUB

SUB PrintSpriteInfo()
       txt.print("\x0D\x05            SPRITE Width: \x99") : txt.print(conv.str_ub(VideoHeader.SpriteWidth))
       txt.print("\x0D\x05           SPRITE Height: \x99") : txt.print(conv.str_ub(VideoHeader.SpriteHeight))
       txt.print("\x0D\x05       Number of Sprites: \x9E") : txt.print(conv.str_uw(NumSprites))
       txt.print("\x0D\x05    Sprite Attr End Addr:\x99 ") : txt.print(conv.str_uwhex(SpriteAttr_EndAddress))
       txt.print("\x0D\x05 Sprite Block2 Data Addr:\x99 ") : txt.print(conv.str_uwhex(SpriteBlock2_DataAddress))
END SUB

SUB PrintMainVideoInfo()
    txt.print("\x0D\x05 Movie Name\x1E: \x96")
    txt.print(BASENAME) : txt.print("\x0D")
    txt.print("\x9B Type:\x05 ") : txt.print(conv.str_ub(VideoHeader.VTYPE))
    txt.print("     \x9E") : floats.print(NumFrames) : txt.print(" \x9BFrame\x99")
    IF IS_SPRITE_VIDEO THEN txt.print(" Sprite")
    IF VideoHeader.VTYPE = 3 OR VideoHeader.VTYPE = 8 THEN txt.print(" Bitmap")
    IF VideoHeader.VTYPE = 23 OR VideoHeader.VTYPE = 28 THEN txt.print(" HIGH BANDWIDTH BITMAP")
    txt.print("\x9B Based \x05VIDEO  ")
    txt.nl()


    IF UseBookMark THEN
       txt.print("\x05 BookMark at FRAME: \x9E") : floats.print(Frame) : cbm.CHROUT(32)
    END IF

    txt.print("\x9B Play Length\x1E: ") : PrintTime(NumFrames / TRUE_FPS)

     IF UseBookMark THEN
        txt.print("\x9B  Time Left\x1E: ") : PrintTime(NumFrames / TRUE_FPS - (Frame/ TRUE_FPS))
     END IF
     txt.nl()
END SUB

SUB PrintMainAudioInfo()
    txt.print("\x0D\x05 Audio Track Name\x1E: \x96")
    txt.print(BASENAME) : txt.print(FOUND_EXT) : txt.nl()

    txt.print("\x9B             Type\x1E:\x05 ")
      IF IS_Z_FILE THEN txt.print("ZCM")
      IF IS_WAV_FILE THEN txt.print("WAV")
    txt.print(" Audio File")

    play_total = (AudFileSize - AudioHeaderSize) / AudioByteRate
    txt.print("\x0D\x9B      Play Length\x1E: ") : PrintTime(play_total) : txt.nl()

    IF UseBookMark THEN
       tmpf2 = (audFilePosition-AudioHeaderSize) / AudioByteRate
       play_left = play_total - tmpf2
       txt.print("\x0D\x05 BookMark at Time: \x9E") : PrintTime(tmpf2) : txt.nl()
       txt.print("\x05   Play Time Left: \x9E") : PrintTime(play_left) : txt.nl()
    END IF
END SUB

SUB VidMediaSpace() : IF NOT IS_PCM_ONLY THEN REPEAT 7 : cbm.CHROUT(32) : END REPEAT : END SUB

SUB PrintPCMFormat()

      txt.print("\x05       PCM Format: \x99")

      IF PCM_IS_16BIT THEN txt.print("16") ELSE txt.print("8")

      txt.print("\x1E bit \x9E")

      IF PCM_IS_STEREO THEN
         txt.print("stereo")
      ELSE
         txt.print("mono")
      END IF
      txt.nl()
    VidMediaSpace()
    txt.print("\x05        VERA Rate: \x99") : txt.print(conv.str_ub(VideoHeader.VERARate))
    txt.nl()
    VidMediaSpace()
    txt.print("        (\x9B") : floats.print(VERA2Hz()) : txt.print(" \x96hz\x99)")
END SUB


SUB PrintAudioInformation_STD()
 IF IS_PCM_ONLY THEN
    txt.row(10)
    GOTO printAudFormat
 END IF


  IF AUDIO_TRACK_IN_SEPERATEFILE THEN
    txt.print("\x05      Audio File size is: \x9E") : floats.print(AudFileSize) : txt.print("\x96 bytes")
  ELSE
    txt.print("\x05                Audio is: \x96embedded")
  END IF

  txt.print("\x0D\x05        Audio Frame Size: \x9E") : txt.print(conv.str_uw(VideoHeader.AudioFrameSize))


  txt.nl()
printAudFormat:

    VidMediaSpace()
    PrintPCMFormat()

END SUB

SUB PrintMediaAttributes()
    txt.color2(15,6)
    txt.clear_screen()

    IF IS_VIDEO THEN
       PrintMainVideoInfo()
       PrintFrameRate()
       PrintFrameInfo_STD()
       IF IS_SPRITE_VIDEO THEN PrintSpriteInfo()
    END IF
    IF IS_PCM_ONLY THEN
       txt.nl()
       PrintMainAudioInfo()
    END IF

    UNROLL 2 : txt.nl() : END UNROLL

    PrintAudioInformation_STD()

    IF IS_STD_VIDEO THEN
       sprites.hide(2)
       txt.row(22)
       txt.print("\x0D\x0D\x05    Vid Buffer 255 Count: \x1E") : txt.print(conv.str_ub(VBuff_255))
       txt.print("\x05  Remainder: \x1E") : txt.print(conv.str_ub(VBuff_Remainder)) : txt.print("\x96 bytes\x0D")
       txt.print("\x05  Audio Buffer 255 Count: \x1E") : txt.print(conv.str_ub(ABuff_255))
       txt.print("\x05   Remainder: \x1E") : txt.print(conv.str_ub(ABuff_Remainder)) : txt.print("\x96 bytes\x0D")
    END IF

    IF IS_SPI_VIDEO THEN tmpw = &spritelogo1
    IF IS_STD_VIDEO THEN tmpw = &spritelogo2
    IF IS_PCM_ONLY THEN tmpw = &spritelogo3
    LoadHD_Logo(458, 178,tmpw, 2,$31,TRUE)

    IF IS_PCM_ONLY THEN GOTO skipNotSupportedMessage

    IF NOT IS_SUPPORTED_VIDEO THEN
       txt.row(23) : txt.column(1)
       txt.print("\x0D\x0D\x96  UNSUPPORTED FORMAT ")
       CheckStandAlonePlayer()
       IF UseStandAlonePlayer THEN
          txt.print("\x0D\x0D\x05  Stand Alone Player was \x99found\x96!\x0D  \x99[\x05F9\x99]\x1E - \x05Execute \x9BStand Alone Player")
          txt.print("\x0D\x81  This player \x9EWILL NOT\x81 return.")
       END IF
    END IF
skipNotSupportedMessage:
   Print_RUN_Environment()
END SUB

SUB spc(L AS UBYTE @R11) : REPEAT L : cbm.CHROUT(32) : END REPEAT : END SUB

SUB Print_RUN_Environment()
  MyPlot(29,7)
  IF PENV.ON_EMU AND PENV.IS_HOST THEN txt.print("\x1C(\x99HOST \x05File System\x1C)") ELSE txt.print("\x1C(\x99Fat\x1E32 \x05File System\x1C)")

  MyPlot(28,1)
  txt.print(PENV.CPU)
  spc(2) : txt.print ("\x9BOn\x1E: \x05")
  IF PENV.ON_EMU THEN txt.print("X16 Emulator")
  IF PENV.ON_HARDWARE THEN txt.print("X16 Hardware")
  spc(2)
  txt.print("\x9BROM\x1E: \x9Cr\x99") : txt.print(conv.str_ub(ROM.VERSION))
  IF ROM.IsPreRelease THEN txt.print("\x1E(\x9BPre-Release\x1E)")
END SUB

FUNCTION CHECK_IS_DIRECT() AS BOOL
   RETURN ((VideoHeader.VTYPE = 23) OR (VideoHeader.VTYPE = 28))
END FUNCTION

FUNCTION CHECK_IS_STANDARD() AS BOOL
    RETURN ((VideoHeader.VTYPE = 1) OR (VideoHeader.VTYPE = 3) OR (VideoHeader.VTYPE = 6) OR (VideoHeader.VTYPE = 8) OR VideoHeader.VTYPE = 2 OR VideoHeader.VTYPE = 7)
END FUNCTION

FUNCTION CHECK_SEP_AUDIO() AS BOOL
  RETURN ((VideoHeader.VTYPE < 4) OR (VideoHeader.VTYPE = 23))
END FUNCTION

FUNCTION CHECK_SPRITE_VIDEO() AS BOOL
   RETURN ((VideoHeader.VTYPE = 1) OR (VideoHeader.VTYPE = 6) OR (VideoHeader.VTYPE = 2) OR (VideoHeader.VTYPE = 7))
END FUNCTION

FUNCTION CHECK_BITMAP_VIDEO() AS BOOL
   RETURN ((VideoHeader.VTYPE = 3) OR (VideoHeader.VTYPE = 8) OR (VideoHeader.VTYPE = 23) OR (VideoHeader.VTYPE = 28))
END FUNCTION

FUNCTION PARSE_VideoHeader() AS BOOL
   MAKE_ALL_FALSE()
   IF VideoHeader.ID[0] = main.FILEID[0] AND VideoHeader.ID[1] = main.FILEID[1] AND VideoHeader.ID[2] = main.FILEID[2] AND VideoHeader.ID[3] = main.FILEID[3] THEN
      IS_VIDEO = TRUE
      F_FPS = VideoHeader.FPS AS FLOAT
      NumFrames = (65536.0 * VideoHeader.NumFramesHI AS FLOAT) + VideoHeader.NumFramesLO AS FLOAT
      IF VideoHeader.XAUD_FLAG = "X"c THEN
         PCM_IS_16BIT = (( VideoHeader.XCFG_BYTE & %00100000 ) = %00100000 )
         PCM_IS_STEREO = (( VideoHeader.XCFG_BYTE & %00010000 ) = %00010000 )
         IF VideoHeader.X_F_FPS > 0 THEN F_FPS = VideoHeader.X_F_FPS AS FLOAT / 100.0
      END IF
      TRUE_FPS = F_FPS
      VID_HAS_PALETTE = TRUE
      IF ((VideoHeader.VTYPE = 2) OR (VideoHeader.VTYPE = 7)) THEN
         VID_HAS_PALETTE = FALSE
         FIND_PAL_FILE()
      END IF
      IS_SPI_VIDEO = CHECK_IS_DIRECT()
      IS_STD_VIDEO = CHECK_IS_STANDARD()
      IS_SPRITE_VIDEO = CHECK_SPRITE_VIDEO()
      IS_BITMAP_VIDEO = CHECK_BITMAP_VIDEO()
      AUDIO_TRACK_IN_SEPERATEFILE = CHECK_SEP_AUDIO()
      IS_SUPPORTED_VIDEO = (IS_SPI_VIDEO OR IS_STD_VIDEO)
   END IF
   success = IS_SUPPORTED_VIDEO
   RETURN IS_VIDEO
END FUNCTION

SUB ReadIn_VideoHeader()
    cbm.CHKIN(VideoLFN)
    FOR i = 0 TO 31 : vHeader[i] = cbm.GETIN2() : NEXT
    cbm.CLRCHN()
END SUB

SUB OpenVideo()
    fopen(VideoLFN, drivenumber , VideoLFN, VIDFILE)
    ReadIn_VideoHeader()
    IF PARSE_VideoHeader() THEN SetVideoAttributes()
END SUB

SUB GetMediaFile_BASENAME()
    FADE2BLUE()
    VID_FILE_FOUND = FALSE
    IS_PCM_ONLY = FALSE
    PCM_FILE_FOUND = FALSE
    LoadHD_Logo(35,5,&spritelogo2,2,$31,FALSE)
    LoadHD_Logo(145,5,&spritelogo3,4,$41,FALSE)
    Load_FilmReel_Image()
getfilename2:
    txt.clear_screen()
    Print_RUN_Environment()
    MyPlot(24,0)
    txt.print(" \x99[\x05LIST\x99] \x9Bfor a Media Listing \x1E(\x9BSPV\x1C,\x9BZCM\x1C,\x9BWAV\x1E)\x0D")
    txt.print(" \x99[\x05EXIT\x99] \x9Bto leave Player")

    MyPlot(9,1)
      txt.print ("\x9C*\x9A*\x99* \x05X16 Movie & Audio Player \x9E*\x81*\x1C*")
    MyPlot(10,1)
      txt.print ("    \x05Version\x1E: ") : txt.print(PENV.ISOASC_VERSTR)

    MyPlot(13,0)
    txt.print("\x0D \x05Enter Media Name \x05(\x99Without extension\x05)\x1E:\x9E ")
    BASENAME[0]=0
    SetDefaultPalette()
    sprites.show(2) : sprites.show(4)
    txt.input_chars(&BASENAME)
    ISOUpper(BASENAME)
END SUB

FUNCTION Yes(y AS UBYTE @R9, x AS UBYTE @R10) AS BOOL
  DIM InChar AS UBYTE = 0
  DIM StoredChar AS UBYTE = 32

  printit:
     MyPlot(y, x)
     cbm.CHROUT(StoredChar)
  skipprint:
     InChar = cbm.GETIN2()
     IF InChar = 217 OR InChar = 121 THEN InChar = 89  ' force PETSCII or ISO to UpperCase for Y
     IF InChar = 110 OR InChar = 206 THEN InChar = 78  ' ditto for N
     SELECT CASE InChar
         CASE 0
             GOTO skipprint
         CASE 89, 78
             StoredChar = InChar : GOTO printit
         CASE 27
             StoredChar = 78
         CASE 13
             IF NOT (StoredChar = 89 OR StoredChar = 78) THEN
                Boing()
                GOTO skipprint
            END IF
         CASE ELSE
             Boing() : GOTO skipprint
     END SELECT
    RETURN (StoredChar = 89)
END FUNCTION


DIM ScrTimer AS FLOAT
DIM SAVE_DC_VIDEO AS UBYTE

SUB SetScreenTimer()
    ScrTimer = floats.time()
    IF ScrTimer > 16773000 THEN
       ScrTimer = 600.0
    ELSE
       ScrTimer = ScrTimer + 3600
    END IF
END SUB

SUB Print_PCM_FILE_ERROR(msg AS STRING)
    txt.color2(15,0)
    ClearUpperRows()
    MyPlot(1,0) : txt.print("\x9E ") : txt.print(AUDFILE)
    MyPlot(3,0) : txt.print("\x05 ") : txt.print(msg)
    Boing()
    txt.color2(15,6)
    sys.wait(180)
END SUB

SUB DO_PLAYVIDEO_SPI()
    cx16.i2c_write_byte($42,$05, $FF)   ' Turn on the (disc) Activity Light
    PlayVideo_SPI()
    cx16.i2c_write_byte($42,$05, $00)   ' Turn OFF the (disc) Activity Light
END SUB

SUB SETUP_PLAY_PCMSTREAM()
      cx16.rambank(STD_ENGINE_BANK)
      SetAudioLFN(AudioLFN)
      SET_PCM_ONLY()
      IF TapeAnimationAvailable() AND (AudioByteRate < 120000) THEN
         TapeAnimation_INIT()
         SET_FRAME_DRIVER(&NextSpriteFrame)
      ELSE
         IF FExists("ASSETS/CDROM.IMG",16) THEN
            cx16.screen_mode($80,FALSE)
            HideBothLayers()
            LoadVERAImage("ASSETS/CDROM.IMG")
            PrintPlayerControls(27,TRUE)
            txt.row(24) : txt.column(0)
            PrintPCMFormat()
            j=35
            ShowBothLayers()
         ELSE
            PrintAudioPlayScreen()
            j=75
         END IF
         SetUpPlayBar(29, 178, j)
         SET_FRAME_DRIVER(&PrintPlayBar)
         showTextLayer()
      END IF
      SetDefaultPalette()
END SUB

SUB PrepareVideoPlay()
    setPlayerScreen()
    IF IS_STD_VIDEO THEN
      cx16.rambank(STD_ENGINE_BANK)
      VERAcfgvar = 0
      IF VideoHeader.XAUD_FLAG = "X"c THEN VERAcfgvar = (VideoHeader.XCFG_BYTE & %00110000)
      ClearAllBuffers()
      IF (VideoHeader.BPP < 8) THEN
        IF VID_HAS_PALETTE THEN
            SetDefaultPalette()
            BlankPAL_LAST32()
        END IF
      END IF
      IF VID_HAS_PALETTE THEN
          TURN_ON_PALETTE_HANDLING()
      ELSE
          TURN_OFF_PALETTE_HANDLING()
          IF PAL_FILE_FOUND THEN
            IF VideoHeader.BPP = 4 THEN
               LoadVERAPalette(PALFILE, $F0)
            ELSE
               LoadVERAPalette(PALFILE,0)
            END IF
          ELSE
            SetDefaultPalette()
          END IF
      END IF
   END IF

   IF IS_SPI_VIDEO THEN
      cx16.rambank(SPI_ENGINE_BANK)
      CLEARGRAPH()
      hideTextLayer()
      showGraphicsLayer()
   END IF
END SUB

SUB WaitForKey_RestoreColor()
   txt.print(AnyMessage)
   txt.waitkey()
   txt.color2(1,6)
END SUB

SUB HIDESPRITES()
    OB=peek(0) : cx16.rambank(STD_ENGINE_BANK) : HIDEALLSPRITES() : cx16.rambank(OB)
END SUB

SUB HIDESPRITES_AND_FADE2BLUE()
    HIDESPRITES()
    FADE2BLUE()
END SUB

SUB BAD_ROM_MESSAGE()
       txt.print("\x07\x0D\x9B   THIS PROGRAM \x05REQUIRES\x9B ROM VERSION \x99R\x0548 \x9BOR \x05 HIGHER\x96 !")
       txt.print("\x0D\x05   VERSION\x1E: \x99") : txt.print(conv.str_ub(ROM.VERSION)) : txt.print("\x9B WAS FOUND.\x0D")
       IF ROM.IsPreRelease THEN txt.print("\x0D\x05   PRE-RELEASE ROM\x0D")
END SUB

FUNCTION Check816Loaders() AS BOOL, BOOL
    DIM enabled AS BOOL = FExists(ENABLE65816,15)
    tmpB = FExists(SPI816,strings.length(SPI816))
    REPEAT 2 : txt.nl() : END REPEAT
    IF ShowLoadMessages AND tmpB THEN
        txt.print("\x0D\x0D\x99    ")
        txt.print(SPI816)
        txt.print("\x05 FOUND \x1E!\x0D")
    END IF
    tmpB2 = FExists(STD816,strings.length(STD816))
    IF ShowLoadMessages AND tmpB2 THEN
        txt.print("\x0D\x0D\x99    ")
        txt.print(STD816)
        txt.print("\x05 FOUND \x1E!\x0D")
    END IF
    IF (NOT enabled) AND (tmpB OR tmpB2) THEN
       tmpB=FALSE : tmpB2=FALSE
       IF ShowLoadMessages THEN
          txt.print("\x0D\x9965816 \x9BCODE \x05LOADING \x9BIS \x96DISABLED\x1E!\x0D")
          sys.wait(290)
       END IF
    END IF
    RETURN tmpB, tmpB2
END FUNCTION

SUB DO_FILENAME_PROCESSING()
    MAKE_ALL_FALSE()
    strings.copy(BASENAME, VIDFILE)
    strings.append(VIDFILE,VEXT)
    strings.copy(BASENAME, PRGFILE)
    strings.append(PRGFILE,PEXT)
    VID_FILE_FOUND = FExists(VIDFILE,strings.length(VIDFILE))
    IF NOT VID_FILE_FOUND THEN FIND_AUDIO_FILE()
END SUB

SUB LOAD_VIDEO_ENGINE()
    IF Load_816_STDCode THEN
       bload(STD816, STD_ENGINE_BANK, $A000)
       IF ShowLoadMessages THEN
          MyPlot(13,5)
          txt.print(" \x05LOADED \x9965816 \x05STD \x9BENGINE \x05!")
          sys.wait(15)
       END IF
       GOTO skipunzipSTD
    END IF

    cx16.rambank(STD_ENGINE_BANK)
    cx16.memory_decompress(&playenginez, $A000)

skipunzipSTD:
    IF Load_816_SPICode THEN
       bload(SPI816, SPI_ENGINE_BANK, $A000)
       IF ShowLoadMessages THEN
          MyPlot(14,5)
          txt.print(" \x05LOADED \x9965816 \x05SPI \x9BENGINE \x05!")
          IF Load_816_STDCode THEN sys.wait(70) ELSE sys.wait(85)
       END IF
       GOTO skipunzipSPI
    END IF

    cx16.rambank(SPI_ENGINE_BANK)
    cx16.memory_decompress(&playengineSPI, $A000)
skipunzipSPI:
END SUB


DIM choice AS UBYTE

SUB start()
    PENV.GetRunEnvironment()
    IF (ROM.VERSION < 48) OR ((ROM.VERSION=48) AND ROM.IsPreRelease) THEN
        BAD_ROM_MESSAGE()
        GOTO skiplast
    END IF
    IF Volume=0 OR Volume>15 THEN Volume = 12
    ProgramBanner()
    VOID diskio.fastmode(3)
    sys.wait(90)
    ShowLoadMessages = TRUE
    DIM IsReload AS BOOL = FALSE

    Load_816_SPICode=FALSE : Load_816_STDCode=FALSE
    IF (PENV.cpu=816) THEN Load_816_SPICode, Load_816_STDCode=Check816Loaders()
loadlib:
    LOAD_VIDEO_ENGINE()

    IF ShowLoadMessages THEN FADE2BLUE()
    ShowLoadMessages = FALSE
    cx16.set_screen_mode(1)
    txt.cp437()
    SAVE_DC_VIDEO = cx16.VERA_DC_VIDEO
    IF IsReload THEN GOTO LoadTheVideo

getfilename:
    GetMediaFile_BASENAME()

    IF BASENAME="EXIT" THEN GOTO alldone
    IF BASENAME="LIST" THEN
       sprites.reset(2,3)
       ShowDirectory_LIST()
       HIDESPRITES()
       GOTO getfilename
    END IF
ProcessFileName:
    DO_FILENAME_PROCESSING()
    txt.print("\x0D\x05\x0D ")
    IF VID_FILE_FOUND OR PCM_FILE_FOUND THEN
       IF VID_FILE_FOUND THEN
          strings.copy(VIDFILE,TMPFILE)
       ELSE
            IF PCM_FILE_FOUND THEN strings.copy(AUDFILE,TMPFILE)
       END IF
       txt.print(TMPFILE)
       txt.print("\x99 FOUND !\x0D")
       sys.wait(45)
       HIDESPRITES_AND_FADE2BLUE()
LoadTheVideo:
      IF VID_FILE_FOUND THEN OpenVideo()
      IF IS_PCM_ONLY THEN
         OpenAudioFile(FOUND_EXT)
         IF IS_Z_FILE THEN
            Read_ZCM_HEADER(AudioLFN)
            IF VERIFY_ZCM_HEADER() THEN GOTO GetUserChoice
               Print_PCM_FILE_ERROR("\x05Invalid or Unsupported \x99ZCM\x05 file!")
               GOTO getfilename
         END IF
         IF IS_WAV_FILE THEN
            Read_WAV_HEADER(AudioLFN)
            PARSE_WAV_HEADER()
            IF success THEN GOTO GetUserChoice
               Print_PCM_FILE_ERROR("\x05Invalid or Unsupported \x99WAV\x05 file!")
               GOTO getfilename
         END IF
      END IF

GetUserChoice:
      hideTextLayer()
      PrintMediaAttributes()
      IF MAKE_WIDE_SCREEN THEN
         MyPlot(4,44) : txt.print(" \x1E(\x9EWide Screen ")
         IF DISABLE_WIDE_SCREEN THEN
           txt.print("\x9Boff\x1E) ")
         ELSE
           txt.print("\x1E(\x99on\x1E) ")
         END IF
      END IF

      MenuBar()
      ShowVolume()
      SetDefaultPalette()
      showTextLayer()
      cbm.CLRCHN()
      VOID diskio.status()
      MyPlot(27,1)
      diskio.fastmode(3)
      SetScreenTimer()
getchoice:
      choice = cbm.GETIN2()
      tmpf = floats.time()
      IF (tmpf > ScrTimer) AND (tmpf < 16773000) THEN cx16.VERA_DC_VIDEO = 0  ' Blank the Screen
SkipChoice:
      IF choice=0 THEN GOTO getchoice
      ' If a key is pressed reset the Screen blank timer
      cx16.VERA_DC_VIDEO = SAVE_DC_VIDEO
      SetScreenTimer()

      IF choice=133 THEN
         IF CAN_PLAY THEN
            IF IS_SPI_VIDEO AND (ROM.VERSION=49) AND ROM.IsPreRelease THEN
                SPI_WARN()
            END IF
            FADE2BLACK()
            IF IS_PCM_ONLY THEN
                GOTO PlayAudioFile
            ELSE
                GOTO PlayTheVideo
            END IF
         END IF
      END IF

      IF choice=145 THEN
          IF Volume < 15 THEN
            Volume++
            ShowVolume()
            Woop()
          ELSE
              Boing()
              WHILE cbm.GETIN2() <> 0
              WEND
          END IF
          GOTO getchoice
      END IF

      IF choice=17 THEN
          IF Volume > 0 THEN
            Volume--
            ShowVolume()
            Woop()
          ELSE
            Boing()
          END IF
          GOTO getchoice
      END IF

      IF choice=134 OR ((choice=138) AND UseBookMark) THEN
         ResetAllLFNs()
         IF choice=134 THEN HIDESPRITES_AND_FADE2BLUE()
         IF choice=138 THEN
            ClearUpperRows()
            CleanUpMarkFile()
            MyPlot(1,1)
            txt.print("\x99") : txt.print(AUDFILE) : txt.print("\x96 Removed\x05!")
            MyPlot(3,2)
            WaitForKey_RestoreColor()
            HIDESPRITES_AND_FADE2BLUE()
            GOTO LoadTheVideo
         END IF

         BASENAME[0] = 0
         'AUDFILE[0] = 0
         'VIDFILE[0] = 0
         IsReload = FALSE
         GOTO loadlib
      END IF

      IF (choice=23) AND (Load_816_SPICode OR Load_816_STDCode) THEN
         ClearUpperRows()
         MyPlot(1,1) : txt.print("\x05 Loading \x9965C02 \x9BDriver \x1E!")
         Load_816_SPICode=FALSE : Load_816_STDCode=FALSE : ShowLoadMessages=FALSE
         LOAD_VIDEO_ENGINE()
         txt.print("\x05.....\x96Done \x1E!")
         MyPlot(3,2) : txt.print("\x9BRemove Enabling file \x05") : txt.print(ENABLE65816) : txt.print("\x99 Y\x1E/\x99N \x9B--\x05>")
         txt.color2(7,11)
         IF Yes(3, 47) THEN diskio.delete(ENABLE65816)
         txt.color2(1,6) : HIDESPRITES_AND_FADE2BLUE()
         GOTO LoadTheVideo
      END IF

      IF choice=137 AND IS_SUPPORTED_VIDEO THEN
         ResetAllLFNs()
         MakeStandAlonePlayer()
         MyPlot(3,2)
         WaitForKey_RestoreColor()
         GOTO LoadTheVideo
      END IF

      IF choice=27 THEN GOTO alldone

      IF (NOT IS_SUPPORTED_VIDEO) AND (UseStandAlonePlayer) AND (choice=16) THEN
         PrepareRun(PRGFILE)
         sprites.reset(2,1)
         GOTO skiplast
      END IF

      Boing()
      GOTO getchoice

PlayAudioFile:
      SETUP_PLAY_PCMSTREAM()
      GOTO START_ENGINE_PLAY
PlayTheVideo:
      PrepareVideoPlay()
START_ENGINE_PLAY:
      IF IS_STD_VIDEO OR IS_PCM_ONLY THEN
        cx16.rambank(STD_ENGINE_BANK)
        PlayVideo_STD(mkword(VERAcfgvar,VideoHeader.VERARate))
      END IF
      IF IS_SPI_VIDEO THEN DO_PLAYVIDEO_SPI()
      IF MAKE_WIDE_SCREEN THEN RESTORE_LINES()
      IF NOT (MANEXIT=$FF) THEN sys.wait(90)
      IF IS_BITMAP_VIDEO AND (VideoHeader.BPP=8) THEN FADE2BLACK() ELSE pal2black()
      HIDESPRITES()
      hideGraphicsLayer()
      verafx.clear(0,0,0,19200)
      verafx.clear(1,0,0,15359)
      cx16.set_screen_mode(1)
      txt.cp437()
      showTextLayer()
      GOTO skiptoend
    END IF
      txt.print("\x96 NOT FOUND !\x0D\x05")
      Boing() : sys.wait(75)
      HIDESPRITES()
      GOTO getfilename
 skiptoend:
    IF MANEXIT=$FF THEN
       FADE2BLUE()
       cx16.rambank(STD_ENGINE_BANK)
       cx16.vaddr(1,$FA08,0,1)
       cx16.VERA_DATA0 = $EE
       cx16.VERA_DATA0 = $0F
       cx16.VERA_DC_HSCALE=92
       cx16.VERA_DC_VSCALE=80
       MyPlot(2,2)
       txt.color(4) : txt.print(" Saving BookMark !")
       IF IS_STD_VIDEO THEN
         GetVideoPosition()
         Frame=Get_VidFrameNumber_STD(vidFilePosition)
       END IF
       IF IS_PCM_ONLY THEN
        GetAudioPosition()
        AdjustAudioPosition()
        Frame=audFilePosition
       END IF
       IF IS_SPI_VIDEO THEN CURFRAMEINT_TOFLOAT()
       strings.copy("@:", TMPFILE)
       strings.append(TMPFILE,BASENAME)
       strings.append(TMPFILE,".MARK,S,W")
       fopen(7,drivenumber,7,TMPFILE)
       ASM {{
               ldx #7
               jsr cbm.CHKOUT
               ldx #<p8v_Frame
               ldy #>p8v_Frame
               lda #5
               jsr cx16.MCIOUT
            }}
       cbm.CLRCHN()
       cbm.CLOSE(7)
       IF IS_PCM_ONLY THEN Frame=0
    ELSE
          CleanUpMarkFile()
    END IF
      FADE2BLACK()
      Volume = AUDIO_CTRL BITAND $0F
      ResetAllLFNs()
      IsReload = TRUE
      GOTO loadlib
alldone:
   HIDESPRITES_AND_FADE2BLUE()
   cx16.set_screen_mode(1)
   txt.iso_off()
   txt.uppercase()
   txt.color2(1,6)
   txt.cls()
   ProgramBanner()
   SetDefaultPalette()

skipoutputstuff1:
   MyPlot(12,1)
   txt.color2(13,6)
   IF BASENAME="EXIT" THEN GOTO skipoutputstuff2
   txt.print("\x0D ") : txt.print(BASENAME) : txt.color(7)
skipoutputstuff2:
   txt.print(" DONE \x05!\x0D\x0D")
skiplast:
   HIDESPRITES()
   ResetAllLFNs()
END SUB
END MODULE
