%option no_sysinit
%import textio
%import conv
%import diskio
%import strings
%import sprites
%import floats
%import verafx
%encoding iso
%zeropage basicsafe
%zpreserved $22, $60

VideoHeader {
  &ubyte[4]          ID = $500
  &ubyte          VTYPE = $504
  &ubyte            BPP = $505
  &ubyte            FPS = $506
  &ubyte    SpriteWidth = $507
  &ubyte   SpriteHeight = $508
  &uword     FrameWidth = $509
  &uword    FrameHeight = $50B
  &uword    NumFramesLO = $50D
  &uword    NumFramesHI = $50F
  &ubyte          SHint = $511
  &ubyte       VERARate = $512
  &uword AudioFrameSize = $513
  &ubyte      XAUD_FLAG = $515
  &ubyte      XCFG_BYTE = $516
  &uword        X_F_FPS = $517
  &ubyte[7]       Extra = $519
 }

WAVHeader {
   &ubyte[4]            RIFFID = $522
   &uword        WAVDATASIZExL = $526
   &uword        WAVDATASIZExH = $528
   &ubyte[4]             WAVID = $52A
   &ubyte[4]         SUBCHKID1 = $52E
   &uword        SUBCHK1SIZExL = $532
   &uword        SUBCHK1SIZExH = $534
   &uword         AUDIO_FORMAT = $536
   &uword       AUDIO_CHANNELS = $538
   &uword        SAMPLE_RATExL = $53A
   &uword        SAMPLE_RATExH = $53C
   &uword          BYTE_RATExL = $53E
   &uword          BYTE_RATExH = $540
   &uword          BLOCK_ALIGN = $542
   &uword          SAMPLE_BITS = $544
   &ubyte[4]         SUBCHKID2 = $546
   &uword        SUBCHK2SIZExL = $54A
   &uword        SUBCHK2SIZExH = $54C
}

ZCMHeader {
    &uword       lptr = $522   ; $FFFF identify's HUGE ZCM
    &ubyte    banknum = $524   ; for standard ZCM
    &uword  SizeLow32 = $524   ; for HUGE ZCM
    &ubyte  SizeLow24 = $525   ; for standard ZCM
    &uword   SizeHigh = $526   ; Multiply by 256 for Standard ZCM, Multiply by 65536 for HUGE ZCM
    &ubyte  ZCFG_BYTE = $528   ; LOW Nibble of ZCFG_BYTE is requested VOLUME and is masked out/ignored by this code
    &ubyte ZRATE_BYTE = $529   ; This and ZCFG are copied directly to the VERA PCM control registers (sans the Volume nibble)
}

ROM {
      ubyte VERSION
      bool  IsPreRelease
}


PENV {
           str HOST = "?"*5
           str PETSCII_VERSTR="RC 1.0 \x1E(\x9B4TH REV\x1E)"
           str  ISOASC_VERSTR="\x05 RC \x991.0 \x9B(\x054th rev\x9B)"
           str    CPU="\x9B65\x99C\x9B02"
           uword   cpu=6502
           bool IS_HOST
           bool ON_HARDWARE
           bool ON_EMU

           sub FIND_CPU() { if sys.cpu_is_65816() { strings.copy("\x9B65\x99816",CPU) cpu=816} }
           sub IS_HOST_FS()-> bool {
               cbm.SETNAM(11, "$")
               cbm.SETLFS(11, 8, 0)
               cbm.OPEN()
               cbm.CHKIN(11)
               ubyte ctr = 0
               while ctr<2 { if cbm.GETIN2()==34 {ctr++} }
               void cbm.GETIN2()
               for ctr in 0 to 3 {HOST[ctr] = cbm.GETIN2()}
               HOST[4] = 0
               cbm.CLRCHN()
               cbm.CLOSE(11)
               return ((HOST=="HOST") and ON_EMU)
             }

&ubyte EMUIndicator1 = $9FBE
&ubyte EMUIndicator2 = $9FBF

sub GetRunEnvironment() {
         FIND_CPU()
         ROM.VERSION, ROM.IsPreRelease = cx16.rom_version()
         ON_EMU = ( (EMUIndicator1==49) and (EMUIndicator2==54) )
         IS_HOST=IS_HOST_FS()
         ON_HARDWARE = not ON_EMU
     }
}



main  {
      &ubyte[32] vHeader = $500
 &ubyte[3] vHdrNUMFRAMES = $50D
           &ubyte Volume = $520

       &ubyte[8] zHeader = $522
      &ubyte[44] wHeader = $522
   &ubyte[44] AUD_Header = $522

&ubyte AUDIO_CTRL = $9F3B
const uword joystick_get = $FF56

boingsnd:
%asmbinary "dull.raw"
endboing:

spritelogo1:
%asmbinary "XHD.Z"

spritelogo2:
%asmbinary "X16-64.Z"

spritelogo3:
%asmbinary "AUD64.Z"

reelsprites:
%asmbinary "REEL.SPRS.Z"
reelend:

defpal:

playenginez:
 %asmbinary "VID.ENGINE.Z"
endenginez:

playengineSPI:
  %asmbinary "SPI.ENGINE.Z"
endengineSPI:


; STD ENGINE ROUTINES
extsub $A000 = PlayVideo_STD(uword PCMParms @ R0) clobbers(A,X,Y)

; Clears TextBuffer at 1:$B000, GraphBuffer at 0:$0000 and the Sprite Data Buffer at 1:$3000
; (fills with 0)
extsub $A003 = ClearAllBuffers() clobbers(A,X,Y)

extsub $A006 = Engine_SetAudioCode(uword AudParms1 @R0, uword AudParms2 @R1, ubyte AbuffRem @R2 ) clobbers(A)
extsub $A009 = SetSpriteMovie8bit(uword BuffSizes @R3, ubyte SpriteCount @R4, uword EndSprAttrs @R5, uword EndSprData @R6 ) clobbers(A,X)
extsub $A00C = SetSpriteMovie4bit(uword BuffSizes @R3, ubyte SpriteCount @R4, uword EndSprAttrs @R5, uword EndSprData @R6 ) clobbers(A,X)
extsub $A00F = SetBitMapMovie(uword BuffSizes @R3, ubyte WidthByte @R2, ubyte PaletteSize @R4) clobbers(A,X)
extsub $A012 = SET_FRAME_DRIVER(uword DriverAddr @ XY)

; Clears the Graphics Buffer at 0:0000 for 76800 bytes.  (execution time app. 2.7 jiffies)
extsub $A015 = ClearGraphBuffer() clobbers(A,X,Y)
; basically CLS (but always to color 0).  Not sure there's an advantage here but it is also
; called by the Above ClearAllBuffers.
extsub $A018 = ClearTextBuffer() clobbers(A,X,Y)

; Clears the Sprite Buffer at 1:$3000
extsub $A01B = ClearSpriteBuffer() clobbers(A,X,Y)

extsub $A021 = SET_PCM_ONLY() clobbers(A,X)

; Only valid value to pass at the moment is 4 which aligns with the Video LFN for
; Video type 6 & 8 (with embedded Audio).
; eventually both LFN's will be assignable.   For NOW video LFN is 4
; Audio LFN if a seperate file is 6 (same as Vid LFN if embedded)
extsub $A01E = SetAudioLFN(ubyte AudLFN @R0) clobbers(A)

extsub $A024 = HIDEALLSPRITES() clobbers(A,X)

; Decrements all RGB Values in the VERA Palette towards 0 individually by 1.
; If DOBLUE is true then the B value is instead shifted towards $F by 1
extsub $A027 = PALSHIFT(bool DOBLUE @ X) clobbers(A,X,Y)

extsub $A02A = SET_DEFAULT_PALETTE() clobbers(A,X,Y)
extsub $A02D = SETEXTERNAL_CONTROLS() clobbers(X,Y)
extsub $A030 = TURN_OFF_PALETTE_HANDLING() clobbers(A)        ;SYS $A030
extsub $A033 = TURN_ON_PALETTE_HANDLING() clobbers(A)         ;SYS $A033
; END STANDARD ENGINE ROUTINES


; SPI ENGINE ROUTINES
extsub $A000 = PlayVideo_SPI() clobbers(A,X,Y)
extsub $A003 = CLEARALLBUFFERS() clobbers(A,X,Y)
extsub $A006 = CLEARGRAPH() clobbers(A,X,Y)
extsub $A00F = SETVIDREAD_BITMAP() clobbers(A,X,Y)
extsub $A012 = SETVIDREAD_BITMAP_LOW() clobbers(A,X,Y)
extsub $A015 = SETAUDIOREAD_SPI() clobbers(A,X)
extsub $A018 = SET_FRAME_38K() clobbers(A)
extsub $A01B = SET_FRAME_51K() clobbers(A)
extsub $A01E = SET_FRAME_62K() clobbers(A)
extsub $A021 = RESETPCM() clobbers(A,X)
; END SPI ENGINE ROUTINES


; will be set to $FF if Video is exited by the user
; in the Bank where the Video Engine code is located
&ubyte MANEXIT = $BFFF

&ubyte[4] AUD_LEAD_START = $BFA0

&bool DO_BUFFER_AUDIO = $27
&ubyte G_WIDTH = $28

&byte VERA_CFG_BYTE = $23

&ubyte[3] FRAME_CURRENT=$34
&ubyte[3] FRAME_TARGET=$37

&ubyte[4] VID_SECTOR_START = $3A
&uword[2] @nosplit VID_SECTOR_STARTw = $3A

&ubyte[4] VID_SECTOR_CURRENT = $3E
&uword[2] @nosplit VID_SECTOR_CURRENTw = $3E

&ubyte[4] AUD_SECTOR_START = $42
&uword[2] @nosplit AUD_SECTOR_STARTw = $42

&ubyte[4] AUD_SECTOR_CURRENT = $46
&uword[2] @nosplit AUD_SECTOR_CURRENTw = $46

&ubyte AUD_SECTOR_ADDER = $50
&ubyte VID_SECTOR_ADDER = $51

&ubyte AUD_FRAME_NUMSECTORS = $52
&ubyte VID_FRAME_NUMSECTORS = $53

ubyte[4] fsector
bool MAKE_WIDE_SCREEN
bool DISABLE_WIDE_SCREEN = false

alias f_size_low = cx16.r2
alias f_size_high = cx16.r3

ubyte[85] butterfly = [$8F,$93,$9C,$12,$DF,$92,$20,$20,$20,$20,$20,$12,$A9,$0D,$9A,$12,$A5,$DF,$92,$20,$20,$20,$12,$A9,$A7,
                       $92,$0D,$9F,$12,$B5,$20,$DF,$92,$20,$12,$A9,$20,$B6,$0D,$1E,$20,$B7,$12,$BB,$92,$20,$12,$AC,$92,$B7,
                       $0D,$9E,$20,$AF,$12,$BE,$92,$20,$12,$BC,$92,$AF,$0D,$81,$AA,$12,$20,$92,$A9,$20,$DF,$12,$20,$92,$B4,
                       $0D,$1C,$B6,$A9,$20,$20,$20,$DF,$B5,0]

&ubyte STASHVEC = $03B2
&ubyte AUDIO_FIFO = $9F3D
&byte AUDIO_FIFOs = $9F3D
&ubyte VERA_AUDIORATE = $9F3C

bool IS_VIDEO
bool IS_BITMAP_VIDEO
bool IS_SPRITE_VIDEO
bool IS_SUPPORTED_VIDEO
bool VID_HAS_PALETTE
bool IS_STD_VIDEO
bool IS_SPI_VIDEO
bool AUDIO_TRACK_IN_SEPERATEFILE
bool VID_FILE_FOUND
bool IS_Z_FILE
bool IS_WAV_FILE
bool PCM_FILE_FOUND
bool PAL_FILE_FOUND
bool IS_PCM_ONLY

bool Load_816_SPICode
bool Load_816_STDCode

bool UseStandAlonePlayer
bool ShowLoadMessages

bool PCM_IS_STEREO
bool PCM_IS_16BIT

ubyte VERAcfgvar = 0
ubyte drivenumber = 8
ubyte OB

str FILEID = iso:"SPRV"
str BASENAME = "?"*96
str PAD = " "*4
str VIDFILE = "MOVIE"+"?"*96
str PAD3 = " "*4
str AUDFILE = "?"*96
str PALFILE = "?"*96
str PAD4 = " "*4
str PRGFILE = "?"*96
str PAD5 = " "*4
str TMPFILE = "?"*96
str VEXT = iso:".SPV"
str AEXT = iso:".SVA"
str PEXT = iso:".PRG"
str ZEXT = iso:".ZCM"
str WEXT = iso:".WAV"
str FOUND_EXT = ".XXX" + "?"*6
str AnyMessage = iso:"\x9BAny \x05KEY\x9B to continue\x99."
str SPI816 = "ASSETS/SPI.816.BIN"
str STD816 = "ASSETS/STD.816.BIN"
str ENABLE65816 = "ENABLE65816CODE"


ubyte i=0
ubyte j=0
uword tmpw
uword tmpw2

ubyte VideoLFN = 4
ubyte AudioLFN = 6
ubyte STD_ENGINE_BANK = 10
ubyte SPI_ENGINE_BANK = 11
ubyte HIRAMBANK = 30

uword FrameBufferSize
uword PalBufferSize
uword NumSprites
uword SpriteAttr_EndAddress
uword SpriteBlock2_DataAddress
ubyte VBuff_255
ubyte VBuff_Remainder
ubyte ABuff_255
ubyte ABuff_Remainder

bool success = false
bool UseBookMark = false

uword SpriteOrigAddress
uword SpriteBuffSize

word XOrg = 0
word YOrg = 0

alias NumFramesHI = cx16.r1
alias NumFramesLO = cx16.r2

float Frame = 0.0
float tmpf = 0.0
float tmpf2 = 0.0
float play_total
float play_left
float PHours
float PMins
float PSecs
float NumFrames = 0.0
float AudFileSize = 0.0
float vidFilePosition
float audFilePosition
float BFrame = 0.0

float VidFile_SECTOR_Position
float AudFile_SECTOR_Position
float VidFileStartSector
float AudFileStartSector
float Seeker

float F_FPS
float TRUE_FPS


asmsub float2long(uword FPtr @ AY, uword lptr @ R1) {
   %asm {{
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
         }}
}

sub PrintINT32(ubyte[] i32) {
   for i in 3 to 0 step -1 { txt.print(conv.str_ubhex(i32[i])) }
  }

sub RESTORE_LINES() {
    poke($9F25,2)
    poke($9F2B,0)
    poke($9F2C,120)
    poke($9F25,0)
}


sub Boing() {
    VERA_AUDIORATE = 0
    if Volume < 8 { AUDIO_CTRL = 8 } else { AUDIO_CTRL = Volume }
    cx16.memory_copy(&boingsnd,&AUDIO_FIFO, &endboing - &boingsnd)
    VERA_AUDIORATE = 26
    sys.wait(22)
    VERA_AUDIORATE = 0
    %asm {{
kflush: jsr cbm.GETIN
        bne kflush
        }}
}

sub LoadHD_Logo(word lX, word lY,uword ZsprPtr,ubyte sprn,ubyte sOffset,bool ShowIt) {
    cx16.VERA_ADDR_H = %00010001
    cx16.VERA_ADDR_M = sOffset
    cx16.VERA_ADDR_L = $00
    cx16.memory_decompress(ZsprPtr,&cx16.VERA_DATA0)
    spr_init(sprn, 1, mkword(sOffset,0), sprites.SIZE_64, sprites.SIZE_64, 128, 0,ShowIt)
    sprites.pos(sprn, lX,lY)
    if ShowIt { sprites.show(sprn) }
}

sub Load_FilmReel_Image() {
    word x word y
    cx16.VERA_ADDR_H = %00010000
    cx16.VERA_ADDR_M = 0
    cx16.VERA_ADDR_L = 0
    cx16.memory_decompress(&reelsprites,&cx16.VERA_DATA0)
    i = 5
    tmpw = 0
    for y in 143 to 207 step 32 {
     for x in 373 to 565 step 64 {
        spr_init(i,0,tmpw,sprites.SIZE_64, sprites.SIZE_32, 128, 0,false)
        sprites.pos(i,x,y)
        i++
        tmpw = tmpw + 2048
       }
     }
    for i in 5 to 17 { sprites.show(i) }
}

sub bload(str filename,ubyte bank1, uword addr @ R0)  {
    %asm {{
           ldx 2
           ldy 3
           phy
           phx
         }}
    cbm.SETNAM(strings.length(filename), filename)
    %asm {{
           lda #0
           ldx p8v_drivenumber
           ldy #2
           jsr cbm.SETLFS
         }}
    OB = peek(0)
    cx16.rambank(bank1)
    %asm {{
            plx
            ply
            lda #0
            jsr cbm.LOAD
        }}
     cx16.rambank(OB)
}

ubyte[26] banks
uword[26] sImages
ubyte curImageIdx
sub NextSpriteFrame() {
    OB = peek(0)
    if curImageIdx == 24 { curImageIdx=0 } else { curImageIdx = curImageIdx + 2 }
    cx16.rambank(banks[curImageIdx])
    cx16.VERA_ADDR_L = 0
    cx16.VERA_ADDR_M = $32
    cx16.VERA_ADDR_H = %00010001
    cx16.memory_copy(sImages[curImageIdx], &cx16.VERA_DATA0, 2048)
    cx16.rambank(OB)
}

sub InitTapeAnimSprites() {
   sprites.init(10, 1, $3200, sprites.SIZE_32, sprites.SIZE_32, 128, 0)
   sprites.init(11, 1, $3600, sprites.SIZE_32, sprites.SIZE_32, 128, 0)
   sprites.pos(10,92,74)
   sprites.pos(11,196,74)
   sprites.show(10)
   sprites.show(11)
   NextSpriteFrame()
}

sub LoadTapeImage() {
    LoadVERAImage("ASSETS/CASSETTE.IMG")
}

sub LoadAudioCDImage() {
    LoadVERAImage("ASSETS/CDROM.IMG")
}

sub DO_VLOAD() {
    while cbm.READST() == 0 {
      %asm {{
             sec
             lda #0
             ldx #$23
             ldy #$9F
             jsr cx16.MACPTR
           }}
     }
   cbm.CLRCHN()
   cbm.CLOSE(7)
}

sub DO_VOPEN(str VFILE) {
    fopen(7,drivenumber,7,VFILE)
    cbm.CHKIN(7)
}

sub LoadVERAImage(str IMAGEFILE) {
    DO_VOPEN(IMAGEFILE)
    cx16.VERA_ADDR_L = 0
    cx16.VERA_ADDR_M = 0
    cx16.VERA_ADDR_H = %00010000
    DO_VLOAD()
    cx16.memory_fill($9F23,15040,0)
}

sub LoadVERAPalette(str PFILE, ubyte StartIndx) {
   uword VAddr
   VAddr = $FA00 + ((StartIndx as uword) * 2)
   DO_VOPEN(PFILE)
   cx16.VERA_ADDR_L = lsb(VAddr)
   cx16.VERA_ADDR_M = msb(VAddr)
   cx16.VERA_ADDR_H = %00010001
   DO_VLOAD()
}

sub PrintPlayerControls(ubyte StartRow,bool PrintTitle) {
    MyPlot(StartRow,24)
    txt.print(" \x1E[\x05\x80\x18\x80\x19\x1E]\x99-\x9BVolume")
    MyPlot(StartRow+1,0)
    txt.print(" \x1E[\x05SPC\x1E]\x99-\x9BPause Play")
    txt.print("       \x1E[\x05ESC\x1E]\x99-\x9BExit Play")
    if strings.length(AUDFILE) > 36 { strings.slice(AUDFILE,0,36,TMPFILE) } else { strings.copy(AUDFILE,TMPFILE) }
    if PrintTitle { MyPlot(1,3) txt.print(TMPFILE) }
}

sub SetSpritePointers(ubyte startbank) {
  ubyte imgctr
  OB = startbank
  curImageIdx = 0
  tmpw = $A000
  imgctr = 0
  while curImageIdx < 26 {
        banks[curImageIdx] = OB
        banks[curImageIdx+1] = OB
        sImages[curImageIdx] = tmpw
        sImages[curImageIdx+1] = tmpw + 1024
        imgctr = imgctr + 2
        curImageIdx = curImageIdx + 2
        tmpw = tmpw + 2048
        if imgctr == 8 { imgctr=0 tmpw=$A000 OB++ }
     }
   curImageIdx = 24
}

sub TapeAnimation_INIT() {
    bload("ASSETS/TAPESPRITES.BIN", 14, $A000)
    SetSpritePointers(14)
    cx16.screen_mode($80,false)
    HideBothLayers()
    LoadTapeImage()
    InitTapeAnimSprites()
    txt.color2(11,0)
    PrintPlayerControls(28,false)
    txt.color2(1,0)
    MyPlot(25,0)
    PrintPCMFormat()
    cx16.GRAPH_set_colors($10,4,6)
    Graph_CHAR(TMPFILE,57,45)
    SetDefaultPalette()
    ShowBothLayers()
}

sub TapeAnimationAvailable() -> bool {
    return FExists("ASSETS/CASSETTE.IMG",19) and FExists("ASSETS/TAPESPRITES.BIN",22)
}

sub MAKE_ALL_FALSE() {
  success = false
  IS_VIDEO = false
  IS_SUPPORTED_VIDEO = false
  IS_SPI_VIDEO = false
  IS_STD_VIDEO = false
  IS_Z_FILE = false
  IS_WAV_FILE = false
  PCM_IS_16BIT = false
  PCM_IS_STEREO = false
}

sub PrintTime(float seconds) {
    PHours = floats.floor(seconds/3600.0)
    tmpf = seconds - (PHours * 3600.0)
    PMins = floats.floor(tmpf/60.0)
    PSecs = floats.round(tmpf - (PMins*60))

      txt.print("\x05")
      if PHours < 10 {cbm.CHROUT('0')}
      floats.print(PHours)

      txt.print("\x99:\x05")
      if PMins < 10 {cbm.CHROUT('0')}
      floats.print(PMins)

      txt.print("\x99:\x05")
      if PSecs < 10 {cbm.CHROUT('0')}
      floats.print(PSecs)
}

asmsub Flush15() clobbers(A,X,Y) {
  %asm {{
          ldx #15
          jsr cbm.CHKIN
          jmp p8s_FExists.flushchars
       }}
}


sub Woop() {
    VERA_AUDIORATE = 0
    ubyte freint = 100
    byte Amplitude = 30

  repeat 20 {
   repeat 2 {
    repeat freint  {
       AUDIO_FIFOs = Amplitude
     }
    repeat freint {
       AUDIO_FIFOs = (0 - Amplitude)
     }
   }
   freint = freint - 5
   Amplitude = Amplitude + 4
  }
    AUDIO_CTRL = Volume
    VERA_AUDIORATE = 35
    sys.wait(10)
}


asmsub FExists(uword FName @ XY, ubyte NameLength @ A) clobbers(A,X,Y) -> bool @Pc  {
   %asm  {{
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
          }}
}

sub ClearUpperRows() {
   txt.color2(1,0)
   MyPlot(0,0)
   repeat 5 {
       repeat 80 {cbm.CHROUT(32)}
     }
   MyPlot(4,0)
   repeat 80  {cbm.CHROUT(95)}
   MyPlot(0,0)
}

sub Graph_CHAR(str s,uword X,uword Y) {
  j = strings.length(s)
  if j>0 {
     j--
     cx16.GRAPH_put_char(X,Y, $06)
     for i in 0 to j {
       cx16.GRAPH_put_next_char(s[i])
      }
   }
}

sub PrepareRun(str PNAME) {
    sprites.reset(1,126)
    while cbm.GETIN2() != 0 { }
    SetDefaultPalette()
    cx16.set_screen_mode(1)
    txt.color2(6,6)
    txt.clear_screen()
    showTextLayer()
    txt.row(2)
    txt.print("^") txt.print(PNAME)
    cx16.kbdbuf_put(13)
    for i in 0 to 2 { cx16.kbdbuf_put(145) }
    cx16.kbdbuf_put(13)
}


sub VERA2Hz() -> float {
    return (VideoHeader.VERARate as float / 128.0) * 48828.15
}

sub GrabBaseName() {
    cx16.r2 = $BF00
    for cx16.r12L in 0 to 94 {
        i = cx16.fetch(cx16.r2L, 0, cx16.r12L)
        if i==0 { break }
        BASENAME[cx16.r12L] = i
      }
    BASENAME[cx16.r12L+1]=0
}

sub check_A_Ext(str extension, str TARGET) -> bool {
    strings.copy(BASENAME,TARGET)
    strings.append(TARGET,extension)
    tmpB2 = FExists(TARGET, strings.length(TARGET))

    if tmpB2 { strings.copy(extension, FOUND_EXT) } else { FOUND_EXT[0] = 0 }
    return tmpB2
}

sub FIND_AUDIO_FILE()  {
    PCM_FILE_FOUND = false
    IS_Z_FILE = false
    IS_WAV_FILE = false
    PCM_FILE_FOUND = check_A_Ext(ZEXT,AUDFILE)
    if PCM_FILE_FOUND { IS_Z_FILE = true goto DoneAudioSearch }
    PCM_FILE_FOUND = check_A_Ext(WEXT,AUDFILE)
    if PCM_FILE_FOUND { IS_WAV_FILE = true }
DoneAudioSearch:
    IS_PCM_ONLY = PCM_FILE_FOUND
    return
}

sub FIND_PAL_FILE() {
    PAL_FILE_FOUND = check_A_Ext(".PAL",PALFILE)
    return
}


sub directory() -> bool {
        ubyte @zp lastchar = 1
        bool inquote = false

        ; -- Prints the directory contents to the screen. Returns success.

        cbm.SETNAM(11, "$:*=P")
internal_dir:
        cbm.SETLFS(11, drivenumber, 0)
        ubyte status = 1
        void cbm.OPEN()          ; open 11,8,0,"$"
        if_cs
            goto io_error

        %asm {{
                ldx #11
                jsr cbm.CHKIN
             }}

        repeat 4 {
            void cbm.CHRIN()     ; skip the 4 prologue bytes
        }

        ; while not stop key pressed / EOF encountered, read data.
        status = cbm.READST()
        if status!=0 {
            status = 1
            goto io_error
        }
        ubyte LineCount = 0
        while status==0 {
            ubyte low = cbm.CHRIN()
            ubyte high = cbm.CHRIN()
            if LineCount == 27 { txt.print("\x0D\x99...\x96more  \x99[\x9ELeft\x9F-\x9ECtrl\x99]     [\x9EENTER\x99] \x9F- \x05Done")
                          %asm {{
getnextbut:                      lda #0
                                 jsr p8c_joystick_get
                                 and #%00010000
                                 beq p8l_io_error
                                 txa
                                 and #128
                                 bne getnextbut
donewaitlist:                    nop
                               }}
                             txt.print("\x0D")
                                 LineCount=0}
            ubyte @zp character
            i = 0
            repeat {
                character = cbm.CHRIN()
                if character==0
                    break
                if character == 34 { inquote = not inquote }
                if inquote and character != 34 { AUDFILE[i] = character i++ }
            }
            AUDFILE[i] = 0
            ISOUpper(AUDFILE)
            bool IsAud = (strings.compare(&AUDFILE + (i-3) as uword,"ZCM") == 0) or (strings.compare(&AUDFILE + (i-3) as uword,"WAV")==0)
            if strings.compare(&AUDFILE + (i-3) as uword,"SPV") == 0 or IsAud {
               txt.print(" \x9B") txt.print_uw(mkword(high, low)) txt.column(10)
               if IsAud { txt.print("\x99") } else { txt.print("\x05") }
               txt.print(AUDFILE) LineCount++ txt.nl() }

            void cbm.CHRIN()     ; skip 2 bytes
            void cbm.CHRIN()
            status = cbm.READST()
            void cbm.STOP()
breaker:
            if_z
                break
        }
        status = cbm.READST()

io_error:
        %asm {{                                 ; restore default i/o devices
               jsr cbm.CLRCHN
               lda #11
               jsr cbm.CLOSE
             }}
        if status!=0 and status & $40 == 0 {            ; bit 6=end of file
            txt.print("\ni/o error, status: ")
            txt.print_ub(status)
            txt.nl()
            return false
        }
        return true
}

sub ShowDirectory_LIST() {
       txt.clear_screen()
       txt.print("\x0D\x05")
       directory();
       %asm {{
gl:            jsr cbm.GETIN
               bne gl
            }}
       txt.print("\x0D\x0D")
       WaitForKey_RestoreColor()
}

ubyte WarnCount = 0
sub SPI_WARN() {
    if WarnCount > 2 { return }
    WarnCount++
    ClearUpperRows()
    MyPlot(1,1)
    txt.print(" \x9BYou are running a \x96prerelease \x99r49 \x05ROM.")
    MyPlot(2,1)
    txt.print(" \x96This \x99MAY\x96 work \x1E!\x1F....\x9B[\x05Recommend \x99UPGRADE\x9B]\x05")
    MyPlot(3,2)
    WaitForKey_RestoreColor()
}

sub MakeStandAlonePlayer() {
    Seeker = $90 as float
    if IS_SPI_VIDEO { strings.copy("ASSETS/SPI-TEMPLATE.PRG",TMPFILE) } else { strings.copy("ASSETS/STD-TEMPLATE.PRG",TMPFILE) }
    ClearUpperRows()
    MyPlot(1,1)
    if FExists(TMPFILE,strings.length(TMPFILE)) {
       txt.print("\x96 CREATING\x1C: \x9E")
       strings.copy("@:", AUDFILE)
       strings.append(AUDFILE,BASENAME)
       strings.append(AUDFILE,".PRG,S,W")
       txt.print(BASENAME) txt.print(".PRG")
       strings.append(TMPFILE,",S,R")
       fopen(7,drivenumber,7,TMPFILE)
       fopen(6,drivenumber,6,AUDFILE)
       cx16.rambank(HIRAMBANK)
       txt.print(" \x99")
       %asm {{
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
       }}
       cbm.CLRCHN()
       cbm.CLOSE(7)
       f_seek(6, Seeker)
       cbm.CHKOUT(6)
       i = 0
       while BASENAME[i] != 0 { cbm.CHROUT(BASENAME[i])  i+=1 }
       cbm.CHROUT(0)
       cbm.CLRCHN()
       cbm.CLOSE(6)
       txt.print("\x99 Done \x05!")
     } else { txt.print("TEMPLATE PROGRAM NOT FOUND\x0D\x0D") }
}

sub MyPlot(ubyte y, ubyte x) {
    txt.row(y)
    txt.column(x)
}


sub ProgramBanner() {
    i=0
    repeat 85 { cbm.CHROUT(butterfly[i]) i +=1 }
    MyPlot(1,10)
    txt.print ("\x9C*\x9A*\x99* \x05X16 STREAMING MEDIA PLAYER ") txt.print(PENV.PETSCII_VERSTR) txt.print(" \x9E*\x81*\x1C*\x05")
    MyPlot(2,14)
    if PENV.ON_EMU { txt.print("EMULATOR") } else { txt.print("HARDWARE") }
    MyPlot(3,10)
    txt.print ("\x9BAUTHOR\x1E: \x05ANTHONY W. HENRY")
    MyPlot(5,10)
    txt.print("\x9ETHIS SOFTWARE IS UNDER THE MIT LICENSE")
    MyPlot(6,10)
    txt.print("    \x05(\x9BPERMISSIVE\x05)")
}

sub ISOUpper(str s) {
  i=0
  while s[i] != 0 {
      if s[i] > 96 and s[i]<123 { s[i] -= 32 }
      i+=1
    }
}

sub BlankPAL_LAST32() {
    cx16.VERA_ADDR_L = $C0
    cx16.VERA_ADDR_M = $FB
    cx16.VERA_ADDR_H = %00010001
    repeat 16 { unroll 4 { cx16.VERA_DATA0 = 0 } }
}

sub SetDefaultPalette() {
    OB = peek(0)
    cx16.rambank(STD_ENGINE_BANK)
    SET_DEFAULT_PALETTE()
    cx16.rambank(OB)
}

sub pal2black() {
    cx16.VERA_ADDR_L = 0
    cx16.VERA_ADDR_M = $FA
    cx16.VERA_ADDR_H = %00010001
    repeat 128 {
      unroll 4 { cx16.VERA_DATA0 = 0 }
     }
}

sub pal2blue() {
    cx16.VERA_ADDR_L = 0
    cx16.VERA_ADDR_M = $FA
    cx16.VERA_ADDR_H = %00010001
    repeat 128 {
      unroll 2 { cx16.VERA_DATA0=$0F cx16.VERA_DATA0=0 }
     }
}

sub hideGraphicsLayer() {
    cx16.VERA_DC_VIDEO = cx16.VERA_DC_VIDEO & 239
}

sub showGraphicsLayer() {
    cx16.VERA_DC_VIDEO = cx16.VERA_DC_VIDEO | 16
}

sub hideTextLayer() {
    cx16.VERA_DC_VIDEO = cx16.VERA_DC_VIDEO & 223
}

sub showTextLayer() {
    cx16.VERA_DC_VIDEO = cx16.VERA_DC_VIDEO | 32
}

sub HideBothLayers() {
    hideGraphicsLayer()
    hideTextLayer()
}

sub ShowBothLayers() {
    showGraphicsLayer()
    showTextLayer()
}

sub setMovieVideoMode() {
    if IS_SPRITE_VIDEO { return }
    if VideoHeader.FrameWidth==320 {
      cx16.set_screen_mode($80) }
    else {
      cx16.set_screen_mode(0)
    }
 }

sub spr_init(ubyte spritenum,
             ubyte databank, uword dataaddr,
             ubyte width_flag, ubyte height_flag,
             ubyte colors_flag, ubyte palette_offset,bool show) {
        sprites.pos(spritenum, -64, -64)                    ; move sprite off-screen initially
        if not show { cx16.vpoke(1, sprite_reg+6, %00000000) }                     ; z depth %11 = in front of both layers, no flips
        cx16.VERA_DC_VIDEO |= %01000000             ; enable sprites globally
        dataaddr >>= 5
        dataaddr |= (databank as uword)<<11
        uword sprite_reg = sprites.VERA_SPRITEREGS + spritenum*$0008
        cx16.vpoke(1, sprite_reg, lsb(dataaddr))                    ; address 12:5
        cx16.vpoke(1, sprite_reg+1, colors_flag | msb(dataaddr))    ; 4 bpp + address 16:13
        if show { cx16.vpoke(1, sprite_reg+6, %00001100) }          ; z depth %11 = in front of both layers, no flips
        cx16.vpoke(1, sprite_reg+7, height_flag<<6 | width_flag<<4 | palette_offset&15) ; 64x64 pixels, palette offset
    }

sub setup_SpriteGrid(word X @R7, word Y @R9) {
    alias X1 = cx16.r5
    alias Y1 = cx16.r6
    ubyte sprPal1 = 0
    ubyte sprPal2 = 0
    ubyte sprDepth = 128
    ubyte sprWidByte = $FF
    ubyte sprHeiByte = $FF
    ubyte sprNum = 1
    ubyte sprBank = 0
    ubyte sprBank2 = 0
    X1  = X as uword
    Y1  = Y as uword
    word XLimit = (X + VideoHeader.FrameWidth) as word
    uword sprBlock1Addr = SpriteOrigAddress
    uword sprBlock2Addr = SpriteBlock2_DataAddress
    uword BuffAdd = (VideoHeader.SpriteWidth as uword * VideoHeader.SpriteHeight as uword)

    if VideoHeader.BPP==4 {
       sprDepth = 0
       if VID_HAS_PALETTE {
          sprPal1 = 14
          sprPal2 = 15
       } else
         {
          if PAL_FILE_FOUND { sprPal1 = 15 } else { sprPal1 = 1 }
          sprPal2 = sprPal1
         }
       sprBank=1
       BuffAdd=BuffAdd/2
     }

   if VideoHeader.SpriteWidth==64 { sprWidByte=sprites.SIZE_64 }
   if VideoHeader.SpriteWidth==32 { sprWidByte=sprites.SIZE_32 }
   if VideoHeader.SpriteWidth==16 { sprWidByte=sprites.SIZE_16 }
   if VideoHeader.SpriteWidth==8 { sprWidByte= sprites.SIZE_8 }

   if VideoHeader.SpriteHeight==64 { sprHeiByte=sprites.SIZE_64 }
   if VideoHeader.SpriteHeight==32 { sprHeiByte=sprites.SIZE_32 }
   if VideoHeader.SpriteHeight==16 { sprHeiByte=sprites.SIZE_16 }
   if VideoHeader.SpriteHeight==8 { sprHeiByte=sprites.SIZE_8 }

   sprBank2 = sprBank

   repeat NumSprites {
         sprites.init(sprNum, sprBank, sprBlock1Addr, sprWidByte, sprHeiByte, sprDepth, sprPal1)
         sprites.pos(sprNum, X1 as word,Y1 as word)
         sprites.init((sprNum+NumSprites) as ubyte, sprBank2, sprBlock2Addr, sprWidByte, sprHeiByte, sprDepth, sprPal2)
         sprites.pos((sprNum+NumSprites) as ubyte, X1 as word,Y1 as word)
         sprNum = sprNum+1
         X1 = X1 + VideoHeader.SpriteWidth
         if X1>=XLimit { X1=X as uword
                         Y1=(Y1+VideoHeader.SpriteHeight) as word }
         sprBlock1Addr = sprBlock1Addr + BuffAdd
         if sprBlock2Addr >= (65535 - BuffAdd) {
              sprBlock2Addr = 512
              sprBank2 = 1
            }  else {
              sprBlock2Addr = sprBlock2Addr + BuffAdd
            }
     }
}

sub fopen(ubyte lfn, ubyte device, ubyte sa, str filename) {
    cbm.SETLFS(lfn, device, sa)
    cbm.SETNAM(strings.length(filename), filename)
    void cbm.OPEN()
}

sub numFrameSectors() -> ubyte {
    tmpw = FrameBufferSize / 512
    if FrameBufferSize % 512 > 0 { tmpw = tmpw + 1 }
    return tmpw as ubyte
  }

sub FLOAT_TO_INT32(float src, ubyte[] target) {
    tmpw = src/65536.0 as uword
    target[3] = msb(tmpw)
    target[2] = lsb(tmpw)
    tmpw2 = (src - (tmpw as float * 65536.0)) as uword
    target[1] = msb(tmpw2)
    target[0] = lsb(tmpw2)
}

sub INT32_TO_FLOAT(ubyte[] src) -> float {
    tmpw = src[2] + (src[3] as uword * 256)
    return (src[0] as float) + (src[1] as float * 256.0) + (tmpw as float * 65536)
}

sub CURFRAMEINT_TOFLOAT() {
    Frame = (FRAME_CURRENT[0] as float) + (256.0 * FRAME_CURRENT[1] as float) + (65536.0 * FRAME_CURRENT[2] as float)
}

sub CURFRAMEFLOAT_TOINT() {
    tmpf = floats.floor((Frame / 65536.0))
    FRAME_CURRENT[2] = tmpf as ubyte
    tmpw = ( Frame - (tmpf * 65536.0) ) as uword
    FRAME_CURRENT[1] = msb(tmpw)
    FRAME_CURRENT[0] = lsb(tmpw)
}

sub CURFRAME_ZERO() {
    Frame = 0
    FRAME_CURRENT[0] = 0
    FRAME_CURRENT[1] = 0
    FRAME_CURRENT[2] = 0
}

sub SET_VID_STARTSECTOR() {
    for i in 0 to 3 { VID_SECTOR_START[i] = fsector[i] }
}

sub SET_AUD_STARTSECTOR() {
    for i in 0 to 3 { AUD_SECTOR_START[i] = fsector[i] }
}

sub VIDSTART_TOFLOAT() -> float {
    return (VID_SECTOR_STARTw[0] as float) + ((VID_SECTOR_STARTw[1] as float)*65536.0)
}

sub FLOATTO_VIDCURRENT(float FStart) {
    VID_SECTOR_CURRENTw[1] = (FStart/65536.0) as uword
    VID_SECTOR_CURRENTw[0] = ( FStart - (AUD_SECTOR_STARTw[1] as float * 65536.0) ) as uword
}

sub AUDSTART_TOFLOAT() -> float {
    return (AUD_SECTOR_STARTw[0] as float) + ((AUD_SECTOR_STARTw[1] as float)*65536.0)
}

sub FLOATTO_AUDCURRENT(float FStart) {
    AUD_SECTOR_CURRENTw[1] = (FStart/65536.0) as uword
    AUD_SECTOR_CURRENTw[0] = ( FStart - (AUD_SECTOR_STARTw[1] as float * 65536.0) ) as uword
}

sub CALC_FILE_STARTUP_SECTORS() {
    VID_FRAME_NUMSECTORS = numFrameSectors()
    if VideoHeader.VTYPE == 23 {
       VID_SECTOR_ADDER = VID_FRAME_NUMSECTORS + 1
       AUD_SECTOR_ADDER = AUD_FRAME_NUMSECTORS

       VidFile_SECTOR_Position = VidFileStartSector + 1
       AudFile_SECTOR_Position = AudFileStartSector

       if AUD_FRAME_NUMSECTORS < 5 {
          AudFile_SECTOR_Position = AudFile_SECTOR_Position + (AUD_FRAME_NUMSECTORS as float)
          FLOAT_TO_INT32(AudFileStartSector, AUD_LEAD_START)
        }
     }

    if VideoHeader.VTYPE == 28 {
       VID_SECTOR_ADDER = VID_FRAME_NUMSECTORS + AUD_FRAME_NUMSECTORS + 1
       AUD_SECTOR_ADDER = VID_SECTOR_ADDER

       VidFile_SECTOR_Position = (VidFileStartSector + 1.0) + (AUD_FRAME_NUMSECTORS as float)
       AudFile_SECTOR_Position = (VidFileStartSector + 1.0)

       if AUD_FRAME_NUMSECTORS < 5 {
          VidFile_SECTOR_Position = VidFile_SECTOR_Position + (AUD_FRAME_NUMSECTORS as float)
          FLOAT_TO_INT32(AudFile_SECTOR_Position, AUD_LEAD_START)
          AudFile_SECTOR_Position = AudFile_SECTOR_Position + (AUD_FRAME_NUMSECTORS as float)
        }
      }
     if UseBookMark {
         if VideoHeader.BPP < 8 {
          if floats.floor(BFrame / 2) != (BFrame / 2)  {BFrame = BFrame - 1}
         }
        VidFile_SECTOR_Position = VidFile_SECTOR_Position + (BFrame * VID_SECTOR_ADDER as float)
        AudFile_SECTOR_Position = AudFile_SECTOR_Position + (BFrame * AUD_SECTOR_ADDER as float)
        Frame = BFrame
        DO_BUFFER_AUDIO = false
      }
     FLOAT_TO_INT32(VidFile_SECTOR_Position, VID_SECTOR_CURRENT)
     FLOAT_TO_INT32(AudFile_SECTOR_Position, AUD_SECTOR_CURRENT)
}

sub SETUP_SPI_ENGINECODE() {
    cx16.rambank(SPI_ENGINE_BANK)
    SETAUDIOREAD_SPI()
    if FrameBufferSize < 38401.00 {
          SET_FRAME_38K()
    }
    else {
       if FrameBufferSize < 51201.00 {
          SET_FRAME_51K()
       }
       else {
          SET_FRAME_62K()
       }
    }
    if VideoHeader.BPP == 8 { SETVIDREAD_BITMAP() }
    if VideoHeader.BPP < 8 { SETVIDREAD_BITMAP_LOW() }
  }

sub CalcFrameSize() {
    ubyte adjust
    if VideoHeader.BPP==1 { adjust=8
                PalBufferSize = 4}
    if VideoHeader.BPP==2 { adjust=4
                PalBufferSize = 8}
    if VideoHeader.BPP==4 { adjust=2
                PalBufferSize = 32}
    if VideoHeader.BPP==8 { adjust=1
                PalBufferSize = 512}
    if not VID_HAS_PALETTE { PalBufferSize=0 }
    FrameBufferSize = ((VideoHeader.FrameWidth as float * VideoHeader.FrameHeight as float) / adjust as float) as uword
}

sub CalculateSprites()  {
    NumSprites = (VideoHeader.FrameWidth/VideoHeader.SpriteWidth) * (VideoHeader.FrameHeight/VideoHeader.SpriteHeight)
    SpriteOrigAddress = 0
    SpriteBuffSize = VideoHeader.SpriteHeight as uword * VideoHeader.SpriteWidth as uword
    if VideoHeader.BPP==4 { SpriteBuffSize = SpriteBuffSize/2
                SpriteOrigAddress = $3000}
    SpriteAttr_EndAddress = $FC06 + (NumSprites*16)
    SpriteBlock2_DataAddress = SpriteOrigAddress + FrameBufferSize
    if VideoHeader.BPP==8 { SpriteBlock2_DataAddress = SpriteBlock2_DataAddress + 512 }
}

sub calc_asize(float filesize, float numframes) -> uword {
    tmpf = floats.round(filesize / numframes)
    if IS_STD_VIDEO {
        if PCM_IS_16BIT or PCM_IS_STEREO {
           if floats.floor(tmpf/2.0) != (tmpf/2.0) {
              tmpf = tmpf - 1
            }
        }
        if PCM_IS_16BIT and PCM_IS_STEREO {
           while (floats.floor(tmpf/4.0) != (tmpf/4.0)) { tmpf = tmpf + 1 }
        }
     }
  return tmpf as uword
}

sub CalcTrueFPS() {
    tmpf = (VideoHeader.VERARate as float / 128.0) * 48828.15  ; ByteRate == PCM Rate for 8 bit mono PCM
    if PCM_IS_16BIT { tmpf = tmpf * 2.0 }
    if PCM_IS_STEREO { tmpf = tmpf * 2.0 }
    TRUE_FPS = tmpf / VideoHeader.AudioFrameSize as float
}

sub CalculateVERARate() -> ubyte {
        tmpf = VideoHeader.AudioFrameSize as float * F_FPS
        if PCM_IS_16BIT { tmpf = tmpf / 2 }
        if PCM_IS_STEREO { tmpf = tmpf / 2 }
        return floats.floor((tmpf / 48828.15) * 128) as ubyte
}

sub Seek_VideoFrame_STD(float FrameNumber) -> float {
      if FrameNumber < 2.0 or FrameNumber > (NumFrames-1) { success=false return 0 }
      uword LeadSize = 32
      uword FrameTotalSize = FrameBufferSize + PalBufferSize
      if VideoHeader.VTYPE==6 or VideoHeader.VTYPE==8 or VideoHeader.VTYPE==7 {
         FrameTotalSize += VideoHeader.AudioFrameSize
         ; also include the extra Audio Frame inserted if AudioFrameSize <= 50% of the Audio FIFO
         if VideoHeader.AudioFrameSize < 2049 { LeadSize += VideoHeader.AudioFrameSize }
         }
      success = true
      return (FrameNumber * FrameTotalSize as float) + LeadSize as float
}

sub Get_VidFrameNumber_STD(float FilePos) -> float {
  uword HeaderSize = 32
  uword FrameTotalSize = FrameBufferSize + PalBufferSize
   if VideoHeader.VTYPE==6 or VideoHeader.VTYPE==8 or VideoHeader.VTYPE==7 {
      FrameTotalSize += VideoHeader.AudioFrameSize
      if VideoHeader.AudioFrameSize < 2049 { HeaderSize += VideoHeader.AudioFrameSize }
    }
   return floats.floor((FilePos - HeaderSize as float)/FrameTotalSize as float) - 1.0
}

sub GetVideoPosition() {
    my_f_tell(VideoLFN)
    vidFilePosition = (65536.0 * cx16.r1 as float) + cx16.r0 as float
}

sub GetAudioPosition() {
    my_f_tell(AudioLFN)
    audFilePosition = (65536.0 * cx16.r1 as float) + cx16.r0 as float
}

sub AdjustAudioPosition() {
  float Divisor = 1
  if PCM_IS_16BIT { Divisor = Divisor * 2 }
  if PCM_IS_STEREO { Divisor = Divisor * 2 }
  if Divisor > 1 { while (floats.floor(audFilePosition/Divisor) * Divisor) != audFilePosition { audFilePosition = audFilePosition + 1 } }
}

sub hFullScale(uword HRes) -> ubyte {
  float hScale = floats.floor((HRes as float/640.0) * 128.0)
  return hScale as ubyte
}

sub vFullScale(uword VRes) -> ubyte {
  float FDiv
  if MAKE_WIDE_SCREEN { FDiv = 360.0 } else { FDiv = 480.0 }
  float vScale = floats.floor((VRes as float/FDiv) * 128.0)
  return vScale as ubyte
}

sub SetFullScreen() {
    ubyte vScale = vFullScale(VideoHeader.FrameHeight)
    ubyte hScale = hFullScale(VideoHeader.FrameWidth)
    if (VideoHeader.FrameWidth as float <= (VideoHeader.FrameHeight as float/1.5)) and IS_SPRITE_VIDEO { hScale = vScale - 1 }
    cx16.VERA_DC_HSCALE = hScale
    cx16.VERA_DC_VSCALE = vScale
    if MAKE_WIDE_SCREEN {
       poke($9F25,2)
       poke($9F2B,30)
       poke($9F2C,210)
       poke($9F25,0)
    }
}

sub setPlayerScreen() {
    if VideoHeader.BPP<8 {
       cx16.VERA_ADDR_L = $C0
       cx16.VERA_ADDR_M = $FB
       cx16.VERA_ADDR_H = %00010001
       repeat 32 { unroll 2 { cx16.VERA_DATA0=0 } }
     }
    ubyte smode = $80
    when VideoHeader.VTYPE {
           1,2,6,7 -> if VideoHeader.BPP==8 {smode=3}
        3, 8,23,28 -> if VideoHeader.FrameWidth==640 { smode=0 }
     }
     cx16.set_screen_mode(smode)
     if IS_BITMAP_VIDEO and VideoHeader.FrameWidth==640 { showGraphicsLayer() }
     if IS_SPRITE_VIDEO and (VideoHeader.BPP==4 and FrameBufferSize>16384) { hideTextLayer() }
     cx16.rambank(STD_ENGINE_BANK)
     ClearAllBuffers()

     ; This little bit of code centers TALL videos.
     if VideoHeader.FrameWidth as float <= (VideoHeader.FrameHeight as float/1.5) {
       word VisPixels = floats.round(((vFullScale(VideoHeader.FrameHeight) - 1) as float / 128.0) * 640) as word
       XOrg = (VisPixels - VideoHeader.FrameWidth) / 2
     }

     if IS_SPRITE_VIDEO { setup_SpriteGrid(XOrg,YOrg) }

     if IS_BITMAP_VIDEO {
         when VideoHeader.BPP {
            1 -> cx16.VERA_L0_CONFIG = %00000100
            2 -> cx16.VERA_L0_CONFIG = %00000101
            4 -> cx16.VERA_L0_CONFIG = %00000110
          }
       }
     SetFullScreen()
}

sub read4hex() -> uword {
    str hex = "0000"
    for cx16.r4L in 0 to 3 {
        hex[cx16.r4L] = cbm.CHRIN()
    }
    return conv.hex2uword(hex)
}


sub my_f_tell(ubyte channel) {
        ; gets the (32 bits) position + file size of the opened read file channel
        ubyte[2] command = ['T',0]
        command[1] = channel       ; f_open uses this secondary address
        cbm.SETNAM(sizeof(command), &command)
        cbm.SETLFS(15, drivenumber, 15)
        void cbm.OPEN()
        void cbm.CHKIN(15)        ; use #15 as input channel
        success=false
        ; valid response starts with "07," followed by hex notations of the position and filesize
        if cbm.CHRIN()=='0' and cbm.CHRIN()=='7' and cbm.CHRIN()==',' {
            cx16.r1 = read4hex()
            cx16.r0 = read4hex()        ; position in R1:R0
            void cbm.CHRIN()            ; separator space
            cx16.r3 = read4hex()
            cx16.r2 = read4hex()        ; filesize in R3:R2
            success = true
        }
        %asm {{ jmp p8s_Flush15 }}
    }

sub f_seek(ubyte channel, float seekpos) {
        ; gets the (32 bits) position + file size of the opened read file channel
        cx16.r2 = floats.floor(seekpos/65536.0) as uword
        cx16.r1 = (seekpos - (cx16.r2 as float * 65536.0)) as uword
        cx16.r0 = mkword(channel,'P')   ; complete building the P command
        cbm.SETNAM(6, &cx16.r0)
        cbm.SETLFS(15, drivenumber, 15)
        void cbm.OPEN()
        void cbm.CHKIN(15)        ; use #15 as input channel
        %asm {{
              jmp p8s_Flush15
            }}
       }

bool tmpB
bool tmpB2

ubyte printchar
ubyte printlen;
ubyte ppos

sub PrintPlayBar() {
    cbm.CLRCHN()
    cbm.CHROUT(printchar)
    ppos++
    if ppos>printlen {
       ppos = 0
       cbm.CHROUT(1)
       txt.column(2)
     }
  }

sub PrintAudioPlayScreen() {
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
 }


sub SetUpPlayBar(ubyte Row, ubyte character, ubyte length) {
    txt.color2(2,11)
    printchar = character
    printlen = length
    MyPlot(Row,2)
    ppos = 0
  }


float AudioByteRate
float AudioHeaderSize
sub CalculateAudioByteRate() {
    AudioByteRate = (VideoHeader.VERARate/128.0) * 48828.15
    if PCM_IS_16BIT  { AudioByteRate = AudioByteRate * 2 }
    if PCM_IS_STEREO { AudioByteRate = AudioByteRate * 2 }
    AudioByteRate = floats.floor(AudioByteRate)
  }


sub PARSE_WAV_HEADER() {
    tmpB = WAVHeader.RIFFID[0]=='R' and WAVHeader.RIFFID[1]=='I' and WAVHeader.RIFFID[2]=='F' and WAVHeader.RIFFID[3]=='F'

    success = WAVHeader.WAVID[0]=='W' and WAVHeader.WAVID[1]=='A' and WAVHeader.WAVID[2]=='V' and WAVHeader.WAVID[3]=='E'

    success = tmpB and success

    if success { success = WAVHeader.AUDIO_FORMAT==1 and (WAVHeader.AUDIO_CHANNELS==1 or WAVHeader.AUDIO_CHANNELS==2) and (WAVHeader.SAMPLE_BITS==16) }

    if success { success = (WAVHeader.SAMPLE_RATExL < 48829) and (WAVHeader.SAMPLE_RATExL > 0) and (WAVHeader.SAMPLE_RATExH==0) }

    if success {
       VideoHeader.XAUD_FLAG = 'X'
       VideoHeader.XCFG_BYTE = %00100000
       PCM_IS_16BIT = true
       PCM_IS_STEREO = (WAVHeader.AUDIO_CHANNELS==2)
       if PCM_IS_STEREO { VideoHeader.XCFG_BYTE = %00110000 }
       VERAcfgvar = VideoHeader.XCFG_BYTE
       VideoHeader.VERARate = ( (WAVHeader.SAMPLE_RATExL as float/48828.15) * 128.0 ) as ubyte
       AudioHeaderSize = 44
       CalculateAudioByteRate()
       IS_WAV_FILE = success
     }
}

sub VERIFY_ZCM_HEADER() -> bool {
     ZCMHeader.ZCFG_BYTE = ZCMHeader.ZCFG_BYTE & $F0  ; mask out the volume value.

     ; Sanity checks to ensure this is actually a proper ZCM file. (No way to be 100% on this)
     tmpB = (ZCMHeader.ZCFG_BYTE == 0) or (ZCMHeader.ZCFG_BYTE == %00100000) or (ZCMHeader.ZCFG_BYTE == %00110000) or (ZCMHeader.ZCFG_BYTE == %00010000)
     tmpB = tmpB and ((ZCMHeader.ZRATE_BYTE < 129) and (ZCMHeader.ZRATE_BYTE > 0))

    if tmpB {
      VideoHeader.VERARate = ZCMHeader.ZRATE_BYTE
      VideoHeader.XCFG_BYTE = ZCMHeader.ZCFG_BYTE
      VERAcfgvar = ZCMHeader.ZCFG_BYTE
      VideoHeader.XAUD_FLAG = 'X'
       PCM_IS_16BIT = (( VERAcfgvar & %00100000 ) == %00100000 )
      PCM_IS_STEREO = (( VERAcfgvar & %00010000 ) == %00010000 )
      AudioHeaderSize = 8
      CalculateAudioByteRate()
    }
    success = tmpB
    return tmpB
}

sub READ_AUDIO_HEADER(ubyte ALFN, ubyte hcount) {
    cbm.CHKIN(ALFN)
    for i in 0 to hcount { AUD_Header[i]=cbm.GETIN2() }
    cbm.CLRCHN()
    audFilePosition = hcount + 1
    UseBookMark = CheckBookMark()
    f_seek(ALFN, audFilePosition)
}

sub Read_ZCM_HEADER(ubyte ALFN)  {
    READ_AUDIO_HEADER(ALFN, 7)
  }

sub Read_WAV_HEADER(ubyte ALFN) {
    READ_AUDIO_HEADER(ALFN, 43)
  }

sub OpenAudioFile(str AUDEXT) {
    strings.copy(BASENAME, AUDFILE)
    strings.append(AUDFILE,AUDEXT)
    if FExists(AUDFILE,strings.length(AUDFILE))
     {
       fopen(AudioLFN, drivenumber,AudioLFN, AUDFILE)
       my_f_tell(AudioLFN)
       AudFileSize = (65536.0 * f_size_high as float) + f_size_low as float
       success = true
       return }
         else {
            ; open failed
            success = false
            return }
    }

sub CheckBookMark() -> bool {
    strings.copy(BASENAME, TMPFILE)
    strings.append(TMPFILE,".MARK")
    if FExists(TMPFILE,strings.length(TMPFILE)) {
       fopen(7,drivenumber,7,TMPFILE)
       %asm {{
               ldx #7
               jsr cbm.CHKIN
               ldx #<p8v_Frame
               ldy #>p8v_Frame
               lda #5
               jsr cx16.MACPTR
            }}
       cbm.CLRCHN()
       cbm.CLOSE(7)
          if IS_PCM_ONLY {
          audFilePosition = Frame
          Frame = 0.0
          return true
        }
       BFrame = Frame
       tmpf = NumFrames - 10.0
       if ( BFrame > 5.0 ) and ( BFrame < tmpf )
          { return true }
       else { Frame = 0.0   BFrame = 0.0 }
     }
   return false
}

sub getStartSector1() {
    cx16.r0 = $B7E1    ; This is valid for r48 only !!!
    for i in 0 to 3 {
        fsector[i] = cx16.fetch(&cx16.r0,0,i)
    }
    cbm.CLOSE(1)
 }


 sub getStartSector2(ubyte device, str filename) {
        fopen(1,device,2,filename)
        if ROM.VERSION==48 { getStartSector1() return }
        File_FL(2)
        fsector[0] = cx16.r0L
        fsector[1] = cx16.r0H
        fsector[2] = cx16.r1L
        fsector[3] = cx16.r1H
        cbm.CLOSE(1)
}

 sub File_FL(ubyte channel) {
        ; gets the (32 bits) position + file size of the opened read file channel
        ubyte[3] command = ['F','L',0]
        command[2] = channel       ; f_open uses this secondary address
        cbm.SETNAM(sizeof(command), &command)
        cbm.SETLFS(15, drivenumber, 15)
        void cbm.OPEN()
        void cbm.CHKIN(15)        ; use #15 as input channel
        success=false
        ; valid response starts with "07," followed by hex notations of the position and filesize
        if cbm.CHRIN()=='0' and cbm.CHRIN()=='7' and cbm.CHRIN()==',' {
            cx16.r1 = read4hex()
            cx16.r0 = read4hex()        ; position in R1:R0
            void cbm.CHRIN()            ; separator space
            cx16.r3 = read4hex()
            cx16.r2 = read4hex()        ; filesize in R3:R2
            success = true
        }
        %asm {{ jmp p8s_Flush15 }}
}


sub SetVideoAttributes_SPI() {
    cbm.CLOSE(VideoLFN)
    getStartSector2(drivenumber, VIDFILE)
    SET_VID_STARTSECTOR()
    VidFileStartSector = VIDSTART_TOFLOAT()
    if VideoHeader.VTYPE==23 {
        OpenAudioFile(AEXT)
        VideoHeader.AudioFrameSize = calc_asize(AudFileSize, NumFrames)
        cbm.CLOSE(AudioLFN)
        getStartSector2(drivenumber,AUDFILE)
        SET_AUD_STARTSECTOR()
        AudFileStartSector = AUDSTART_TOFLOAT()
        AUD_FRAME_NUMSECTORS = ((VideoHeader.AudioFrameSize + 256)/512) as ubyte
        VideoHeader.AudioFrameSize = (AUD_FRAME_NUMSECTORS as uword * 512)
        VideoHeader.VERARate = CalculateVERARate()
      }
    else
      {
        AUD_FRAME_NUMSECTORS = ((VideoHeader.AudioFrameSize/512) as ubyte)
        AudFileStartSector = 0
      }
    CalcTrueFPS()
    G_WIDTH = 0
    if VideoHeader.FrameWidth == 640 { G_WIDTH=1 }
    cx16.rambank(SPI_ENGINE_BANK)
    CalcFrameSize()
    DO_BUFFER_AUDIO = ( AUD_FRAME_NUMSECTORS < 5)
    UseBookMark = CheckBookMark()
    CALC_FILE_STARTUP_SECTORS()
    SETUP_SPI_ENGINECODE()
}

sub FADE2BLACK() {
    OB = peek(0)
    cx16.rambank(STD_ENGINE_BANK)
    repeat 15 { sys.wait(1) PALSHIFT(false) }
    cx16.rambank(OB)
}

sub FADE2BLUE() {
    OB = peek(0)
    cx16.rambank(STD_ENGINE_BANK)
    repeat 15 { sys.wait(1) PALSHIFT(true) }
    cx16.VERA_ADDR_L=0
    cx16.VERA_ADDR_M=$FA
    cx16.VERA_ADDR_H=%00100001
    repeat 64 { unroll 4 { cx16.VERA_DATA0 = $F } }
    cx16.rambank(OB)
}

sub SetVideoAttributes_STD() {
    UseBookMark = CheckBookMark()
    if VideoHeader.VTYPE < 4 {
       OpenAudioFile(AEXT)
       VideoHeader.AudioFrameSize=calc_asize(AudFileSize, NumFrames)
       VideoHeader.VERARate = CalculateVERARate()
    }
    CalcTrueFPS()
    XOrg = 0
    YOrg = 0

    ABuff_255 = (VideoHeader.AudioFrameSize / 255 ) as ubyte
    ABuff_Remainder = (VideoHeader.AudioFrameSize - (ABuff_255 as uword * 255)) as ubyte

    if ABuff_Remainder==0 { ABuff_255 -= 1 ABuff_Remainder=255 }  ; adjust for rare case where AFrame is evenly divisible by 255

    CalcFrameSize()
    VBuff_255 = (FrameBufferSize / 255 ) as ubyte
    VBuff_Remainder = (FrameBufferSize - (VBuff_255 as uword * 255)) as ubyte

    if IS_SPRITE_VIDEO { CalculateSprites() }

    ubyte APreLoops = 1 if (VideoHeader.AudioFrameSize<2048 and  (not UseBookMark)) { APreLoops +=1 }

    if IS_SUPPORTED_VIDEO {
      cx16.rambank(STD_ENGINE_BANK)
      Engine_SetAudioCode(mkword(ABuff_255,APreLoops), mkword(ABuff_255, ABuff_Remainder), ABuff_Remainder)

      if VideoHeader.VTYPE==6 or VideoHeader.VTYPE==7 or VideoHeader.VTYPE==8 { SetAudioLFN(VideoLFN) }

      if IS_SPRITE_VIDEO {
         if VideoHeader.BPP==4 { SetSpriteMovie4bit(mkword(VBuff_Remainder,VBuff_255), NumSprites as ubyte, SpriteAttr_EndAddress, SpriteBlock2_DataAddress) }
         if VideoHeader.BPP==8 { SetSpriteMovie8bit(mkword(VBuff_Remainder,VBuff_255), NumSprites as ubyte, SpriteAttr_EndAddress, SpriteBlock2_DataAddress) }
      }

       if VideoHeader.VTYPE==3 or VideoHeader.VTYPE==8 {
          ubyte WidthVal = 0 if VideoHeader.FrameWidth==640 { WidthVal=1 }
          SetBitMapMovie(mkword(VBuff_Remainder,VBuff_255), WidthVal, PalBufferSize as ubyte)
       }

      if UseBookMark {
         f_seek(VideoLFN, Seek_VideoFrame_STD(Frame))
         if VideoHeader.VTYPE < 4 { f_seek(AudioLFN, ((Frame-2) * VideoHeader.AudioFrameSize as float) )  }
      }
    }
}

sub SetVideoAttributes() {
  for i in 0 to 2 { FRAME_TARGET[i] = vHdrNUMFRAMES[i] }
  CURFRAME_ZERO()
  if IS_STD_VIDEO { SetVideoAttributes_STD() }
  if IS_SPI_VIDEO { SetVideoAttributes_SPI() }
  MAKE_WIDE_SCREEN = (VideoHeader.SHint=='W')
  if UseBookMark { CURFRAMEFLOAT_TOINT() }
}

sub ShowVolume() {
    txt.color2(7,0)
    MyPlot(17,47)
    txt.print(" \x1E[\x05\x80\x18\x80\x19\x1E]\x99 - \x9BVolume\x05: \x96")

    when Volume {
                  0 -> txt.print("00")
                  1 -> txt.print("07")
                  2 -> txt.print("13")
                  3 -> txt.print("20")
                  4 -> txt.print("27")
                  5 -> txt.print("34")
                  6 -> txt.print("40")
                  7 -> txt.print("47")
                  8 -> txt.print("53")
                  9 -> txt.print("60")
                 10 -> txt.print("67")
                 11 -> txt.print("73")
                 12 -> txt.print("80")
                 13 -> txt.print("87")
                 14 -> txt.print("93")
                 15 -> txt.print("100")
                }
    txt.print("\x05% ")
    txt.color2(1,6)
}

sub MenuBox() {
    i=6
    MyPlot(i,45) cbm.CHROUT(201) repeat 28 { cbm.CHROUT(205) } cbm.CHROUT(187)
    repeat 13 { i++ MyPlot(i,45) cbm.CHROUT(186) repeat 28 { cbm.CHROUT(32) } cbm.CHROUT(186) }
    i++
    MyPlot(i,45) cbm.CHROUT(200) repeat 28 { cbm.CHROUT(205) } cbm.CHROUT(188)
}

bool CAN_PLAY
sub MenuBar() {
    if Load_816_SPICode or Load_816_STDCode {
     MyPlot(0,47) txt.color2(1,0)
     txt.print("\x1E[\x05F12\x1E]\x99 - \x9BDisable 65816 Driver ")
     txt.color2(1,6)
    }
    i=5
    CAN_PLAY = IS_SUPPORTED_VIDEO or IS_PCM_ONLY
    if CAN_PLAY { CAN_PLAY = ((not PENV.IS_HOST)) or (not IS_SPI_VIDEO) }

    txt.color2(1,0)
    repeat 17 {
      MyPlot(i,43)
      repeat 34 { cbm.CHROUT(176)  }
      i++
    }
    txt.color(8)
    MenuBox()
    MyPlot(7,48)
    txt.print("\x1E[")
    if CAN_PLAY { txt.print("\x05") goto SkipBrightP}
    txt.print("\x98")
SkipBrightP:
    txt.print("F1\x1E]\x99 - ")
    if CAN_PLAY { txt.print("\x9B") goto SkipBrightP2 }
    txt.print("\x97")
SkipBrightP2:
    txt.print("Play ")
    if IS_VIDEO { txt.print("Movie") }
    if IS_PCM_ONLY { txt.print("Audio File") }

    if IS_VIDEO {
      MyPlot(9,48)
      txt.print("\x1E[\x05F2\x1E]\x99 - \x9BCreate Stand Alone")
      MyPlot(10,55) txt.print("Player")
     }


    MyPlot(12,48)
    txt.print("\x1E[\x05F3\x1E]\x99 - \x9BSelect Another")
    MyPlot(13,55) txt.print("Media File")
    MyPlot(15,48)
    txt.print("\x1E[")
    if UseBookMark { txt.print("\x05") goto SkipBright}
    txt.print("\x98")
SkipBright:
    txt.print("F4\x1E]")
    txt.print("\x99 - ")
    if UseBookMark { txt.print("\x9B") goto SkipBright2 }
    txt.print("\x97")
SkipBright2:
    txt.print("Scratch BookMark")
    MyPlot(19,47)
    txt.print("\x1E[\x05ESC\x1E]\x99 - \x9BExit")
    txt.color2(1,6)
}

sub ResetAllLFNs() {
    for i in 0 to 15 { cbm.CLOSE(i) }
}

sub CleanUpMarkFile() {
    strings.copy(BASENAME,AUDFILE)
    strings.append(AUDFILE,".MARK")
    if FExists(AUDFILE,strings.length(AUDFILE)) { diskio.delete(AUDFILE) }
}

sub CheckStandAlonePlayer() {
    UseStandAlonePlayer = FExists(PRGFILE, strings.length(PRGFILE))
}

sub PrintFrameRate() {
    txt.print("\x0D \x81") cbm.CHROUT(175) txt.print("\x05Frames Per Second\x81") cbm.CHROUT(174)
    txt.print("\x0D\x9B (\x96byte\x9B)\x99") txt.print(conv.str_ub(VideoHeader.FPS))
    txt.print("  \x9B(\x96encode\x9B)\x99") floats.print(F_FPS)
    txt.print("  \x9B(\x96play\x9B)\x99") floats.print(TRUE_FPS) txt.nl()
}

sub PrintFrameInfo_STD() {
    txt.print("\x0D\x05             Color Depth: \x99") txt.print(conv.str_ub(VideoHeader.BPP)) txt.print(" \x9Ebpp\x99")
    txt.print("\x0D\x05      Frame X Resolution: \x9E") txt.print(conv.str_uw(VideoHeader.FrameWidth))
    txt.print("\x0D\x05      Frame Y Resolution: \x9E") txt.print(conv.str_uw(VideoHeader.FrameHeight))
    txt.print("\x0D\x05       Frame Buffer Size: \x9E") txt.print(conv.str_uw(FrameBufferSize))
}

sub PrintSpriteInfo() {
       txt.print("\x0D\x05            SPRITE Width: \x99") txt.print(conv.str_ub(VideoHeader.SpriteWidth))
       txt.print("\x0D\x05           SPRITE Height: \x99") txt.print(conv.str_ub(VideoHeader.SpriteHeight))
       txt.print("\x0D\x05       Number of Sprites: \x9E") txt.print(conv.str_uw(NumSprites))
       txt.print("\x0D\x05    Sprite Attr End Addr:\x99 ") txt.print(conv.str_uwhex(SpriteAttr_EndAddress))
       txt.print("\x0D\x05 Sprite Block2 Data Addr:\x99 ") txt.print(conv.str_uwhex(SpriteBlock2_DataAddress))
}

sub PrintMainVideoInfo() {
    txt.print("\x0D\x05 Movie Name\x1E: \x96")
    txt.print(BASENAME) txt.print("\x0D")
    txt.print("\x9B Type:\x05 ") txt.print(conv.str_ub(VideoHeader.VTYPE))
    txt.print("     \x9E") floats.print(NumFrames) txt.print(" \x9BFrame\x99")
    if IS_SPRITE_VIDEO { txt.print(" Sprite") }
    if VideoHeader.VTYPE==3 or VideoHeader.VTYPE==8 { txt.print(" Bitmap") }
    if VideoHeader.VTYPE==23 or VideoHeader.VTYPE==28 { txt.print(" HIGH BANDWIDTH BITMAP") }
    txt.print("\x9B Based \x05VIDEO  ")
    txt.nl()


    if UseBookMark {
       txt.print("\x05 BookMark at FRAME: \x9E") floats.print(Frame) cbm.CHROUT(32)
    }

    txt.print("\x9B Play Length\x1E: ") PrintTime(NumFrames / TRUE_FPS)

     if UseBookMark {
        txt.print("\x9B  Time Left\x1E: ") PrintTime(NumFrames / TRUE_FPS - (Frame/ TRUE_FPS))
     }
     txt.nl()
}

sub PrintMainAudioInfo() {
    txt.print("\x0D\x05 Audio Track Name\x1E: \x96")
    txt.print(BASENAME) txt.print(FOUND_EXT) txt.nl()

    txt.print("\x9B             Type\x1E:\x05 ")
      if IS_Z_FILE { txt.print("ZCM") }
      if IS_WAV_FILE { txt.print("WAV") }
    txt.print(" Audio File")

    play_total = (AudFileSize - AudioHeaderSize) / AudioByteRate
    txt.print("\x0D\x9B      Play Length\x1E: ") PrintTime(play_total) txt.nl()

    if UseBookMark {
       tmpf2 = (audFilePosition-AudioHeaderSize) / AudioByteRate
       play_left = play_total - tmpf2
       txt.print("\x0D\x05 BookMark at Time: \x9E") PrintTime(tmpf2) txt.nl()
       txt.print("\x05   Play Time Left: \x9E") PrintTime(play_left) txt.nl()
    }
}

sub VidMediaSpace() { if not IS_PCM_ONLY { repeat 7 { cbm.CHROUT(32) } } }

sub PrintPCMFormat() {

      txt.print("\x05       PCM Format: \x99")

      if PCM_IS_16BIT { txt.print("16") } else { txt.print("8") }

      txt.print("\x1E bit \x9E")

      if PCM_IS_STEREO
         { txt.print("stereo") }
      else
         { txt.print("mono") }
      txt.nl()
    VidMediaSpace()
    txt.print("\x05        VERA Rate: \x99") txt.print(conv.str_ub(VideoHeader.VERARate))
    txt.nl()
    VidMediaSpace()
    txt.print("        (\x9B") floats.print(VERA2Hz()) txt.print(" \x96hz\x99)")
  }


sub PrintAudioInformation_STD() {
 if IS_PCM_ONLY {    txt.row(10)
                     goto printAudFormat }


  if AUDIO_TRACK_IN_SEPERATEFILE
    { txt.print("\x05      Audio File size is: \x9E") floats.print(AudFileSize) txt.print("\x96 bytes") }
  else
    { txt.print("\x05                Audio is: \x96embedded") }

  txt.print("\x0D\x05        Audio Frame Size: \x9E") txt.print(conv.str_uw(VideoHeader.AudioFrameSize))


  txt.nl()
printAudFormat:

    VidMediaSpace()
    PrintPCMFormat()

}

sub PrintMediaAttributes() {
    txt.color2(15,6)
    txt.clear_screen()

    if IS_VIDEO {
       PrintMainVideoInfo()
       PrintFrameRate()
       PrintFrameInfo_STD()
       if IS_SPRITE_VIDEO { PrintSpriteInfo() }
     }
    if IS_PCM_ONLY {
       txt.nl()
       PrintMainAudioInfo()
     }

    unroll 2 { txt.nl() }

    PrintAudioInformation_STD()

    if IS_STD_VIDEO {
       sprites.hide(2)
       txt.row(22)
       txt.print("\x0D\x0D\x05    Vid Buffer 255 Count: \x1E") txt.print(conv.str_ub(VBuff_255))
       txt.print("\x05  Remainder: \x1E") txt.print(conv.str_ub(VBuff_Remainder)) txt.print("\x96 bytes\x0D")
       txt.print("\x05  Audio Buffer 255 Count: \x1E") txt.print(conv.str_ub(ABuff_255))
       txt.print("\x05   Remainder: \x1E") txt.print(conv.str_ub(ABuff_Remainder)) txt.print("\x96 bytes\x0D")
     }

    if IS_SPI_VIDEO { tmpw = &spritelogo1 }
    if IS_STD_VIDEO { tmpw = &spritelogo2 }
    if IS_PCM_ONLY { tmpw = &spritelogo3 }
    LoadHD_Logo(458, 178,tmpw, 2,$31,true)

    if IS_PCM_ONLY { goto skipNotSupportedMessage }

    if not IS_SUPPORTED_VIDEO {
       txt.row(23) txt.column(1)
       txt.print("\x0D\x0D\x96  UNSUPPORTED FORMAT ")
       CheckStandAlonePlayer()
       if UseStandAlonePlayer {
          txt.print("\x0D\x0D\x05  Stand Alone Player was \x99found\x96!\x0D  \x99[\x05F9\x99]\x1E - \x05Execute \x9BStand Alone Player")
          txt.print("\x0D\x81  This player \x9EWILL NOT\x81 return.")
          }
     }
skipNotSupportedMessage:
   Print_RUN_Environment()
}

sub spc(ubyte L @ R11 ) { repeat L { cbm.CHROUT(32) } }

sub Print_RUN_Environment() {
  MyPlot(29,7)
  if PENV.ON_EMU and PENV.IS_HOST { txt.print("\x1C(\x99HOST \x05File System\x1C)") } else { txt.print("\x1C(\x99Fat\x1E32 \x05File System\x1C)") }

  MyPlot(28,1)
  txt.print(PENV.CPU)
  spc(2) txt.print ("\x9BOn\x1E: \x05")
  if PENV.ON_EMU { txt.print("X16 Emulator") }
  if PENV.ON_HARDWARE { txt.print("X16 Hardware") }
  spc(2)
  txt.print("\x9BROM\x1E: \x9Cr\x99") txt.print(conv.str_ub(ROM.VERSION))
  if ROM.IsPreRelease { txt.print("\x1E(\x9BPre-Release\x1E)") }
}

sub CHECK_IS_DIRECT() -> bool {
   return ((VideoHeader.VTYPE==23) or (VideoHeader.VTYPE==28))
}

sub CHECK_IS_STANDARD() -> bool {
    return ((VideoHeader.VTYPE==1) or (VideoHeader.VTYPE==3) or (VideoHeader.VTYPE==6) or (VideoHeader.VTYPE==8) or VideoHeader.VTYPE==2 or VideoHeader.VTYPE==7)
}

sub CHECK_SEP_AUDIO() -> bool {
  return ((VideoHeader.VTYPE<4) or (VideoHeader.VTYPE==23))
}

sub CHECK_SPRITE_VIDEO() -> bool {
   return ((VideoHeader.VTYPE==1) or (VideoHeader.VTYPE==6) or (VideoHeader.VTYPE==2) or (VideoHeader.VTYPE==7))
}

sub CHECK_BITMAP_VIDEO() -> bool {
   return ((VideoHeader.VTYPE==3) or (VideoHeader.VTYPE==8) or (VideoHeader.VTYPE==23) or (VideoHeader.VTYPE==28))
}

sub PARSE_VideoHeader() -> bool {
   MAKE_ALL_FALSE()
   if VideoHeader.ID[0]== main.FILEID[0]  and VideoHeader.ID[1]== main.FILEID[1]  and VideoHeader.ID[2]== main.FILEID[2]  and VideoHeader.ID[3]== main.FILEID[3] {
      IS_VIDEO = true
      F_FPS = VideoHeader.FPS as float
      NumFrames = (65536.0 * VideoHeader.NumFramesHI as float) + VideoHeader.NumFramesLO as float
      if VideoHeader.XAUD_FLAG == 'X' {
         PCM_IS_16BIT = (( VideoHeader.XCFG_BYTE & %00100000 ) == %00100000 )
         PCM_IS_STEREO = (( VideoHeader.XCFG_BYTE & %00010000 ) == %00010000 )
         if VideoHeader.X_F_FPS > 0 { F_FPS = VideoHeader.X_F_FPS as float / 100.0 }
       }
      TRUE_FPS = F_FPS
      VID_HAS_PALETTE = true
      if ((VideoHeader.VTYPE==2) or (VideoHeader.VTYPE==7)) {
         VID_HAS_PALETTE = false
         FIND_PAL_FILE()
      }
      IS_SPI_VIDEO = CHECK_IS_DIRECT()
      IS_STD_VIDEO = CHECK_IS_STANDARD()
      IS_SPRITE_VIDEO = CHECK_SPRITE_VIDEO()
      IS_BITMAP_VIDEO = CHECK_BITMAP_VIDEO()
      AUDIO_TRACK_IN_SEPERATEFILE = CHECK_SEP_AUDIO()
      IS_SUPPORTED_VIDEO = (IS_SPI_VIDEO or IS_STD_VIDEO)
    }
   success = IS_SUPPORTED_VIDEO
   return IS_VIDEO
}

sub ReadIn_VideoHeader() {
    cbm.CHKIN(VideoLFN)
    for i in 0 to 31 { vHeader[i] = cbm.GETIN2() }
    cbm.CLRCHN()
}

sub OpenVideo() {
    fopen(VideoLFN, drivenumber , VideoLFN, VIDFILE)
    ReadIn_VideoHeader()
    if PARSE_VideoHeader() { SetVideoAttributes() }
}

sub GetMediaFile_BASENAME() {
    FADE2BLUE()
    VID_FILE_FOUND = false
    IS_PCM_ONLY = false
    PCM_FILE_FOUND = false
    LoadHD_Logo(35,5,&spritelogo2,2,$31,false)
    LoadHD_Logo(145,5,&spritelogo3,4,$41,false)
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
      txt.print ("    \x05Version\x1E: ") txt.print(PENV.ISOASC_VERSTR)

    MyPlot(13,0)
    txt.print("\x0D \x05Enter Media Name \x05(\x99Without extension\x05)\x1E:\x9E ")
    BASENAME[0]=0
    SetDefaultPalette()
    sprites.show(2) sprites.show(4)
    txt.input_chars(&BASENAME)
    ISOUpper(BASENAME)
}

sub Yes(ubyte y @R9, ubyte x @R10) -> bool {
  ubyte InChar=0
  ubyte StoredChar=32

  printit:
     MyPlot(y, x)
     cbm.CHROUT(StoredChar)
  skipprint:
     InChar = cbm.GETIN2()
     if InChar==217 or InChar==121 { InChar=89 }  ; force PETSCII or ISO to UpperCase for Y
     if InChar==110 or InChar==206 { InChar=78 }  ; ditto for N
     when InChar {
         0 -> { goto skipprint }
     89,78 -> { StoredChar=InChar goto printit }
        27 -> { StoredChar = 78 }
        13 -> { if not (StoredChar==89 or StoredChar==78) { Boing() goto skipprint }  }
      else -> { Boing() goto skipprint }
    }
    return (StoredChar==89)
}


float ScrTimer
ubyte SAVE_DC_VIDEO

sub SetScreenTimer() {
    ScrTimer = floats.time()
    if ScrTimer > 16773000 {
       ScrTimer = 600.0
      }
     else
      { ScrTimer = ScrTimer + 3600 }
}

sub Print_PCM_FILE_ERROR(str msg) {
    txt.color2(15,0)
    ClearUpperRows()
    MyPlot(1,0) txt.print("\x9E ") txt.print(AUDFILE)
    MyPlot(3,0) txt.print("\x05 ") txt.print(msg)
    Boing()
    txt.color2(15,6)
    sys.wait(180)
}

sub DO_PLAYVIDEO_SPI() {
    cx16.i2c_write_byte($42,$05, $FF)   ; Turn on the (disc) Activity Light
    PlayVideo_SPI()
    cx16.i2c_write_byte($42,$05, $00)   ; Turn OFF the (disc) Activity Light
}

sub SETUP_PLAY_PCMSTREAM() {
      cx16.rambank(STD_ENGINE_BANK)
      SetAudioLFN(AudioLFN)
      SET_PCM_ONLY()
      if TapeAnimationAvailable() and (AudioByteRate < 120000) {
         TapeAnimation_INIT()
         SET_FRAME_DRIVER(&NextSpriteFrame)
       } else {
         if FExists("ASSETS/CDROM.IMG",16) {
            cx16.screen_mode($80,false)
            HideBothLayers()
            LoadVERAImage("ASSETS/CDROM.IMG")
            PrintPlayerControls(27,true)
            txt.row(24) txt.column(0)
            PrintPCMFormat()
            j=35
            ShowBothLayers()
          }
         else {
            PrintAudioPlayScreen()
            j=75
           }
            SetUpPlayBar(29, 178, j)
            SET_FRAME_DRIVER(&PrintPlayBar)
            showTextLayer()
         }
         SetDefaultPalette()
}

sub PrepareVideoPlay() {
    setPlayerScreen()
    if IS_STD_VIDEO {
      cx16.rambank(STD_ENGINE_BANK)
      VERAcfgvar = 0
      if VideoHeader.XAUD_FLAG == 'X' { VERAcfgvar = (VideoHeader.XCFG_BYTE & %00110000) }
      ClearAllBuffers()
      if (VideoHeader.BPP < 8) { if VID_HAS_PALETTE { SetDefaultPalette() BlankPAL_LAST32() } }
      if VID_HAS_PALETTE
         { TURN_ON_PALETTE_HANDLING() }
      else
        {
          TURN_OFF_PALETTE_HANDLING()
          if PAL_FILE_FOUND
            {
            if VideoHeader.BPP==4
               { LoadVERAPalette(PALFILE, $F0) }
            else
               { LoadVERAPalette(PALFILE,0) }
            }
          else { SetDefaultPalette() }
      }
   }

   if IS_SPI_VIDEO {
      cx16.rambank(SPI_ENGINE_BANK)
      CLEARGRAPH()
      hideTextLayer()
      showGraphicsLayer()
    }
}

sub WaitForKey_RestoreColor() {
   txt.print(AnyMessage)
   txt.waitkey()
   txt.color2(1,6)
}

sub HIDESPRITES() { OB=peek(0) cx16.rambank(STD_ENGINE_BANK) HIDEALLSPRITES() cx16.rambank(OB) }

sub HIDESPRITES_AND_FADE2BLUE() {
    HIDESPRITES()
    FADE2BLUE()
}

sub BAD_ROM_MESSAGE() {
       txt.print("\x07\x0D\x9B   THIS PROGRAM \x05REQUIRES\x9B ROM VERSION \x99R\x0548 \x9BOR \x05 HIGHER\x96 !")
       txt.print("\x0D\x05   VERSION\x1E: \x99") txt.print(conv.str_ub(ROM.VERSION)) txt.print("\x9B WAS FOUND.\x0D")
       if ROM.IsPreRelease { txt.print("\x0D\x05   PRE-RELEASE ROM\x0D") }
}

sub Check816Loaders() -> bool, bool {
    bool enabled = FExists(ENABLE65816,15)
    tmpB = FExists(SPI816,strings.length(SPI816))
    repeat 2 { txt.nl() }
    if ShowLoadMessages and tmpB  { txt.print("\x0D\x0D\x99    ") txt.print(SPI816) txt.print("\x05 FOUND \x1E!\x0D") }
    tmpB2 = FExists(STD816,strings.length(STD816))
    if ShowLoadMessages and tmpB2 { txt.print("\x0D\x0D\x99    ") txt.print(STD816) txt.print("\x05 FOUND \x1E!\x0D") }
    if (not enabled) and (tmpB or tmpB2) {
       tmpB=false tmpB2=false
       if ShowLoadMessages {
          txt.print("\x0D\x9965816 \x9BCODE \x05LOADING \x9BIS \x96DISABLED\x1E!\x0D")
          sys.wait(290)
       }
    }
    return tmpB, tmpB2
}

sub DO_FILENAME_PROCESSING() {
    MAKE_ALL_FALSE()
    strings.copy(BASENAME, VIDFILE)
    strings.append(VIDFILE,VEXT)
    strings.copy(BASENAME, PRGFILE)
    strings.append(PRGFILE,PEXT)
    VID_FILE_FOUND = FExists(VIDFILE,strings.length(VIDFILE))
    if not VID_FILE_FOUND { FIND_AUDIO_FILE() }
}

sub LOAD_VIDEO_ENGINE() {
    if Load_816_STDCode {
       bload(STD816, STD_ENGINE_BANK, $A000)
       if ShowLoadMessages {
          MyPlot(13,5)
          txt.print(" \x05LOADED \x9965816 \x05STD \x9BENGINE \x05!")
          sys.wait(15)
       }
       goto skipunzipSTD
     }

    cx16.rambank(STD_ENGINE_BANK)
    cx16.memory_decompress(&playenginez, $A000)

skipunzipSTD:
    if Load_816_SPICode {
       bload(SPI816, SPI_ENGINE_BANK, $A000)
       if ShowLoadMessages {
          MyPlot(14,5)
          txt.print(" \x05LOADED \x9965816 \x05SPI \x9BENGINE \x05!")
          if Load_816_STDCode { sys.wait(70) } else { sys.wait(85) }
       }
       goto skipunzipSPI
    }

    cx16.rambank(SPI_ENGINE_BANK)
    cx16.memory_decompress(&playengineSPI, $A000)
skipunzipSPI:
}


ubyte choice

sub start() {
    PENV.GetRunEnvironment()
    if (ROM.VERSION < 48) or ((ROM.VERSION==48) and ROM.IsPreRelease) {
        BAD_ROM_MESSAGE()
        goto skiplast
    }
    if Volume==0 or Volume>15 { Volume = 12 }
    ProgramBanner()
    void diskio.fastmode(3)
    sys.wait(90)
    ShowLoadMessages = true
    bool IsReload = false

    Load_816_SPICode=false Load_816_STDCode=false
    if (PENV.cpu==816) { Load_816_SPICode, Load_816_STDCode=Check816Loaders() }
loadlib:
    LOAD_VIDEO_ENGINE()

    if ShowLoadMessages { FADE2BLUE() }
    ShowLoadMessages = false
    cx16.set_screen_mode(1)
    txt.cp437()
    SAVE_DC_VIDEO = cx16.VERA_DC_VIDEO
    if IsReload goto LoadTheVideo

getfilename:
    GetMediaFile_BASENAME()

    if BASENAME=="EXIT" { goto alldone }
    if BASENAME=="LIST" {
       sprites.reset(2,3)
       ShowDirectory_LIST()
       HIDESPRITES()
       goto getfilename
     }
ProcessFileName:
    DO_FILENAME_PROCESSING()
    txt.print("\x0D\x05\x0D ")
    if VID_FILE_FOUND or PCM_FILE_FOUND {
       if VID_FILE_FOUND {
           strings.copy(VIDFILE,TMPFILE)
       } else {
           if PCM_FILE_FOUND { strings.copy(AUDFILE,TMPFILE) }
    }
       txt.print(TMPFILE)
       txt.print("\x99 FOUND !\x0D")
       sys.wait(45)
       HIDESPRITES_AND_FADE2BLUE()
LoadTheVideo:
      if VID_FILE_FOUND { OpenVideo() }
      if IS_PCM_ONLY {
         OpenAudioFile(FOUND_EXT)
         if IS_Z_FILE {
            Read_ZCM_HEADER(AudioLFN)
            if VERIFY_ZCM_HEADER() { goto GetUserChoice }
               Print_PCM_FILE_ERROR("\x05Invalid or Unsupported \x99ZCM\x05 file!")
               goto getfilename
         }
         if IS_WAV_FILE {
            Read_WAV_HEADER(AudioLFN)
            PARSE_WAV_HEADER()
            if success { goto GetUserChoice }
               Print_PCM_FILE_ERROR("\x05Invalid or Unsupported \x99WAV\x05 file!")
               goto getfilename
          }
       }

GetUserChoice:
      hideTextLayer()
      PrintMediaAttributes()
      if MAKE_WIDE_SCREEN {
         MyPlot(4,44) txt.print(" \x1E(\x9EWide Screen ")
         if DISABLE_WIDE_SCREEN
           { txt.print("\x9Boff\x1E) ") }
         else
           { txt.print("\x1E(\x99on\x1E) ") }
      }

      MenuBar()
      ShowVolume()
      SetDefaultPalette()
      showTextLayer()
      cbm.CLRCHN()
      void diskio.status()
      MyPlot(27,1)
      diskio.fastmode(3)
      SetScreenTimer()
getchoice:
      choice = cbm.GETIN2()
      tmpf = floats.time()
      if (tmpf > ScrTimer) and (tmpf < 16773000) { cx16.VERA_DC_VIDEO = 0 }  ; Blank the Screen
SkipChoice:
      if choice==0 { goto getchoice }
      ; If a key is pressed reset the Screen blank timer
      cx16.VERA_DC_VIDEO = SAVE_DC_VIDEO
      SetScreenTimer()

      if choice==133 {
         if CAN_PLAY {
            if IS_SPI_VIDEO and (ROM.VERSION==49) and ROM.IsPreRelease { SPI_WARN() }
            FADE2BLACK()
            if IS_PCM_ONLY { goto PlayAudioFile }
            else { goto PlayTheVideo }
          }
       }

      if choice==145
      {
        if Volume < 15 {
            Volume++
            ShowVolume()
            Woop()
        }
        else {
          Boing()
          while cbm.GETIN2() != 0 { }
         }
          goto getchoice
       }

      if choice==17 { if Volume > 0 {Volume-- ShowVolume() Woop()} else { Boing() } goto getchoice }

      if choice==134 or ((choice==138) and UseBookMark) {
         ResetAllLFNs()
         if choice==134 { HIDESPRITES_AND_FADE2BLUE() }
         if choice==138 {
            ClearUpperRows()
            CleanUpMarkFile()
            MyPlot(1,1)
            txt.print("\x99") txt.print(AUDFILE) txt.print("\x96 Removed\x05!")
            MyPlot(3,2)
            WaitForKey_RestoreColor()
            HIDESPRITES_AND_FADE2BLUE()
            goto LoadTheVideo
          }

         BASENAME[0] = 0
         ;AUDFILE[0] = 0
         ;VIDFILE[0] = 0
         IsReload = false
         goto loadlib
         }

      if (choice==23) and (Load_816_SPICode or Load_816_STDCode) {
         ClearUpperRows()
         MyPlot(1,1) txt.print("\x05 Loading \x9965C02 \x9BDriver \x1E!")
         Load_816_SPICode=false Load_816_STDCode=false ShowLoadMessages=false
         LOAD_VIDEO_ENGINE()
         txt.print("\x05.....\x96Done \x1E!")
         MyPlot(3,2) txt.print("\x9BRemove Enabling file \x05") txt.print(ENABLE65816) txt.print("\x99 Y\x1E/\x99N \x9B--\x05>")
         txt.color2(7,11)
         if Yes(3, 47) { diskio.delete(ENABLE65816) }
         txt.color2(1,6) HIDESPRITES_AND_FADE2BLUE()
         goto LoadTheVideo
      }

      if choice==137 and IS_SUPPORTED_VIDEO {
         ResetAllLFNs()
         MakeStandAlonePlayer()
         MyPlot(3,2)
         WaitForKey_RestoreColor()
         goto LoadTheVideo
      }

      if choice==27 { goto alldone }

      if (not IS_SUPPORTED_VIDEO) and (UseStandAlonePlayer) and (choice==16) {
         PrepareRun(PRGFILE)
         sprites.reset(2,1)
         goto skiplast
         }

      Boing()
      goto getchoice

PlayAudioFile:
      SETUP_PLAY_PCMSTREAM()
      goto START_ENGINE_PLAY
PlayTheVideo:
      PrepareVideoPlay()
START_ENGINE_PLAY:
      if IS_STD_VIDEO or IS_PCM_ONLY {
        cx16.rambank(STD_ENGINE_BANK)
        PlayVideo_STD(mkword(VERAcfgvar,VideoHeader.VERARate))
      }
      if IS_SPI_VIDEO { DO_PLAYVIDEO_SPI() }
      if MAKE_WIDE_SCREEN { RESTORE_LINES() }
      if not (MANEXIT==$FF) { sys.wait(90) }
      if IS_BITMAP_VIDEO and (VideoHeader.BPP==8)  { FADE2BLACK() } else { pal2black() }
      HIDESPRITES()
      hideGraphicsLayer()
      verafx.clear(0,0,0,19200)
      verafx.clear(1,0,0,15359)
      cx16.set_screen_mode(1)
      txt.cp437()
      showTextLayer()
      goto skiptoend
    }
      txt.print("\x96 NOT FOUND !\x0D\x05")
      Boing() sys.wait(75)
      HIDESPRITES()
      goto getfilename
 skiptoend:
    if MANEXIT==$FF {
       FADE2BLUE()
       cx16.rambank(STD_ENGINE_BANK)
       cx16.vaddr(1,$FA08,0,1)
       cx16.VERA_DATA0 = $EE
       cx16.VERA_DATA0 = $0F
       cx16.VERA_DC_HSCALE=92
       cx16.VERA_DC_VSCALE=80
       MyPlot(2,2)
       txt.color(4) txt.print(" Saving BookMark !")
       if IS_STD_VIDEO {
         GetVideoPosition()
         Frame=Get_VidFrameNumber_STD(vidFilePosition)
        }
       if IS_PCM_ONLY { GetAudioPosition() AdjustAudioPosition() Frame=audFilePosition }
       if IS_SPI_VIDEO { CURFRAMEINT_TOFLOAT() }
       strings.copy("@:", TMPFILE)
       strings.append(TMPFILE,BASENAME)
       strings.append(TMPFILE,".MARK,S,W")
       fopen(7,drivenumber,7,TMPFILE)
       %asm {{
               ldx #7
               jsr cbm.CHKOUT
               ldx #<p8v_Frame
               ldy #>p8v_Frame
               lda #5
               jsr cx16.MCIOUT
            }}
       cbm.CLRCHN()
       cbm.CLOSE(7)
       if IS_PCM_ONLY { Frame=0 }
    } else {
          CleanUpMarkFile()
        }
      FADE2BLACK()
      Volume = AUDIO_CTRL & $0F
      ResetAllLFNs()
      IsReload = true
      goto loadlib
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
   if BASENAME=="EXIT" { goto skipoutputstuff2 }
   txt.print("\x0D ")  txt.print(BASENAME) txt.color(7)
skipoutputstuff2:
   txt.print(" DONE \x05!\x0D\x0D")
skiplast:
   HIDESPRITES()
   ResetAllLFNs()
 }
}
