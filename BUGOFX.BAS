' ---- Common Consts ----

CONST SCRWIDTH% = 320
CONST SCRHEIGHT% = 200
CONST SCRWIDTH2% = SCRWIDTH% \ 2
CONST SCRHEIGHT2% = SCRHEIGHT% \ 2

CONST FALSE% = 0
CONST TRUE% = 1

CONST PI! = 3.14159265359#


' ---- Main Functions ----

DECLARE SUB FFIX (Mode%)

DECLARE SUB InitPrecalcs ()

DECLARE SUB SetDefaultPalette ()
DECLARE SUB SetGradPal (c0%, c1%, r0%, g0%, b0%, r1%, g1%, b1%)
DECLARE SUB DisplayPalette ()

DECLARE SUB ClearVram ()
DECLARE SUB WriteVram ()
DECLARE SUB WaitForVsync ()

DECLARE SUB HandleInput (dt!)


' ---- FX functions  ----


' ---- Main arrays ----

'$DYNAMIC

DIM SHARED vram%(0 TO 32007)
vram%(6) = 320 * 8
vram%(7) = 200

'$STATIC

DIM SHARED quit%
quit% = FALSE%


' ---- Init ----

' Load FFIX to memory
'FFIX 0

InitPrecalcs

SCREEN 13

COLOR 63

SetDefaultPalette


' ---- Main Loop ----

freeMem& = FRE(-1)

nframe& = 0


t! = 0

timeStart! = TIMER
timeDiff! = 0
lastTime! = timeStart!
DO WHILE quit% = FALSE%

    'ClearVram

    HandleInput timeDiff!

    'WaitForVsync

    WriteVram

    'DisplayPalette

    'PRINT freeMem&

    t! = t! + .01
    nframe& = nframe& + 1

    timeDiff! = TIMER - lastTime!
    lastTime! = TIMER
    timeElapsed! = TIMER - timeStart!
LOOP
timeEnd! = TIMER

PRINT nframe& / (timeEnd! - timeStart!)
SLEEP

' Unload FFIX from memory
'FFIX 1

SUB ClearVram

    REDIM vram%(0 TO 32007)

    vram%(6) = 320 * 8
    vram%(7) = 200

END SUB

SUB DisplayPalette

    FOR y% = 0 TO 55
        DEF SEG = &HA000 + y% * 20
        FOR x% = 0 TO 256
            IF y% < 50 THEN
                POKE x%, x%
            ELSE
                IF (x% AND 63) = 0 THEN POKE x%, 127
            END IF
        NEXT x%
    NEXT y%

END SUB

SUB FFIX (Mode%)

STATIC OldISR1%, OldISR2%, OldISR3%, OldISR4%

IF Mode% = 0 THEN

    DIM isr(0 TO 5) AS LONG                     'FFix by Dav,Plasma and v1ctor
    isr(0) = &H53EC8B55: isr(1) = &H83025E8B
    isr(2) = &H8E0602EB: isr(3) = &HC7260446
    isr(4) = &H79B9007: isr(5) = &HCF9B5D5B
    DEF SEG = 0
    OldISR1% = PEEK(&HF4)
    OldISR2% = PEEK(&HF5)
    OldISR3% = PEEK(&HF6)
    OldISR4% = PEEK(&HF7)
    POKE &HF4, VARPTR(isr(0)) AND 255
    POKE &HF5, (CLNG(VARPTR(isr(0))) AND &HFF00&) \ 256
    POKE &HF6, VARSEG(isr(0)) AND 255
    POKE &HF7, (CLNG(VARSEG(isr(0))) AND &HFF00&) \ 256

ELSE

    IF OldISR1% <> 0 AND OldISR2% <> 0 AND OldISR3% <> 0 AND OldISR4% <> 0 THEN

        DEF SEG = 0
        POKE &HF4, OldISR1%
        POKE &HF5, OldISR2%
        POKE &HF6, OldISR3%
        POKE &HF7, OldISR4%
   
    END IF

END IF

END SUB

SUB HandleInput (dt!)

    key$ = INKEY$

    IF key$ <> "" THEN
        IF ASC(key$) = 27 THEN
            quit% = TRUE%
            EXIT SUB
        END IF
    END IF

    SELECT CASE key$

        CASE "q", "Q"

        CASE "w", "W"

        CASE "e", "E"

    CASE ELSE

    END SELECT

END SUB

SUB InitPrecalcs

END SUB

SUB SetDefaultPalette

OUT &H3C8, 0
OUT &H3C9, 0
OUT &H3C9, 0
OUT &H3C9, 0

CONST palTest% = 2

    SELECT CASE palTest%

        CASE 0

            FOR i% = 1 TO 127
                OUT &H3C9, (1 * i% \ 2) AND 63
                OUT &H3C9, (.75 * i% \ 2) AND 63
                OUT &H3C9, (2 * i% \ 2) AND 63
            NEXT i%
            FOR i% = 128 TO 255
                OUT &H3C9, (2 * (i% - 128) \ 2) AND 63
                OUT &H3C9, (1 * (i% - 128) \ 2) AND 63
                OUT &H3C9, (.75 * (i% - 128) \ 2) AND 63
            NEXT i%

        CASE 1

            FOR i% = 1 TO 63
                OUT &H3C9, i% \ 1
                OUT &H3C9, 16 + i% \ 2
                OUT &H3C9, 32 + i% \ 4
            NEXT i%

            FOR i% = 64 TO 127
                ii% = i% - 64
                OUT &H3C9, ii% \ 3
                OUT &H3C9, ii% \ 1
                OUT &H3C9, ii% \ 4
            NEXT i%

            FOR i% = 128 TO 191
                ii% = i% - 128
                OUT &H3C9, ii% \ 3
                OUT &H3C9, ii% \ 4
                OUT &H3C9, ii% \ 1
            NEXT i%

            FOR i% = 192 TO 255
                ii% = i% - 192
                OUT &H3C9, ii% \ 1
                OUT &H3C9, ii% \ 3
                OUT &H3C9, ii% \ 1
            NEXT i%

        CASE 2
            r0% = 0: g0% = 0: b0% = 0
            r1% = 15: g1% = 23: b1% = 31
            r2% = 63: g2% = 63: b2% = 63

            SetGradPal 0, 31, r0%, g0%, b0%, r1%, g1%, b1%
            SetGradPal 32, 63, r1%, g1%, b1%, r2%, g2%, b2%
           
            SetGradPal 64, 95, b0%, g0%, r0%, b1%, g1%, r1%
            SetGradPal 96, 127, b1%, g1%, r1%, b2%, g2%, r2%
           
            SetGradPal 128, 159, r0%, b0%, g0%, r1%, b1%, g1%
            SetGradPal 160, 191, r1%, b1%, g1%, r2%, b2%, g2%
           
            SetGradPal 192, 223, g0%, r0%, b0%, g1%, r1%, b1%
            SetGradPal 224, 255, g1%, r1%, b1%, g2%, r2%, b2%

        CASE ELSE

    END SELECT

END SUB

SUB SetGradPal (c0%, c1%, r0%, g0%, b0%, r1%, g1%, b1%)

    dc% = c1% - c0%
    r! = r0%: g! = g0%: b! = b0%
    dr! = (r1% - r0%) / dc%
    dg! = (g1% - g0%) / dc%
    db! = (b1% - b0%) / dc%

    OUT &H3C8, c0%

    FOR c% = c0% TO c1%
        IF r! < 0 THEN r! = 0
        IF r! > 63 THEN r! = 63
        IF g! < 0 THEN g! = 0
        IF g! > 63 THEN g! = 63
        IF b! < 0 THEN b! = 0
        IF b! > 63 THEN b! = 63

        OUT &H3C9, INT(r!)
        OUT &H3C9, INT(g!)
        OUT &H3C9, INT(b!)

        r! = r! + dr!
        g! = g! + dg!
        b! = b! + db!
    NEXT c%

END SUB

SUB WaitForVsync

    WAIT &H3DA, 8
    WAIT &H3DA, 8, 8

END SUB

SUB WriteVram

    PUT (0, 0), vram%(6), PSET

END SUB

