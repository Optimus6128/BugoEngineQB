' ---- Common Consts ----

CONST SCRWIDTH% = 320
CONST SCRHEIGHT% = 200
CONST SCRWIDTH2% = SCRWIDTH% \ 2
CONST SCRHEIGHT2% = SCRHEIGHT% \ 2

CONST FALSE% = 0
CONST TRUE% = 1

CONST PI! = 3.14159265359#
CONST RAD256! = 256! / (2 * PI!)


' ---- Main Functions ----

DECLARE SUB FFIX (Mode%)

DECLARE SUB InitPrecalcs (fxType%)

DECLARE SUB InitPalette (fxType%)
DECLARE SUB SetGradPal (c0%, c1%, r0%, g0%, b0%, r1%, g1%, b1%)
DECLARE SUB DisplayPalette ()

DECLARE SUB ClearVram ()
DECLARE SUB WriteVram ()
DECLARE SUB WaitForVsync ()

DECLARE SUB HandleInput (dt!)


' ---- FX functions  ----

DECLARE SUB InitFxPlasma ()

DECLARE SUB RunFxPlasma ()

DECLARE SUB RunFx (fxType%)

' ---- Main arrays ----

'$DYNAMIC

DIM SHARED vram%(0 TO 32007)
vram%(6) = 320 * 8
vram%(7) = 200

'$STATIC

DIM SHARED quit%
quit% = FALSE%

DIM SHARED nframe&


' ---- Fx arrays ----

DIM SHARED fsin1%(0 TO 255)
DIM SHARED fsin2%(0 TO 255)
DIM SHARED fsin3%(0 TO 255)


' ---- Fx index ----

CONST fxPlasma% = 0
CONST fxRotozoomer% = 1
CONST fxFloor% = 2
CONST fxBump% = 3
CONST fxSphere% = 4
CONST fxVoxel% = 5
CONST fxWater% = 6

fxCurrent% = fxPlasma%


' ---- Init ----

' Load FFIX to memory
'FFIX 0

InitPrecalcs fxCurrent%

SCREEN 13

InitPalette fxCurrent%


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

    RunFx fxCurrent%

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

SUB InitFxPlasma

    FOR i% = 0 TO 255
        fsin1%(i%) = INT(SIN(i% / RAD256!) * 31!)
        fsin2%(i%) = INT(SIN(i% / (RAD256! / 2)) * 45!)
        fsin3%(i%) = INT(SIN(i% / (RAD256! / 4)) * 7!)
    NEXT i%

END SUB

SUB InitPalette (fxType%)

    OUT &H3C8, 0
    OUT &H3C9, 0
    OUT &H3C9, 0
    OUT &H3C9, 0

    SELECT CASE fxType%

        CASE fxPlasma%

            SetGradPal 0, 63, 0, 0, 0, 15, 31, 63
            SetGradPal 64, 127, 15, 31, 63, 63, 47, 15
          
            SetGradPal 128, 191, 63, 47, 15, 47, 15, 63
            SetGradPal 192, 255, 47, 15, 63, 0, 0, 0

        CASE fxRotozoomer%

        CASE fxFloor%

        CASE fxBump%

        CASE fxSphere%

        CASE fxVoxel%

        CASE fxWater%

        CASE ELSE

    END SELECT

    COLOR 63

END SUB

SUB InitPrecalcs (fxType%)

    SELECT CASE fxType%

        CASE fxPlasma%
            InitFxPlasma

        CASE fxRotozoomer%

        CASE fxFloor%

        CASE fxBump%

        CASE fxSphere%

        CASE fxVoxel%

        CASE fxWater%

        CASE ELSE

    END SELECT

END SUB

SUB RunFx (fxType%)

    SELECT CASE fxType%

        CASE fxPlasma%
            RunFxPlasma

        CASE fxRotozoomer%

        CASE fxFloor%

        CASE fxBump%

        CASE fxSphere%

        CASE fxVoxel%

        CASE fxWater%

        CASE ELSE

    END SELECT

END SUB

SUB RunFxPlasma

    DIM fsinX%(0 TO SCRWIDTH% - 1)

    t% = (nframe& \ 2) AND 32767

    i% = 0
    FOR x% = 0 TO SCRWIDTH% - 1 STEP 2
        c0% = x% + fsin1%((x% + t%) AND 255) + fsin2%((x% + fsin3%((x% - 2 * t%) AND 255)) AND 255)
        c1% = x% + 1 + fsin1%((x% + 1 + t%) AND 255) + fsin2%((x% + 1 + fsin3%((x% + 1 - 2 * t%) AND 255)) AND 255)
        fsinX%(i%) = (c1% * 256) OR (c0% AND 255)
        i% = i% + 1
    NEXT x%

    i% = 8
    FOR y% = 0 TO SCRHEIGHT% - 1

        yp% = y% - fsin2%((y% - t%) AND 255) + fsin3%((fsin1%((y% + t%) AND 255) + y% + 3 * t%) AND 255)
        yp% = yp% AND 255
        yp% = (yp% * 256) OR yp%
        FOR x% = 0 TO SCRWIDTH% / 2 - 1 STEP 8
            vram%(i%) = fsinX%(x%) + yp%
            vram%(i% + 1) = fsinX%(x% + 1) + yp%
            vram%(i% + 2) = fsinX%(x% + 2) + yp%
            vram%(i% + 3) = fsinX%(x% + 3) + yp%
            vram%(i% + 4) = fsinX%(x% + 4) + yp%
            vram%(i% + 5) = fsinX%(x% + 5) + yp%
            vram%(i% + 6) = fsinX%(x% + 6) + yp%
            vram%(i% + 7) = fsinX%(x% + 7) + yp%
            i% = i% + 8
        NEXT x%
    NEXT y%

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

