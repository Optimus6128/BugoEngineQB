DECLARE SUB RunFxRotozoomer ()
DECLARE SUB InitFxRotozoomer ()
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
DIM SHARED vsync%
quit% = FALSE%
vsync% = FALSE%

DIM SHARED nframe&


' ---- Fx arrays ----

DIM SHARED fsin1%(0 TO 255)
DIM SHARED fsin2%(0 TO 255)
DIM SHARED fsin3%(0 TO 255)


' If you change the texture sizes (preferably always power of two) you have to manually change the numbers in the array declaration below
CONST RotoTexW% = 32
CONST RotoTexH% = 32
CONST RotoDivN% = RotoTexW% * 256 - 1
CONST fuw% = RotoTexW% - 1
CONST fuh% = RotoTexH% - 1

DIM SHARED tile%(0 TO fuw%, 0 TO fuh%)' using the consts here would degrade performance, suddenly the compiler will not produce shifts for 16 but imul
' update, using second consts to do first const minus 1 will save you. Weird.. but ugly

DIM SHARED RotoDivs%(0 TO RotoDivN%)


' ---- Fx index ----

CONST fxPlasma% = 0
CONST fxRotozoomer% = 1
CONST fxFloor% = 2
CONST fxBump% = 3
CONST fxSphere% = 4
CONST fxVoxel% = 5
CONST fxWater% = 6

fxCurrent% = fxRotozoomer%


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

    IF vsync% = TRUE% THEN WaitForVsync

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

        CASE "v", "V"
            vsync% = vsync% XOR TRUE%

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

SUB InitFxRotozoomer

    FOR y% = 0 TO RotoTexH% - 1
        FOR x% = 0 TO RotoTexW% - 1
            tile%(y%, x%) = (y% AND x%) * RND(255)
        NEXT x%
    NEXT y%

    FOR i% = 0 TO RotoDivN%
        RotoDivs%(i%) = i% \ 256
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

            SetGradPal 0, 15, 0, 0, 0, 63, 47, 31

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
            InitFxRotozoomer

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
            RunFxRotozoomer

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

SUB RunFxRotozoomer

    STATIC rotAng!

    t% = nframe& AND 32767

    zoom! = SIN(t% * .02) + 1.5
   
    du% = COS(rotAng!) * 64 * zoom!
    dv% = SIN(rotAng!) * 64 * zoom!

    du2% = 2 * du%
    dv2% = 2 * dv%
    du3% = 3 * du%
    dv3% = 3 * dv%
    du4% = 4 * du%
    dv4% = 4 * dv%
    du5% = 5 * du%
    dv5% = 5 * dv%
    du6% = 6 * du%
    dv6% = 6 * dv%
    du7% = 7 * du%
    dv7% = 7 * dv%
    du8% = 8 * du%
    dv8% = 8 * dv%

    u% = -160 * du%
    v% = -100 * dv%

    screenSeg& = VARSEG(vram%(0)) + 1

    FOR y% = 0 TO SCRHEIGHT% - 1
        DEF SEG = screenSeg& + y% * 20

        pu% = u%
        pv% = v%

        FOR x% = 0 TO SCRWIDTH% - 1 STEP 8

            POKE x%, tile%(RotoDivs%(pv% AND RotoDivN%), RotoDivs%(pu% AND RotoDivN%))
            POKE x% + 1, tile%(RotoDivs%((pv% + dv%) AND RotoDivN%), RotoDivs%((pu% + du%) AND RotoDivN%))
            POKE x% + 2, tile%(RotoDivs%((pv% + dv2%) AND RotoDivN%), RotoDivs%((pu% + du2%) AND RotoDivN%))
            POKE x% + 3, tile%(RotoDivs%((pv% + dv3%) AND RotoDivN%), RotoDivs%((pu% + du3%) AND RotoDivN%))
            POKE x% + 4, tile%(RotoDivs%((pv% + dv4%) AND RotoDivN%), RotoDivs%((pu% + du4%) AND RotoDivN%))
            POKE x% + 5, tile%(RotoDivs%((pv% + dv5%) AND RotoDivN%), RotoDivs%((pu% + du5%) AND RotoDivN%))
            POKE x% + 6, tile%(RotoDivs%((pv% + dv6%) AND RotoDivN%), RotoDivs%((pu% + du6%) AND RotoDivN%))
            POKE x% + 7, tile%(RotoDivs%((pv% + dv7%) AND RotoDivN%), RotoDivs%((pu% + du7%) AND RotoDivN%))

            pu% = pu% + du8%
            pv% = pv% + dv8%

        NEXT x%

        u% = u% - dv%
        v% = v% + du%
    NEXT y%

    rotAng! = rotAng! + .025

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

