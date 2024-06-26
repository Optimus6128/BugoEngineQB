DECLARE SUB MirrorVramY ()
TYPE StepVars
    u AS INTEGER
    v AS INTEGER
    du AS INTEGER
    dv AS INTEGER
    shade AS INTEGER
END TYPE

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
DECLARE SUB InitFxRotozoomer ()
DECLARE SUB InitFxFloor ()

DECLARE SUB RunFxPlasma ()
DECLARE SUB RunFxRotozoomer ()
DECLARE SUB RunFxFloor ()

DECLARE SUB RunFx (fxType%)

DECLARE SUB RotatePoint2D (x&, y&, angle!)
DECLARE SUB CalcLineStepVars (moveX&, moveY&, angle!)

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
CONST RotoTexW% = 16
CONST RotoTexH% = 16
CONST RotoDivN% = RotoTexW% * 256 - 1
CONST fuw% = RotoTexW% - 1
CONST fuh% = RotoTexH% - 1

DIM SHARED tile%(0 TO fuw%, 0 TO fuh%)' using the consts here would degrade performance, suddenly the compiler will not produce shifts for 16 but imul
' update, using second consts to do first const minus 1 will save you. Weird.. but ugly

DIM SHARED RotoDivs%(0 TO RotoDivN%)

DIM SHARED lineStepVars(0 TO SCRHEIGHT% - 1) AS StepVars


' ---- Fx index ----

CONST fxPlasma% = 0
CONST fxRotozoomer% = 1
CONST fxFloor% = 2
CONST fxBump% = 3
CONST fxSphere% = 4
CONST fxVoxel% = 5
CONST fxWater% = 6

fxCurrent% = fxFloor%


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

SCREEN 1

PRINT nframe& / (timeEnd! - timeStart!)
SLEEP

' Unload FFIX from memory
'FFIX 1

SUB CalcLineStepVars (moveX&, moveY&, angle!)

    CONST SKYHEIGHT& = 4096

    FOR y% = 0 TO SCRHEIGHT% \ 2 - 1
        yp% = SCRHEIGHT% \ 2 - y%
        IF yp% < 0 THEN yp% = -yp%
        IF yp% = 0 THEN yp% = 1

        z& = SKYHEIGHT& \ yp%
        u& = 0 + moveX&
        v& = 128 * z& + moveY&

        du& = z&
        dv& = 0

        RotatePoint2D u&, v&, angle!
        RotatePoint2D du&, dv&, angle!

        u& = u& + (-SCRWIDTH% \ 2) * du&
        v& = v& + (-SCRHEIGHT% \ 2) * dv&

        lineStepVars(y%).u = u&
        lineStepVars(y%).v = v&
        lineStepVars(y%).du = du&
        lineStepVars(y%).dv = dv&

        s% = z& \ 64
        IF s% < 0 THEN s% = 0
        IF s% > 15 THEN s% = 15
        lineStepVars(y%).shade = s%

        u% = u% - dv%
        v% = v% + du%
    NEXT y%

END SUB

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

SUB InitFxFloor

    FOR y% = 0 TO RotoTexH% - 1
        FOR x% = 0 TO RotoTexW% - 1
            tile%(y%, x%) = ((x% XOR y%) * RND) AND 15
        NEXT x%
    NEXT y%

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
            tile%(y%, x%) = 2 * (x% * y%)
        NEXT x%
    NEXT y%

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

            SetGradPal 0, 127, 15, 7, 31, 63, 47, 31
            SetGradPal 128, 255, 63, 47, 31, 15, 7, 31

        CASE fxFloor%

            FOR i% = 0 TO 15
                s% = i% * 16
                ii% = 15 - i%
                r1% = (31 * ii%) \ 15: g1% = (27 * ii%) \ 15: b1% = (15 * ii%) \ 15
                r2% = (23 * ii%) \ 15: g2% = (7 * ii%) \ 15: b2% = (15 * ii%) \ 15
                r3% = (47 * ii%) \ 15: g3% = (31 * ii%) \ 15: b3% = (31 * ii%) \ 15
                SetGradPal s% + 0, s% + 3, 0, 0, 0, r1%, g1%, b1%
                SetGradPal s% + 4, s% + 7, r1%, g1%, b1%, r2%, g2%, b2%
                SetGradPal s% + 8, s% + 11, r2%, g2%, b2%, r3%, g3%, b3%
                SetGradPal s% + 12, s% + 15, r3%, g3%, b3%, 0, 0, 0
            NEXT i%

        CASE fxBump%

        CASE fxSphere%

        CASE fxVoxel%

        CASE fxWater%

        CASE ELSE

    END SELECT

END SUB

SUB InitPrecalcs (fxType%)

    mustInitRotoDivs% = FALSE%

    SELECT CASE fxType%

        CASE fxPlasma%
            InitFxPlasma

        CASE fxRotozoomer%
            InitFxRotozoomer
            mustInitRotoDivs% = TRUE%

        CASE fxFloor%
            InitFxFloor
            mustInitRotoDivs% = TRUE%

        CASE fxBump%

        CASE fxSphere%

        CASE fxVoxel%

        CASE fxWater%

        CASE ELSE

    END SELECT


    ' ---- Common Precalcs ----

    IF mustInitRotoDivs% = TRUE% THEN
        FOR i% = 0 TO RotoDivN%
            RotoDivs%(i%) = i% \ 256
        NEXT i%
    END IF

END SUB

SUB MirrorVramY

    FOR y% = 0 TO SCRHEIGHT% \ 2 - 1
        i% = 8 + y% * (SCRWIDTH% \ 2)
        j% = 8 + (SCRHEIGHT% - 1 - y%) * (SCRWIDTH% \ 2)
        FOR x% = 0 TO SCRWIDTH% \ 2 - 1 STEP 8
            vram%(j%) = vram%(i%)
            vram%(j% + 1) = vram%(i% + 1)
            vram%(j% + 2) = vram%(i% + 2)
            vram%(j% + 3) = vram%(i% + 3)
            vram%(j% + 4) = vram%(i% + 4)
            vram%(j% + 5) = vram%(i% + 5)
            vram%(j% + 6) = vram%(i% + 6)
            vram%(j% + 7) = vram%(i% + 7)
            i% = i% + 8: j% = j% + 8
        NEXT x%
    NEXT y%

END SUB

SUB RotatePoint2D (x&, y&, angle!)

    rx! = COS(angle!)
    ry! = SIN(angle!)

    xp! = x&
    yp! = y&

    x& = xp! * rx! - yp! * ry!
    y& = xp! * ry! + yp! * rx!

END SUB

SUB RunFx (fxType%)

    SELECT CASE fxType%

        CASE fxPlasma%
            RunFxPlasma

        CASE fxRotozoomer%
            RunFxRotozoomer

        CASE fxFloor%
            RunFxFloor

        CASE fxBump%

        CASE fxSphere%

        CASE fxVoxel%

        CASE fxWater%

        CASE ELSE

    END SELECT

END SUB

SUB RunFxFloor

    STATIC angle!
    STATIC posX&, posY&

    CalcLineStepVars posX&, posY&, angle!

    screenSeg& = VARSEG(vram%(0)) + 1

    FOR y% = 0 TO SCRHEIGHT% \ 2 - 1
        DEF SEG = screenSeg& + y% * 20

        u% = lineStepVars(y%).u
        v% = lineStepVars(y%).v
        du% = lineStepVars(y%).du
        dv% = lineStepVars(y%).dv
        shadePal% = lineStepVars(y%).shade * 16

        du2% = 2 * du%
        dv2% = 2 * dv%
        du3% = du2% + du%
        dv3% = dv2% + dv%
        du4% = 2 * du2%
        dv4% = 2 * dv2%
        du5% = du4% + du%
        dv5% = dv4% + dv%
        du6% = du4% + du2%
        dv6% = dv4% + dv2%
        du7% = du6% + du%
        dv7% = dv6% + dv%
        du8% = 2 * du4%
        dv8% = 2 * dv4%

        FOR x% = 0 TO SCRWIDTH% - 1 STEP 8

            POKE x%, tile%(RotoDivs%(v% AND RotoDivN%), RotoDivs%(u% AND RotoDivN%)) + shadePal%
            POKE x% + 1, tile%(RotoDivs%((v% + dv1%) AND RotoDivN%), RotoDivs%((u% + du1%) AND RotoDivN%)) + shadePal%
            POKE x% + 2, tile%(RotoDivs%((v% + dv2%) AND RotoDivN%), RotoDivs%((u% + du2%) AND RotoDivN%)) + shadePal%
            POKE x% + 3, tile%(RotoDivs%((v% + dv3%) AND RotoDivN%), RotoDivs%((u% + du3%) AND RotoDivN%)) + shadePal%
            POKE x% + 4, tile%(RotoDivs%((v% + dv4%) AND RotoDivN%), RotoDivs%((u% + du4%) AND RotoDivN%)) + shadePal%
            POKE x% + 5, tile%(RotoDivs%((v% + dv5%) AND RotoDivN%), RotoDivs%((u% + du5%) AND RotoDivN%)) + shadePal%
            POKE x% + 6, tile%(RotoDivs%((v% + dv6%) AND RotoDivN%), RotoDivs%((u% + du6%) AND RotoDivN%)) + shadePal%
            POKE x% + 7, tile%(RotoDivs%((v% + dv7%) AND RotoDivN%), RotoDivs%((u% + du7%) AND RotoDivN%)) + shadePal%

            u% = u% + du8%
            v% = v% + dv8%

        NEXT x%

    NEXT y%

    MirrorVramY

    angle! = angle! + .005
    posX& = posX& + 64 * COS(angle!)
    posY& = posY& + 64 * SIN(angle!)

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

    STATIC angle!

    t% = nframe& AND 32767
    zoom! = SIN(t% * .02) + 1.5

    du% = COS(angle!) * 64 * zoom!
    dv% = SIN(angle!) * 64 * zoom!

    du2% = 2 * du%
    dv2% = 2 * dv%
    du3% = du2% + du%
    dv3% = dv2% + dv%
    du4% = 2 * du2%
    dv4% = 2 * dv2%
    du5% = du4% + du%
    dv5% = dv4% + dv%
    du6% = du4% + du2%
    dv6% = dv4% + dv2%
    du7% = du6% + du%
    dv7% = dv6% + dv%
    du8% = 2 * du4%
    dv8% = 2 * dv4%

    u% = -160 * du%
    v% = -100 * dv%

    screenSeg& = VARSEG(vram%(0)) + 1

    FOR y% = 0 TO SCRHEIGHT% \ 2 - 1
        DEF SEG = screenSeg& + y% * 20

        pu% = u%
        pv% = v%

        FOR x% = 0 TO SCRWIDTH% \ 2 - 1 STEP 8

            c% = tile%(RotoDivs%(pv% AND RotoDivN%), RotoDivs%(pu% AND RotoDivN%)): POKE x%, c%: POKE SCRWIDTH% - 1 - x%, c%
            c% = tile%(RotoDivs%((pv% + dv1%) AND RotoDivN%), RotoDivs%((pu% + du1%) AND RotoDivN%)): POKE x% + 1, c%: POKE SCRWIDTH% - 2 - x%, c%
            c% = tile%(RotoDivs%((pv% + dv2%) AND RotoDivN%), RotoDivs%((pu% + du2%) AND RotoDivN%)): POKE x% + 2, c%: POKE SCRWIDTH% - 3 - x%, c%
            c% = tile%(RotoDivs%((pv% + dv3%) AND RotoDivN%), RotoDivs%((pu% + du3%) AND RotoDivN%)): POKE x% + 3, c%: POKE SCRWIDTH% - 4 - x%, c%
            c% = tile%(RotoDivs%((pv% + dv4%) AND RotoDivN%), RotoDivs%((pu% + du4%) AND RotoDivN%)): POKE x% + 4, c%: POKE SCRWIDTH% - 5 - x%, c%
            c% = tile%(RotoDivs%((pv% + dv5%) AND RotoDivN%), RotoDivs%((pu% + du5%) AND RotoDivN%)): POKE x% + 5, c%: POKE SCRWIDTH% - 6 - x%, c%
            c% = tile%(RotoDivs%((pv% + dv6%) AND RotoDivN%), RotoDivs%((pu% + du6%) AND RotoDivN%)): POKE x% + 6, c%: POKE SCRWIDTH% - 7 - x%, c%
            c% = tile%(RotoDivs%((pv% + dv7%) AND RotoDivN%), RotoDivs%((pu% + du7%) AND RotoDivN%)): POKE x% + 7, c%: POKE SCRWIDTH% - 8 - x%, c%

            pu% = pu% + du8%
            pv% = pv% + dv8%

        NEXT x%


        u% = u% - dv%
        v% = v% + du%
    NEXT y%

    MirrorVramY

    angle! = angle! + .025

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

