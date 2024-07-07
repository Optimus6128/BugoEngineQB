#lang "fblite"

DECLARE SUB RenderLightToBuffer (xp%, yp%, buff%())
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

DECLARE SUB MirrorVramY ()

DECLARE SUB HandleInput (dt!)


' ---- FX functions  ----

DECLARE SUB InitFxPlasma ()
DECLARE SUB InitFxRotozoomer ()
DECLARE SUB InitFxFloor ()
DECLARE SUB InitFxSphere ()
DECLARE SUB InitFxBump ()

DECLARE SUB RunFxPlasma ()
DECLARE SUB RunFxRotozoomer ()
DECLARE SUB RunFxFloor ()
DECLARE SUB RunFxBump ()
DECLARE SUB RunFxSphere ()

DECLARE SUB RunFx (fxType%)

DECLARE SUB RotatePoint2D (x&, y&, angle!)
DECLARE SUB CalcLineStepVars (moveX&, moveY&, angle!)

DECLARE SUB InitBumpSurface (buff%(), tempBuff%(), buffIndex%)
DECLARE SUB InitBumpLight ()

' ---- Main arrays ----

'$DYNAMIC

DIM SHARED vram%(0 TO 32007)
vram%(6) = 320 * 8
vram%(7) = 200

DIM SHARED buff0%(0 TO 31999)
DIM SHARED buff1%(0 TO 31999)
DIM SHARED buff2%(0 TO 31999)
'DIM SHARED buff3%(0 TO 31999)

dim shared pixel as byte ptr

'$STATIC

DIM SHARED quit%
DIM SHARED vsync%
quit% = FALSE%
vsync% = FALSE%

DIM SHARED nframe&


' ---- Fx arrays ----

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

DIM SHARED fsin1%(0 TO 255)
DIM SHARED fsin2%(0 TO 255)
DIM SHARED fsin3%(0 TO 255)

CONST bumpLightW% = 128
CONST bumpLightH% = 128
CONST bumpLightSize% = bumpLightW% * bumpLightH%

DIM SHARED bumpLight%(0 TO bumpLightSize% - 1)


' ---- Fx index ----

CONST fxPlasma% = 0
CONST fxRotozoomer% = 1
CONST fxFloor% = 2
CONST fxBump% = 3
CONST fxSphere% = 4
CONST fxVoxel% = 5
CONST fxWater% = 6

fxCurrent% = fxBump%


' ---- Init ----

' Load FFIX to memory
'FFIX 0

InitPrecalcs fxCurrent%

SCREEN 13,8,2

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

    'screenlock
    
    pixel = screenptr

    RunFx fxCurrent%

    'screenunlock
    'screensync

    'WriteVram

    flp% = (flp% + 1) AND 1
    SCREENSET flp%, ((flp% + 1) AND 1)

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

        u& = u& - dv&
        v& = v& + du&
    NEXT y%

END SUB

SUB ClearVram

    REDIM vram%(0 TO 32007)

    vram%(6) = 320 * 8
    vram%(7) = 200

END SUB

SUB DisplayPalette

    pixel = screenptr
    FOR y% = 0 TO 55
        'DEF SEG = &HA000 + y% * 20
        yp% =y% * 320
        FOR x% = 0 TO 256
            IF y% < 50 THEN
                'POKE x%, x%
                pixel[yp%+x%] = x%
            ELSE
                IF (x% AND 63) = 0 THEN pixel[yp%+x%] = 127
            END IF
        NEXT x%
    NEXT y%

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

SUB InitBumpLight

    i% = 0
    FOR y% = 0 TO bumpLightH% - 1
        yp% = y% - bumpLightH% \ 2
        FOR x% = 0 TO bumpLightW% - 1
            xp% = x% - bumpLightW% \ 2

            r& = xp% * xp% + yp% * yp%
            'r& = r& * r&
            IF r& = 0 THEN r& = 1
            'c& = (8 * 16777216) \ r&
            c& = 131072 \ r&
            IF c& > 255 THEN c& = 255
            bumpLight%(i%) = c&
            i% = i% + 1

        NEXT x%
    NEXT y%

END SUB

SUB InitBumpSurface (buff%(), tempBuff%(), buffIndex%)

    CONST bScale% = 256

    FOR i% = 0 TO 31999
        buff%(i%) = INT(RND * bScale%)
    NEXT i%

    FOR j% = 0 TO 1
        FOR i% = 0 TO 31999
            iL% = (i% - 1) MOD 32000: IF iL% < 0 THEN iL% = iL% + 32000
            iR% = (i% + 1) MOD 32000
            iU% = (i% - 320) MOD 32000: IF iU% < 0 THEN iU% = iU% + 32000
            iD% = (i% + 320) MOD 32000

            c% = (buff%(iL%) + buff%(iR%) + buff%(iU%) + buff%(iD%)) \ 4
            buff%(i%) = c%
        NEXT i%
    NEXT j%

    i% = 0
    FOR y% = 0 TO SCRHEIGHT% \ 2 - 1
        yi% = (y% \ 2) * SCRWIDTH%
        FOR x% = 0 TO SCRWIDTH% - 1
            iR% = (i% + 1) MOD 32000
            iD% = (i% + 320) MOD 32000

            dx% = buff%(iR%) - buff%(i%): dy% = buff%(iD%) - buff%(i%)
            diOff& = buffIndex% * 16000
            di& = (diOff& + yi% + (dy% \ 2) * SCRWIDTH% + x% + dx%) MOD 32000
            IF di& < 0 THEN di& = di& + 32000

            tempBuff%(i%) = di&
            i% = i% + 1
        NEXT x%
    NEXT y%

    FOR i% = 0 TO 31999
        buff%(i%) = tempBuff%(i%)
    NEXT i%

END SUB

SUB InitFxBump

    InitBumpSurface buff0%(), buff2%(), 0
    InitBumpSurface buff1%(), buff2%(), 1

    InitBumpLight

    REDIM buff2%(0 TO 31999)

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

SUB InitFxSphere

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
            SetGradPal 0, 127, 0, 0, 0, 31, 27, 15
            SetGradPal 128, 255, 31, 27, 15, 47, 44, 40

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
            InitFxBump

        CASE fxSphere%
            InitFxSphere

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

SUB RenderLightToBuffer (xp%, yp%, buff%())

    i% = 0
    FOR y% = 0 TO bumpLightH% - 1
        yi% = y% + yp%
        IF yi% >= 0 AND yi% < SCRHEIGHT% THEN
            yi% = (yi% \ 2) * SCRWIDTH%
            FOR x% = 0 TO bumpLightW% - 1 STEP 8
                buff%(yi% + x% + xp%) = bumpLight%(i%)
                buff%(yi% + x% + xp% + 1) = bumpLight%(i% + 1)
                buff%(yi% + x% + xp% + 2) = bumpLight%(i% + 2)
                buff%(yi% + x% + xp% + 3) = bumpLight%(i% + 3)
                buff%(yi% + x% + xp% + 4) = bumpLight%(i% + 4)
                buff%(yi% + x% + xp% + 5) = bumpLight%(i% + 5)
                buff%(yi% + x% + xp% + 6) = bumpLight%(i% + 6)
                buff%(yi% + x% + xp% + 7) = bumpLight%(i% + 7)
                i% = i% + 8
            NEXT x%
        END IF
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
            RunFxBump

        CASE fxSphere%
            RunFxSphere

        CASE fxVoxel%

        CASE fxWater%

        CASE ELSE

    END SELECT

END SUB

SUB RunFxBump

    ' Dirty rectangles will do for now (commenting out below line)
    'REDIM buff2%(0 TO 31999)

    t% = (nframe& \ 64) AND 32767
    px% = SCRWIDTH% \ 2 - bumpLightW% \ 2 + SIN(t% / 32) * 64
    py% = SCRHEIGHT% \ 2 - bumpLightH% \ 2 + SIN(t% / 40) * 24

    RenderLightToBuffer px%, py%, buff2%()

    'screenSeg& = VARSEG(vram%(0)) + 1
    'pixel = screenptr

    'DEF SEG = screenSeg&
    FOR i% = 0 TO 31999 STEP 1
        pixel[i%] = buff2%(buff0%(i%))
    NEXT i%

    'DEF SEG = screenSeg& + 100 * 20
    FOR i% = 0 TO 31999 STEP 1
        pixel[32000+i%] = buff2%(buff1%(i%))
    NEXT i%

END SUB

SUB RunFxFloor

    STATIC angle!
    STATIC posX&, posY&

    CalcLineStepVars posX&, posY&, angle!

    'screenSeg& = VARSEG(vram%(0)) + 1
    'pixel = screenptr

    FOR y% = 0 TO SCRHEIGHT% \ 2 - 1
        'DEF SEG = screenSeg& + y% * 20

        u% = lineStepVars(y%).u
        v% = lineStepVars(y%).v
        du% = lineStepVars(y%).du
        dv% = lineStepVars(y%).dv
        shadePal% = lineStepVars(y%).shade * 16

        yi% = y% * 320
        FOR x% = 0 TO SCRWIDTH% - 1 STEP 1

        pixel[yi%+x%] = tile%(RotoDivs%(v% AND RotoDivN%), RotoDivs%(u% AND RotoDivN%)) + shadePal%
        u% = u% + du%
        v% = v% + dv%

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

    u% = -160 * du%
    v% = -100 * dv%

    'screenSeg& = VARSEG(vram%(0)) + 1
    'pixel = screenptr

    FOR y% = 0 TO SCRHEIGHT% \ 2 - 1
        'DEF SEG = screenSeg& + y% * 20
        yi% = y% * 320

        pu% = u%
        pv% = v%

        FOR x% = 0 TO SCRWIDTH% \ 2 - 1 STEP 1

            c% = tile%(RotoDivs%(pv% AND RotoDivN%), RotoDivs%(pu% AND RotoDivN%)): pixel[x%+yi%] = c%: pixel[yi%+ SCRWIDTH% - 1 - x%] = c%

            pu% = pu% + du%
            pv% = pv% + dv%

        NEXT x%


        u% = u% - dv%
        v% = v% + du%
    NEXT y%

    MirrorVramY

    angle! = angle! + .025

END SUB

SUB RunFxSphere

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

