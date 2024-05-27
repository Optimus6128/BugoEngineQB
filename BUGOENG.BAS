DECLARE SUB RenderObject3Ddots (obj AS ANY)
DECLARE SUB RenderObject3Dwire (obj AS ANY)
DECLARE SUB RenderObject3Dpoly (obj AS ANY)
DECLARE SUB AddLineIndices (i0%, i1%)
DECLARE SUB ClearBlurJunk ()
DECLARE SUB BlurVram ()
DECLARE SUB BlurVramX ()
DECLARE SUB BlurVramY ()
DECLARE SUB RunScene3D (t!)
DECLARE SUB InitScene3D ()
DECLARE SUB InitObject3DdotCube (params AS ANY, obj AS ANY)
DECLARE SUB SetDefaultPalette ()
DECLARE SUB InitPrecalcs ()
DECLARE SUB ClearBuffer (bufferNum%)
DECLARE SUB WriteScreen (screenNum%)
DECLARE SUB ClearVram ()
DECLARE SUB WriteVram ()
DECLARE SUB WaitForVsync ()

DECLARE SUB PlotPixel (x%, y%, col%)
DECLARE SUB SetObjectPosition (obj AS ANY, px!, py!, pz!)
DECLARE SUB SetObjectRotation (obj AS ANY, rx!, ry!, rz!)
DECLARE SUB RenderObject3D (obj AS ANY)
DECLARE SUB TransformObject3D (obj AS ANY)
DECLARE SUB AddTriangleIndices (i0%, i1%, i2%)
DECLARE SUB ResetElementAddIndices ()

DECLARE SUB InitObject3Dcube (params AS ANY, obj AS ANY)
DECLARE SUB InitObject3Dsphere (params AS ANY, obj AS ANY)
DECLARE SUB InitObject3Dtorus (params AS ANY, obj AS ANY)
DECLARE SUB InitObject3D (params AS ANY, obj AS ANY)
DECLARE SUB LoadObject3D (objFile$)


CONST SCRWIDTH% = 320
CONST SCRHEIGHT% = 200
CONST SCRWIDTH2% = SCRWIDTH% \ 2
CONST SCRHEIGHT2% = SCRHEIGHT% \ 2

CONST SHADEMAX% = 63

CONST BLURSWITCH% = 0   ' blur will overflow in interpreter only compiled exe


CONST maxTempVertices% = 512
CONST maxVertices% = 2 * maxTempVertices%
CONST maxPolys% = 2 * maxVertices%

CONST objGenCube% = 0
CONST objGenDotCube% = 1
CONST objGenSphere% = 2
CONST objGenTorus% = 3
CONST numObjects% = 4

CONST renderTypeDots% = 0
CONST renderTypeWire% = 1
CONST renderTypePoly% = 2


' CONST PI! = 3.14159265359#
' CONST D2R! = 180! / PI
' CONST ANG256! = (256! * D2R!) / 360!


TYPE Vertex3D
    x AS INTEGER
    y AS INTEGER
    z AS INTEGER
END TYPE

TYPE Vector3D
    x AS SINGLE
    y AS SINGLE
    z AS SINGLE
END TYPE

TYPE PolyData
    col AS INTEGER
    texOffset AS INTEGER
    texWidth AS INTEGER
    texHeight AS INTEGER
END TYPE

TYPE ObjGenParams
    index AS INTEGER
    size AS INTEGER
END TYPE

TYPE Object3D
    verticesOffset AS INTEGER
    polyIndicesOffset AS INTEGER
    lineIndicesOffset AS INTEGER
    polysOffset AS INTEGER

    numVertices AS INTEGER
    numPolyIndices AS INTEGER
    numLineIndices AS INTEGER
    numPolys AS INTEGER

    renderType AS INTEGER

    position AS Vector3D
    rotation AS Vector3D
END TYPE

'$DYNAMIC

DIM SHARED vram%(0 TO 31999)
DIM SHARED shr8%(0 TO &H7F7F)

'$STATIC

DIM SHARED screenVertices(0 TO maxTempVertices%) AS Vertex3D

DIM SHARED objectVertices(0 TO maxVertices%) AS Vertex3D
DIM SHARED objectPolyIndices%(0 TO 3 * maxPolys%)
DIM SHARED objectLineIndices%(0 TO 6 * maxPolys%)
DIM SHARED objectPolygons(0 TO maxPolys%) AS PolyData

DIM SHARED currentVertexAdd%
DIM SHARED currentPolyIndexAdd%
DIM SHARED currentLineIndexAdd%
DIM SHARED currentPolyAdd%

DIM SHARED object(0 TO numObjects% - 1) AS Object3D
DIM SHARED currentObject%

currentObject% = objGenCube%


InitPrecalcs

SCREEN 13

SetDefaultPalette

ResetElementAddIndices
InitScene3D


' ---------------------------------------

freeMem& = FRE(-1)

timeStart! = TIMER
nframe& = 0

t! = 0
DO WHILE INKEY$ = ""
    'dt! = TIMER - timeStart!

    IF BLURSWITCH% = 0 THEN ClearVram

    RunScene3D (t!)

    IF BLURSWITCH% = 1 THEN
        BlurVram
    END IF

    'WaitForVsync

    WriteVram

    'PRINT freeMem&

    t! = t! + .01
    nframe& = nframe& + 1
LOOP
timeEnd! = TIMER

COLOR 31
PRINT nframe& / (timeEnd! - timeStart!)
SLEEP

SUB AddLineIndices (i0%, i1%)

    objectLineIndices%(currentLineIndexAdd%) = i0%
    objectLineIndices%(currentLineIndexAdd% + 1) = i1%

    currentLineIndexAdd% = currentLineIndexAdd% + 2

END SUB

SUB AddTriangleIndices (i0%, i1%, i2%)

    objectPolyIndices%(currentPolyIndexAdd%) = i0%
    objectPolyIndices%(currentPolyIndexAdd% + 1) = i1%
    objectPolyIndices%(currentPolyIndexAdd% + 2) = i2%

    currentPolyIndexAdd% = currentPolyIndexAdd% + 3

END SUB

SUB BlurVram

    ' Dirty blur two pixels at once (works only if colors are from 0 to 63)
    FOR i% = SCRWIDTH2% TO SCRWIDTH2% * (SCRHEIGHT% - 1) - 1
        vram%(i%) = ((vram%(i% - 1) + vram%(i% + 1) + vram%(i% - SCRWIDTH2%) + vram%(i% + SCRWIDTH2%)) \ 4) AND &H3F3F
    NEXT i%

    ClearBlurJunk

END SUB

SUB BlurVramX

    ' Dirty blur two pixels at once (works only if colors are from 0 to 127)
    FOR i% = SCRWIDTH% \ 2 TO (SCRWIDTH% \ 2) * (SCRHEIGHT% - 1) - 1
        vram%(i%) = ((vram%(i% - 1) + vram%(i% + 1)) \ 2) AND &H7F7F
    NEXT i%

    ClearBlurJunk

END SUB

SUB BlurVramY

    ' Dirty blur two pixels at once (works only if colors are from 0 to 127)
    FOR i% = SCRWIDTH2% TO SCRWIDTH2% * (SCRHEIGHT% - 1) - 1
        vram%(i%) = ((vram%(i% - SCRWIDTH2%) + vram%(i% + SCRWIDTH2%)) \ 2) AND &H7F7F
    NEXT i%

    ClearBlurJunk

END SUB

SUB ClearBlurJunk

    ' Clear junk at top scanline
    FOR i% = 0 TO SCRWIDTH2% - 1
        vram%(i%) = 0
    NEXT i%

    ' Clear junk at bottom scanline
    FOR i% = SCRWIDTH2% * (SCRHEIGHT% - 1) TO SCRWIDTH2% * SCRHEIGHT% - 1
        vram%(i%) = 0
    NEXT i%

END SUB

SUB ClearVram

    REDIM vram%(0 TO 31999)

END SUB

SUB InitObject3D (params AS ObjGenParams, obj AS Object3D)

    obj.verticesOffset = currentVertexAdd%
    obj.polyIndicesOffset = currentPolyIndexAdd%
    obj.lineIndicesOffset = currentLineIndexAdd%
    obj.polysOffset = currentPolyAdd%

    SELECT CASE params.index
       
        CASE objGenCube%
            InitObject3Dcube params, obj

        CASE objGenDotCube%
            InitObject3DdotCube params, obj

        CASE objGenSphere%
            InitObject3Dsphere params, obj

        CASE objGenTorus%
            InitObject3Dtorus params, obj

        CASE ELSE
            PRINT "Object Index doesn't exist.."
    END SELECT

END SUB

SUB InitObject3Dcube (params AS ObjGenParams, obj AS Object3D)

    obj.numVertices = 8
    obj.numPolys = 12
    obj.numPolyIndices = obj.numPolys * 3
    obj.numLineIndices = 2 * 12

    halfSize% = params.size

    FOR z% = -1 TO 1 STEP 2
        FOR y% = -1 TO 1 STEP 2
            FOR x% = -1 TO 1 STEP 2
                objectVertices(currentVertexAdd%).x = x% * halfSize%
                objectVertices(currentVertexAdd%).y = y% * halfSize%
                objectVertices(currentVertexAdd%).z = z% * halfSize%
                currentVertexAdd% = currentVertexAdd% + 1
            NEXT x%
        NEXT y%
    NEXT z%

    AddTriangleIndices 0, 3, 2
    AddTriangleIndices 0, 1, 3
    AddTriangleIndices 1, 7, 3
    AddTriangleIndices 1, 5, 7
    AddTriangleIndices 5, 6, 7
    AddTriangleIndices 5, 4, 6
    AddTriangleIndices 4, 2, 6
    AddTriangleIndices 4, 0, 2
    AddTriangleIndices 2, 7, 6
    AddTriangleIndices 2, 3, 7
    AddTriangleIndices 4, 1, 0
    AddTriangleIndices 4, 5, 1

    AddLineIndices 0, 1
    AddLineIndices 1, 3
    AddLineIndices 3, 2
    AddLineIndices 2, 0
   
    AddLineIndices 4, 5
    AddLineIndices 5, 7
    AddLineIndices 7, 6
    AddLineIndices 6, 4
   
    AddLineIndices 1, 5
    AddLineIndices 2, 6
    AddLineIndices 0, 4
    AddLineIndices 3, 7

END SUB

SUB InitObject3DdotCube (params AS ObjGenParams, object AS Object3D)

    object.numVertices = 512
    object.numPolys = 0
    object.numPolyIndices = 0
    object.numLineIndices = 0

    halfSize% = params.size

    FOR z% = -4 TO 3
        FOR y% = -4 TO 3
            FOR x% = -4 TO 3
                objectVertices(currentVertexAdd%).x = x% * halfSize%
                objectVertices(currentVertexAdd%).y = y% * halfSize%
                objectVertices(currentVertexAdd%).z = z% * halfSize%
                currentVertexAdd% = currentVertexAdd% + 1
            NEXT x%
        NEXT y%
    NEXT z%

END SUB

SUB InitObject3Dsphere (params AS ObjGenParams, obj AS Object3D)

END SUB

SUB InitObject3Dtorus (params AS ObjGenParams, obj AS Object3D)

END SUB

SUB InitPrecalcs


' LUT to get high byte of 16bit word where each byte is from 0 to 127 only
' This is to avoid the division on the current WriteVram method.
' Bigger LUT for 256 didn't fit. Might discard later if replacing the screen copy with assembly.

FOR i% = 0 TO &H7F7F
    shr8%(i%) = i% \ 256
NEXT i%

END SUB

SUB InitScene3D

    DIM cubeParams AS ObjGenParams
   
    cubeParams.index = objGenCube%
    cubeParams.size = 32
    InitObject3D cubeParams, object(objGenCube%)
    object(objGenCube%).renderType = renderTypeWire%

    DIM dotCubeParams AS ObjGenParams

    dotCubeParams.index = objGenDotCube%
    dotCubeParams.size = 24
    InitObject3D dotCubeParams, object(objGenDotCube%)

END SUB

SUB LoadObject3D (objFile$)

END SUB

SUB PlotPixel (x%, y%, col%)

    screenSeg& = VARSEG(vram%(0))
    DEF SEG = screenSeg& + y% * (SCRWIDTH% \ 16)
    POKE x%, col%

END SUB

SUB RenderObject3D (obj AS Object3D)

    SELECT CASE obj.renderType

        CASE renderTypeDots%
            RenderObject3Ddots obj

        CASE renderTypeWire%
            RenderObject3Dwire obj

        CASE renderTypePoly%
            RenderObject3Dpoly obj

        CASE ELSE
            PRINT "Render Type doesn't exist.."
    END SELECT

END SUB

SUB RenderObject3Ddots (obj AS Object3D)

    n% = obj.numVertices - 1

    FOR i% = 0 TO n%
      
        z% = screenVertices(i%).z

        IF z% > 0 THEN
            x% = screenVertices(i%).x
            y% = screenVertices(i%).y

            IF x% >= 0 AND x% < SCRWIDTH% AND y% >= 0 AND y% < SCRHEIGHT% THEN
                'c% = (512 - z%) \ 8
                'IF c% < 0 THEN c% = 0
                'IF c% > SHADEMAX% THEN c% = SHADEMAX%

                PlotPixel x%, y%, SHADEMAX%
            END IF
        END IF

    NEXT i%

END SUB

SUB RenderObject3Dpoly (obj AS Object3D)

END SUB

SUB RenderObject3Dwire (obj AS Object3D)

    CONST pointsBetween% = 32

    offset% = obj.lineIndicesOffset
    n% = offset% + obj.numLineIndices - 1

    FOR i% = offset% TO n% STEP 2

        i0% = objectLineIndices%(i%)
        i1% = objectLineIndices%(i% + 1)

        z0% = screenVertices(i0%).z
        z1% = screenVertices(i1%).z

        IF z0% > 0 AND z1% > 0 THEN
            x0% = screenVertices(i0%).x
            y0% = screenVertices(i0%).y
            x1% = screenVertices(i1%).x
            y1% = screenVertices(i1%).y

            ' Obviously I am gonna write a line renderer soon
            ' Now just testing the wireframe data with naive inbetweener
            FOR t% = 0 TO pointsBetween%
                x% = (x0% * t% + x1% * (pointsBetween% - t%)) \ pointsBetween%
                y% = (y0% * t% + y1% * (pointsBetween% - t%)) \ pointsBetween%
                IF x% >= 0 AND x% < SCRWIDTH% AND y% >= 0 AND y% < SCRHEIGHT% THEN
                    PlotPixel x%, y%, SHADEMAX%
                END IF
            NEXT t%
        END IF

    NEXT i%

END SUB

SUB ResetElementAddIndices

    currentVertexAdd% = 0
    currentPolyIndexAdd% = 0
    currentLineIndexAdd% = 0
    currentPolyAdd% = 0

END SUB

SUB RunScene3D (t!)

    xt! = SIN(t!) * 48
    yt! = SIN(1.5 * t!) * 32
    zt! = 256 + SIN(1.25 * t!) * 96
   
    SetObjectPosition object(currentObject%), xt!, yt!, zt!
    SetObjectRotation object(currentObject%), .7 * t!, t!, 1.25 * t!

    TransformObject3D object(currentObject%)
    RenderObject3D object(currentObject%)

END SUB

SUB SetDefaultPalette

OUT &H3C8, 0
FOR i% = 0 TO 127
    OUT &H3C9, i%
    OUT &H3C9, i%
    OUT &H3C9, i%
NEXT i%

END SUB

SUB SetObjectPosition (obj AS Object3D, px!, py!, pz!)

    obj.position.x = px!
    obj.position.y = py!
    obj.position.z = pz!

END SUB

SUB SetObjectRotation (obj AS Object3D, rx!, ry!, rz!)

    obj.rotation.x = rx!
    obj.rotation.y = ry!
    obj.rotation.z = rz!

END SUB

SUB TransformObject3D (obj AS Object3D)

    CONST proj! = 256

    posX! = obj.position.x
    posY! = obj.position.y
    posZ! = obj.position.z

    rotX! = obj.rotation.x
    rotY! = obj.rotation.y
    rotZ! = obj.rotation.z

    cosxr! = COS(rotX!)
    cosyr! = COS(rotY!)
    coszr! = COS(rotZ!)
    sinxr! = SIN(rotX!)
    sinyr! = SIN(rotY!)
    sinzr! = SIN(rotZ!)

    ' Matrix values
    m0! = cosyr! * coszr!
    m1! = sinxr! * sinyr! * coszr! - cosxr! * sinzr!
    m2! = cosxr! * sinyr! * coszr! + sinxr! * sinzr!
    m3! = cosyr! * sinzr!
    m4! = cosxr! * coszr! + sinxr! * sinyr! * sinzr!
    m5! = -sinxr! * coszr! + cosxr! * sinyr! * sinzr!
    m6! = -sinyr!
    m7! = sinxr! * cosyr!
    m8! = cosxr! * cosyr!


    offset% = obj.verticesOffset
    n% = obj.numVertices - 1

    FOR i% = 0 TO n%

        k% = i% + offset%
        xo! = objectVertices(k%).x
        yo! = objectVertices(k%).y
        zo! = objectVertices(k%).z

        x! = xo! * m0! + yo! * m3! + zo! * m6!
        y! = xo! * m1! + yo! * m4! + zo! * m7!
        z! = xo! * m2! + yo! * m5! + zo! * m8!

        z! = z! + posZ!
        screenVertices(i%).z = INT(z!)
        IF z! > 0 THEN
            screenVertices(i%).x = SCRWIDTH2% + INT(((x! + posX!) * proj!) \ z!)
            screenVertices(i%).y = SCRHEIGHT2% - INT(((y! + posY!) * proj!) \ z!)
        END IF

    NEXT i%


END SUB

SUB WaitForVsync

    WAIT &H3DA, 8
    WAIT &H3DA, 8, 8

END SUB

SUB WriteVram

    i% = 0
    FOR y% = 0 TO 199
        DEF SEG = &HA000 + y% * (SCRWIDTH% \ 16)
        FOR x% = 0 TO 319 STEP 16
            c% = vram%(i%): POKE x%, c% AND 255: POKE x% + 1, shr8%(c%)
            c% = vram%(i% + 1): POKE x% + 2, c% AND 255: POKE x% + 3, shr8%(c%)
            c% = vram%(i% + 2): POKE x% + 4, c% AND 255: POKE x% + 5, shr8%(c%)
            c% = vram%(i% + 3): POKE x% + 6, c% AND 255: POKE x% + 7, shr8%(c%)
            c% = vram%(i% + 4): POKE x% + 8, c% AND 255: POKE x% + 9, shr8%(c%)
            c% = vram%(i% + 5): POKE x% + 10, c% AND 255: POKE x% + 11, shr8%(c%)
            c% = vram%(i% + 6): POKE x% + 12, c% AND 255: POKE x% + 13, shr8%(c%)
            c% = vram%(i% + 7): POKE x% + 14, c% AND 255: POKE x% + 15, shr8%(c%)
            i% = i% + 8
        NEXT x%
    NEXT y%

END SUB

