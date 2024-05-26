DECLARE SUB ClearBuffer (bufferNum%)
DECLARE SUB WriteScreen (screenNum%)
DECLARE SUB ClearVram ()
DECLARE SUB WriteVram ()
DECLARE SUB WaitForVsync ()

DECLARE SUB PlotPixel (x%, y%, col%)
DECLARE SUB SetObjectPosition (object AS ANY, px%, py%, pz%)
DECLARE SUB SetObjectRotation (object AS ANY, rx%, ry%, rz%)
DECLARE SUB RenderObject3D (object AS ANY)
DECLARE SUB TransformObject3D (object AS ANY)
DECLARE SUB AddTriangleIndices (i0%, i1%, i2%)
DECLARE SUB ResetElementAddIndices ()

DECLARE SUB InitObject3Dcube (params AS ANY, object AS ANY)
DECLARE SUB InitObject3Dsphere (params AS ANY, object AS ANY)
DECLARE SUB InitObject3Dtorus (params AS ANY, object AS ANY)
DECLARE SUB InitObject3D (params AS ANY, object AS ANY)
DECLARE SUB LoadObject3D (objFile$)


CONST screenWidth% = 320
CONST screenHeight% = 200

CONST maxTempVertices% = 256
CONST maxVertices% = 1024
CONST maxPolys% = 2048

CONST objGenCube% = 0
CONST objGenSphere% = 1
CONST objGenTorus% = 2


TYPE Vertex3D
    x AS INTEGER
    y AS INTEGER
    z AS INTEGER
END TYPE

TYPE Vector3D
    x AS INTEGER
    y AS INTEGER
    z AS INTEGER
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
    indicesOffset AS INTEGER
    polysOffset AS INTEGER

    numVertices AS INTEGER
    numIndices AS INTEGER
    numPolys AS INTEGER

    position AS Vector3D
    rotation AS Vector3D
END TYPE

'$DYNAMIC

DIM SHARED vram%(0 TO 31999)

DIM SHARED shr8%(0 TO &H7F7F)

'$STATIC
DIM SHARED transformedVertices(0 TO maxTempVertices%) AS Vertex3D

DIM SHARED objectVertices(0 TO maxVertices%) AS Vertex3D
DIM SHARED objectIndices%(0 TO 3 * maxPolys%)
DIM SHARED objectPolygons(0 TO maxPolys%) AS PolyData

DIM SHARED currentVertexAdd%
DIM SHARED currentIndexAdd%
DIM SHARED currentPolyAdd%

DIM objectCube AS Object3D
DIM objectSphere AS Object3D
DIM objectTorus AS Object3D

FOR i% = 0 TO &H7F7F
    shr8%(i%) = i% \ 256
NEXT i%

SCREEN 13

ResetElementAddIndices

DIM cubeParams AS ObjGenParams

cubeParams.index = objGenCube%
cubeParams.size = 32
InitObject3D cubeParams, objectCube

SetObjectPosition objectCube, 0, 0, 256

timeStart = TIMER
nframe& = 0

t = 0
DO WHILE INKEY$ = ""
    't = .5 * TIMER
    t = t + .01

    ClearVram

    xt% = SIN(2 * t) * 63
    yt% = SIN(3 * t) * 48
    zt% = 256 + SIN(4 * t) * 128
    SetObjectPosition objectCube, xt%, yt%, zt%

    TransformObject3D objectCube
    RenderObject3D objectCube

    'WaitForVsync
   
    WriteVram

    nframe& = nframe& + 1
LOOP
timeEnd = TIMER

PRINT nframe& / (timeEnd - timeStart)
SLEEP

SUB AddTriangleIndices (i0%, i1%, i2%)

    objectIndices%(currentIndexAdd%) = i0%
    objectIndices%(currentIndexAdd% + 1) = i1%
    objectIndices%(currentIndexAdd% + 2) = i2%

    currentIndexAdd% = currentIndexAdd% + 3

END SUB

SUB ClearVram

    REDIM vram%(0 TO 31999)

END SUB

SUB InitObject3D (params AS ObjGenParams, object AS Object3D)

    object.verticesOffset = currentVertexAdd%
    object.indicesOffset = currentIndexAdd%
    object.polysOffset = currentPolyAdd%

    SELECT CASE params.index
       
        CASE objGenCube%
            InitObject3Dcube params, object

        CASE objGenSphere%
            InitObject3Dsphere params, object

        CASE objGenTorus%
            InitObject3Dtorus params, object

        CASE ELSE
            PRINT "Object Index doesn't exist.."
    END SELECT

END SUB

SUB InitObject3Dcube (params AS ObjGenParams, object AS Object3D)

    object.numVertices = 8
    object.numPolys = 12
    object.numIndices = object.numPolys * 3

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

END SUB

SUB InitObject3Dsphere (params AS ObjGenParams, object AS Object3D)

END SUB

SUB InitObject3Dtorus (params AS ObjGenParams, object AS Object3D)

END SUB

SUB LoadObject3D (objFile$)

END SUB

SUB PlotPixel (x%, y%, col%)

    screenSeg& = VARSEG(vram%(0))
    DEF SEG = screenSeg& + y% * (screenWidth% \ 16)
    POKE x%, col%

END SUB

SUB RenderObject3D (object AS Object3D)

    n% = object.numVertices - 1

    FOR i% = 0 TO n%
       
        z% = transformedVertices(i%).z

        IF z% > 0 THEN
            x% = transformedVertices(i%).x
            y% = transformedVertices(i%).y

            IF (x% >= 0 AND x% < screenWidth% AND y% >= 0 AND y% < screenHeight%) THEN
                PlotPixel x%, y%, 15
            END IF
        END IF

    NEXT i%

END SUB

SUB ResetElementAddIndices

    currentVertexAdd% = 0
    currentIndexAdd% = 0
    currentPolyAdd% = 0

END SUB

SUB SetObjectPosition (object AS Object3D, px%, py%, pz%)

    object.position.x = px%
    object.position.y = py%
    object.position.z = pz%

END SUB

SUB SetObjectRotation (object AS Object3D, rx%, ry%, rz%)

    object.rotation.x = rx%
    object.rotation.y = ry%
    object.rotation.z = rz%

END SUB

SUB TransformObject3D (object AS Object3D)

    CONST proj% = 256

    posX% = object.position.x
    posY% = object.position.y
    posZ% = object.position.z

    offset% = object.verticesOffset
    n% = object.numVertices - 1

    FOR i% = 0 TO n%

        k% = i% + offset%

        z% = objectVertices(k%).z + posZ%
        transformedVertices(i%).z = z%

        IF z% > 0 THEN
            x% = objectVertices(k%).x + posX%
            y% = objectVertices(k%).y + posY%

            transformedVertices(i%).x = screenWidth% / 2 + (x% * proj%) \ z%
            transformedVertices(i%).y = screenHeight% / 2 + (y% * proj%) \ z%
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
        DEF SEG = &HA000 + y% * (screenWidth% \ 16)
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

