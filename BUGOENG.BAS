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

DECLARE SUB WaitForVsync ()
DECLARE SUB ClearFullBuffer ()
DECLARE SUB ClearBuffer (bufferNum%)
DECLARE SUB WriteFullScreen ()
DECLARE SUB WriteScreen (screenNum%)

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

DIM SHARED screen0%(0 TO 31999)
DIM SHARED screen1%(0 TO 31999)

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


SCREEN 13

ResetElementAddIndices

DIM cubeParams AS ObjGenParams

cubeParams.index = objGenCube%
cubeParams.size = 32
InitObject3D cubeParams, objectCube

SetObjectPosition objectCube, 0, 0, 256

timeStart! = TIMER
nframe& = 0

DO WHILE INKEY$ = ""
    t! = .5 * TIMER

    ClearFullBuffer

    xt% = INT(SIN(2! * t!) * 63!)
    yt% = INT(SIN(3! * t!) * 48!)
    zt% = INT(256 + SIN(4! * t!) * 128!)
    SetObjectPosition objectCube, xt%, yt%, zt%

    TransformObject3D objectCube
    RenderObject3D objectCube

    'WaitForVsync
   
    WriteFullScreen

    nframe& = nframe& + 1
LOOP
timeEnd! = TIMER

PRINT nframe& / (timeEnd! - timeStart!)
SLEEP

SUB AddTriangleIndices (i0%, i1%, i2%)

    objectIndices%(currentIndexAdd%) = i0%
    objectIndices%(currentIndexAdd% + 1) = i1%
    objectIndices%(currentIndexAdd% + 2) = i2%

    currentIndexAdd% = currentIndexAdd% + 3

END SUB

SUB ClearBuffer (bufferNum%) STATIC

    IF (bufferNum% = 0) THEN
        REDIM screen0%(0 TO 31999)
    ELSE
        REDIM screen1%(0 TO 31999)
    END IF

END SUB

SUB ClearFullBuffer

    ClearBuffer 0
    ClearBuffer 1

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

    IF y% < screenHeight% \ 2 THEN
        screenSeg& = VARSEG(screen0%(0))
    ELSE
        screenSeg& = VARSEG(screen1%(0))
        y% = y% - screenHeight% \ 2
    END IF

    DEF SEG = screenSeg& + y% * (screenWidth% \ 8)

    POKE 2 * x%, col%

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

SUB WriteFullScreen
   
    WriteScreen 0
    WriteScreen 1

END SUB

SUB WriteScreen (screenNum%) STATIC

    DEF SEG = &HA000 + screenNum% * 2000

    IF (screenNum% = 0) THEN
        FOR i% = 0 TO 31999
            POKE i%, screen0%(i%)
        NEXT i%
    ELSE
        FOR i% = 0 TO 31999
            POKE i%, screen1%(i%)
        NEXT i%
    END IF

END SUB

