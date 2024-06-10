DECLARE SUB ReadLineStringFromPlg (fileHandle%, bufDst$)
DECLARE SUB FlipPolygonOrientation (obj AS ANY)
DECLARE SUB GenObject3Dcube (params AS ANY, obj AS ANY)
DECLARE SUB GenObject3DdotCube (params AS ANY, object AS ANY)
DECLARE SUB GenObject3Dsphere (params AS ANY, obj AS ANY)
DECLARE SUB GenObject3Dtorus (params AS ANY, obj AS ANY)
DECLARE SUB GenObject3D (params AS ANY, obj AS ANY)
DECLARE SUB FindObjectScaleToFit (obj AS ANY, desiredRange!, scale!)
DECLARE SUB CalcObjectVerticesRange (obj AS ANY, range AS ANY)
DECLARE SUB ScaleObjectVertices (obj AS ANY, scale!)
DECLARE SUB DrawPolyAlt (i%, polyIndex%)
DECLARE SUB DrawPoly (i%, polyIndex%)
DECLARE SUB SetCurrentObject (objNum%, mustGen%, mustLoad%)
DECLARE SUB ReadLineValues (inp$, out$(), numVals%)
DECLARE SUB DrawEdgesArr (col%)
DECLARE SUB WriteVram ()
DECLARE SUB PrepareEdgeList (x0%, y0%, x1%, y1%)
DECLARE SUB DrawEdges (col%)
DECLARE SUB RenderObject3Ddots (obj AS ANY)
DECLARE SUB RenderObject3Dwire (obj AS ANY)
DECLARE SUB RenderObject3Dpoly (obj AS ANY)
DECLARE SUB SortObject3Dpoly (obj AS ANY)
DECLARE SUB CenterObjectVertices (obj AS ANY)
DECLARE SUB AddLineIndices (i0%, i1%)
DECLARE SUB ClearBlurJunk ()
DECLARE SUB BlurVram ()
DECLARE SUB BlurVramX ()
DECLARE SUB BlurVramY ()
DECLARE SUB RunScene3D (t!)
DECLARE SUB InitScene3D ()
DECLARE SUB SetDefaultPalette ()
DECLARE SUB InitPrecalcs ()
DECLARE SUB ClearBuffer (bufferNum%)
DECLARE SUB WriteScreen (screenNum%)
DECLARE SUB ClearVram ()
DECLARE SUB WaitForVsync ()

DECLARE SUB PlotPixel (x%, y%, col%)
DECLARE SUB SetObjectPosition (obj AS ANY, px!, py!, pz!)
DECLARE SUB SetObjectRotation (obj AS ANY, rx!, ry!, rz!)
DECLARE SUB RenderObject3D (obj AS ANY)
DECLARE SUB TransformObject3D (obj AS ANY)
DECLARE SUB AddVertex (x%, y%, z%)
DECLARE SUB AddTriangleIndices (i0%, i1%, i2%)
DECLARE SUB ResetElementAddIndices ()

DECLARE SUB InitObject3Dcube (params AS ANY, obj AS ANY)
DECLARE SUB InitObject3Dsphere (params AS ANY, obj AS ANY)
DECLARE SUB InitObject3Dtorus (params AS ANY, obj AS ANY)
DECLARE SUB InitObject3D (params AS ANY, obj AS ANY)
DECLARE SUB LoadObject3D (objFile$, obj AS ANY)


' ---- Common Consts ----

CONST SCRWIDTH% = 320
CONST SCRHEIGHT% = 200
CONST SCRWIDTH2% = SCRWIDTH% \ 2
CONST SCRHEIGHT2% = SCRHEIGHT% \ 2

CONST FALSE% = 0
CONST TRUE% = 1

CONST PI! = 3.14159265359#


' ---- Engine Stuff ----

CONST SHADEMAX% = 63

CONST BLURSWITCH% = 0   ' blur will overflow in interpreter only compiled exe

CONST ALTPOLY% = 0      ' 1 is slow check if pixel is inside triangle
CONST FPRAST% = 64

CONST ZBUCKETSIZE% = 16
CONST ZBUCKETMAX% = 2048
CONST ZBUCKETS% = ZBUCKETMAX% / ZBUCKETSIZE%
CONST ZBUCKETPOLYS% = 64
CONST LEFTOVERSSIZE% = 512

DIM SHARED zBucket%(0 TO ZBUCKETS%, 0 TO ZBUCKETPOLYS%)
' Value 0 in ZBUCKETPOLYS% will really be the counter of num polys in list or next poly to add
DIM SHARED zBucketLeftovers%(0 TO LEFTOVERSSIZE%)
' Polys that don't fit in buckets anymore or over ZBUCKETMAX%
' Value 0 in this array will be again the current count

DIM SHARED minZbucket%
DIM SHARED maxZbucket%

CONST maxTempVertices% = 768
CONST maxVertices% = 2 * maxTempVertices%
CONST maxPolys% = 2 * maxVertices%


' ---- Object Selector ----

DIM SHARED currentObject%
DIM SHARED objectMustLoad%
DIM SHARED objectMustGen%

CONST objGenCube% = 0
CONST objGenDotCube% = 1
CONST objGenSphere% = 2
CONST objGenTorus% = 3
CONST objLoadTest% = 4
CONST objLoadTeapot% = 5
CONST objLoadMask% = 6
CONST numObjects% = 7

CONST renderTypeDots% = 0
CONST renderTypeWire% = 1
CONST renderTypePoly% = 2

SetCurrentObject objLoadTest%, FALSE%, TRUE%
'SetCurrentObject objGenDotCube%, TRUE%, FALSE%


' ---- Engine Data Types ----

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

TYPE Edge
    x AS INTEGER
END TYPE

TYPE Range3D
    min AS Vector3D
    max AS Vector3D
END TYPE

' ---- Main Arrays ----

'$DYNAMIC

DIM SHARED vram%(0 TO 32001)
vram%(0) = 320 * 8
vram%(1) = 200

'$STATIC

DIM SHARED segY%(0 TO SCRHEIGHT% - 1)

DIM SHARED object(0 TO numObjects% - 1) AS Object3D

DIM SHARED screenVertices(0 TO maxTempVertices%) AS Vertex3D

DIM SHARED objectVertices(0 TO maxVertices%) AS Vertex3D
DIM SHARED objectPolyIndices%(0 TO 3 * maxPolys%)
DIM SHARED objectLineIndices%(0 TO 6 * maxPolys%)
DIM SHARED objectPolygons(0 TO maxPolys%) AS PolyData

DIM SHARED currentVertexAdd%
DIM SHARED currentPolyIndexAdd%
DIM SHARED currentLineIndexAdd%
DIM SHARED currentPolyAdd%

DIM SHARED edgeYmin%, edgeYmax%
DIM SHARED polyEdge(0 TO 1, 0 TO SCRHEIGHT% - 1) AS Edge


' ---- Init ----

InitPrecalcs

SCREEN 13

COLOR 63

SetDefaultPalette

ResetElementAddIndices
InitScene3D


' ---- Main Loop ----

freeMem& = FRE(-1)

timeStart! = TIMER
nframe& = 0

t! = 0
DO WHILE INKEY$ = ""
    'dt! = TIMER - timeStart!

    IF BLURSWITCH% = 0 THEN ClearVram

    RunScene3D (4 * t!)

    IF BLURSWITCH% = 1 THEN
        BlurVramX
    END IF

    'WaitForVsync

    WriteVram

    'PRINT freeMem&

    t! = t! + .01
    nframe& = nframe& + 1
LOOP
timeEnd! = TIMER

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

SUB AddVertex (x%, y%, z%)

    objectVertices(currentVertexAdd%).x = x%
    objectVertices(currentVertexAdd%).y = y%
    objectVertices(currentVertexAdd%).z = z%

    currentVertexAdd% = currentVertexAdd% + 1

END SUB

SUB BlurVram

    ' Dirty blur two pixels at once (works only if colors are from 0 to 63)
    FOR i% = 2 + SCRWIDTH2% TO 2 + SCRWIDTH2% * (SCRHEIGHT% - 1) - 1
        vram%(i%) = ((vram%(i% - 1) + vram%(i% + 1) + vram%(i% - SCRWIDTH2%) + vram%(i% + SCRWIDTH2%)) \ 4) AND &H3F3F
    NEXT i%

    ClearBlurJunk

END SUB

SUB BlurVramX

    ' Dirty blur two pixels at once (works only if colors are from 0 to 127)
    FOR i% = 2 + SCRWIDTH% \ 2 TO 2 + (SCRWIDTH% \ 2) * (SCRHEIGHT% - 1) - 1
        vram%(i%) = ((vram%(i% - 1) + vram%(i% + 1)) \ 2) AND &H3F3F
    NEXT i%

    ClearBlurJunk

END SUB

SUB BlurVramY

    ' Dirty blur two pixels at once (works only if colors are from 0 to 127)
    FOR i% = 2 + SCRWIDTH2% TO 2 + SCRWIDTH2% * (SCRHEIGHT% - 1) - 1
        vram%(i%) = ((vram%(i% - SCRWIDTH2%) + vram%(i% + SCRWIDTH2%)) \ 2) AND &H3F3F
    NEXT i%

    ClearBlurJunk

END SUB

SUB CalcObjectVerticesRange (obj AS Object3D, range AS Range3D)

    offset% = obj.verticesOffset
    n% = obj.numVertices - 1

    minX! = 1000000: maxX! = -1000000
    minY! = 1000000: maxY! = -1000000
    minZ! = 1000000: maxZ! = -1000000

    FOR i% = 0 TO n%
        k% = i% + offset%
      
        x! = objectVertices(k%).x
        y! = objectVertices(k%).y
        z! = objectVertices(k%).z

        IF x! < minX! THEN minX! = x! ELSE IF x! > maxX! THEN maxX! = x!
        IF y! < minY! THEN minY! = y! ELSE IF y! > maxY! THEN maxY! = y!
        IF z! < minZ! THEN minZ! = z! ELSE IF z! > maxZ! THEN maxZ! = z!
    NEXT i%

    range.min.x = minX!
    range.max.x = maxX!
    range.min.y = minY!
    range.max.y = maxY!
    range.min.z = minZ!
    range.max.z = maxZ!

END SUB

SUB CenterObjectVertices (obj AS Object3D)

    offset% = obj.verticesOffset
    n% = obj.numVertices - 1

    DIM range AS Range3D
    CalcObjectVerticesRange obj, range

    centerX! = (range.min.x + range.max.x) / 2
    centerY! = (range.min.y + range.max.y) / 2
    centerZ! = (range.min.z + range.max.z) / 2

    FOR i% = 0 TO n%
        k% = i% + offset%

        objectVertices(k%).x = objectVertices(k%).x - centerX!
        objectVertices(k%).y = objectVertices(k%).y - centerY!
        objectVertices(k%).z = objectVertices(k%).z - centerZ!
    NEXT i%

END SUB

SUB ClearBlurJunk

    ' Clear junk at top scanline
    FOR i% = 2 TO 2 + SCRWIDTH2% - 1
        vram%(i%) = 0
    NEXT i%

    ' Clear junk at bottom scanline
    FOR i% = 2 + SCRWIDTH2% * (SCRHEIGHT% - 1) TO 2 + SCRWIDTH2% * SCRHEIGHT% - 1
        vram%(i%) = 0
    NEXT i%

END SUB

SUB ClearVram

    REDIM vram%(0 TO 32001)

    vram%(0) = 320 * 8
    vram%(1) = 200

END SUB

SUB DrawEdges (col%)

    screenSeg& = VARSEG(vram%(0))

    yEnd% = edgeYmax% - 1 ' one less edge bottom to avoid overdraw
    FOR y% = edgeYmin% TO yEnd%
        xs0% = polyEdge(0, y%).x
        xs1% = polyEdge(1, y%).x - 1 ' one less pixel right to avoid overdraw

        IF xs1% >= xs0% THEN
            DEF SEG = screenSeg& + segY%(y%)

            IF xs0% AND 1 THEN
                POKE 4 + xs0%, col%
                xs0% = xs0% + 1
            END IF
            IF NOT xs1% AND 1 THEN
                POKE 4 + xs1%, col%
                xs1% = xs1% - 1
            END IF

            FOR x% = xs0% TO xs1% STEP 2
                POKE 4 + x%, col%
                POKE 5 + x%, col%
            NEXT x%
        END IF

    NEXT y%

END SUB

SUB DrawEdgesArr (col%)

    colH% = col% * 256
    col16% = colH% OR col%

    yEnd% = edgeYmax% - 1 ' one less edge bottom to avoid overdraw
    ofs% = 2 + edgeYmin% * (SCRWIDTH% \ 2)
    FOR y% = edgeYmin% TO yEnd%
        xs0% = polyEdge(0, y%).x
        xs1% = polyEdge(1, y%).x - 1 ' one less pixel right to avoid overdraw

        IF xs1% >= xs0% THEN

            xi% = ofs% + xs0% \ 2

            IF xs0% AND 1 THEN
                c% = vram%(xi%)
                vram%(xi%) = (c% AND &HFF00) OR colH%
                xs0% = xs0% + 1
                xi% = xi% + 1
            END IF
            IF NOT xs1% AND 1 THEN
                xr% = ofs% + xs1% \ 2
                c% = vram%(xr%)
                vram%(xr%) = (c% AND &HFF) OR col%
                xs1% = xs1% - 1
            END IF

            FOR x% = xs0% TO xs1% STEP 2
                vram%(xi%) = col16%
                xi% = xi% + 1
            NEXT x%

        END IF

    ofs% = ofs% + (SCRWIDTH% \ 2)
    NEXT y%

END SUB

SUB DrawPoly (i%, polyIndex%)

    i0% = objectPolyIndices%(i%)
    i1% = objectPolyIndices%(i% + 1)
    i2% = objectPolyIndices%(i% + 2)

    x0% = screenVertices(i0%).x
    x1% = screenVertices(i1%).x
    x2% = screenVertices(i2%).x
    y0% = screenVertices(i0%).y
    y1% = screenVertices(i1%).y
    y2% = screenVertices(i2%).y

    min% = y0%
    max% = min%
    IF y1% < min% THEN min% = y1% ELSE IF y1% > max% THEN max% = y1%
    IF y2% < min% THEN min% = y2% ELSE IF y2% > max% THEN max% = y2%
    edgeYmin% = min%
    edgeYmax% = max%

    IF edgeYmin% < 0 THEN edgeYmin% = 0
    IF edgeYmax% > SCRHEIGHT% - 1 THEN edgeYmax% = SCRHEIGHT% - 1

    IF edgeYmax% > edgeYmin% THEN
        PrepareEdgeList x0%, y0%, x1%, y1%
        PrepareEdgeList x1%, y1%, x2%, y2%
        PrepareEdgeList x2%, y2%, x0%, y0%

        col% = objectPolygons(polyIndex%).col
        DrawEdges col%
        'DrawEdgesArr col%
    END IF

END SUB

SUB DrawPolyAlt (i%, polyIndex%)

    i0% = objectPolyIndices%(i%)
    i1% = objectPolyIndices%(i% + 1)
    i2% = objectPolyIndices%(i% + 2)

    x0% = screenVertices(i0%).x
    y0% = screenVertices(i0%).y
    x1% = screenVertices(i1%).x
    y1% = screenVertices(i1%).y
    x2% = screenVertices(i2%).x
    y2% = screenVertices(i2%).y

    xMin% = x0%
    yMin% = y0%
    xMax% = xMin%
    yMax% = yMin%

    IF x1% < xMin% THEN xMin% = x1%
    IF x1% > xMax% THEN xMax% = x1%
    IF y1% < yMin% THEN yMin% = y1%
    IF y1% > yMax% THEN yMax% = y1%

    IF x2% < xMin% THEN xMin% = x2%
    IF x2% > xMax% THEN xMax% = x2%
    IF y2% < yMin% THEN yMin% = y2%
    IF y2% > yMax% THEN yMax% = y2%

    IF xMin% < 0 THEN xMin% = 0
    IF yMin% < 0 THEN yMin% = 0
    IF xMax% > SCRWIDTH% THEN xMax% = SCRWIDTH%
    IF yMax% > SCRHEIGHT% THEN yMax% = SCRHEIGHT%

    col% = objectPolygons(polyIndex%).col

    screenSeg& = VARSEG(vram%(0))

    FOR y% = yMin% TO yMax% - 1
        DEF SEG = screenSeg& + segY%(y%)
        FOR x% = xMin% TO xMax% - 1
            IF (y% - y0%) * (x1% - x0%) - (x% - x0%) * (y1% - y0%) <= 0 THEN
                IF (y% - y1%) * (x2% - x1%) - (x% - x1%) * (y2% - y1%) <= 0 THEN
                    IF (y% - y2%) * (x0% - x2%) - (x% - x2%) * (y0% - y2%) <= 0 THEN
                        POKE 4 + x%, col%
                    END IF
                END IF
            END IF
        NEXT x%
    NEXT y%

END SUB

SUB FindObjectScaleToFit (obj AS Object3D, desiredRange!, scale!)

    DIM range AS Range3D
    CalcObjectVerticesRange obj, range

    dx! = range.max.x - range.min.x
    dy! = range.max.y - range.min.y
    dz! = range.max.z - range.min.z

    rangeLengthDiag! = SQR(dx! * dx! + dy! * dy! + dz! * dz!)

    scale! = desiredRange! / rangeLengthDiag!

END SUB

SUB FlipPolygonOrientation (obj AS Object3D)

    offset% = obj.polyIndicesOffset
    n% = offset% + obj.numPolyIndices - 1

    FOR i% = offset% TO n% STEP 3
        FOR j% = 0 TO 2 STEP 2
            k% = objectPolyIndices%(i% + j%)
            objectPolyIndices%(i% + 2 - j%) = k%
        NEXT j%
    NEXT i%

END SUB

SUB GenObject3D (params AS ObjGenParams, obj AS Object3D)

    obj.verticesOffset = currentVertexAdd%
    obj.polyIndicesOffset = currentPolyIndexAdd%
    obj.lineIndicesOffset = currentLineIndexAdd%
    obj.polysOffset = currentPolyAdd%

    SELECT CASE params.index
       
        CASE objGenCube%
            GenObject3Dcube params, obj

        CASE objGenDotCube%
            GenObject3DdotCube params, obj

        CASE objGenSphere%
            GenObject3Dsphere params, obj

        CASE objGenTorus%
            GenObject3Dtorus params, obj

        CASE ELSE
            PRINT "Object Index doesn't exist.."
    END SELECT

END SUB

SUB GenObject3Dcube (params AS ObjGenParams, obj AS Object3D)

    obj.numVertices = 8
    obj.numPolys = 12
    obj.numPolyIndices = obj.numPolys * 3
    obj.numLineIndices = 2 * 12

    halfSize% = params.size

    FOR z% = -1 TO 1 STEP 2
        FOR y% = -1 TO 1 STEP 2
            FOR x% = -1 TO 1 STEP 2
                AddVertex x% * halfSize%, y% * halfSize%, z% * halfSize%
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

SUB GenObject3DdotCube (params AS ObjGenParams, object AS Object3D)

    object.numVertices = 512
    object.numPolys = 0
    object.numPolyIndices = 0
    object.numLineIndices = 0

    halfSize% = params.size

    FOR z% = -4 TO 3
        FOR y% = -4 TO 3
            FOR x% = -4 TO 3
                AddVertex x% * halfSize%, y% * halfSize%, z% * halfSize%
            NEXT x%
        NEXT y%
    NEXT z%

END SUB

SUB GenObject3Dsphere (params AS ObjGenParams, obj AS Object3D)

END SUB

SUB GenObject3Dtorus (params AS ObjGenParams, obj AS Object3D)

END SUB

SUB InitPrecalcs

' precalcs to avoid multiply when DEF SEG on vram

    FOR i% = 0 TO SCRHEIGHT% - 1
        segY%(i%) = i% * 20
    NEXT i%

' Just randomize polygon colors for now

    FOR i& = 0 TO maxPolys%
        c% = (32 + ((8 * i&) AND 63)) MOD SHADEMAX%
        objectPolygons(i&).col = c%
    NEXT i&

END SUB

SUB InitScene3D

    DIM genParams AS ObjGenParams

    CONST defaultScaleRange! = 1024 - 128

    object(currentObject%).renderType = renderTypePoly%
    mustFlipPolygonOrientation% = FALSE%

    SELECT CASE currentObject%

        CASE objGenCube%
            genParams.size = 32

        CASE objGenDotCube%
            genParams.size = 16
            object(currentObject%).renderType = renderTypeDots%

        CASE objGenSphere%

        CASE objGenTorus%

        CASE objLoadTest%
            objFile$ = "teapot.plg"
            'mustFlipPolygonOrientation% = TRUE%
            'object(currentObject%).renderType = renderTypeDots%

        CASE objLoadTeapot%
            objFile$ = "teapot.plg"

        CASE objLoadMask%
            objFile$ = "mask.plg"

        CASE ELSE

    END SELECT


    IF objectMustGen% = TRUE% THEN
        genParams.index = currentObject%
        GenObject3D genParams, object(currentObject%)
    END IF

    IF objectMustLoad% = TRUE% THEN
        IF objFile$ <> "" THEN LoadObject3D objFile$, object(currentObject%)

        FindObjectScaleToFit object(currentObject%), defaultScaleRange!, scaleValue!
        ScaleObjectVertices object(currentObject%), scaleValue!

        IF mustFlipPolygonOrientation% THEN FlipPolygonOrientation object(currentObject%)
    END IF

END SUB

SUB LoadObject3D (objFile$, obj AS Object3D)

    CONST maxPolyPoints% = 32
    CONST maxIndexDataNum% = 2 + maxPolyPoints%

    DIM values(0 TO maxIndexDataNum% - 1) AS STRING

    obj.verticesOffset = currentVertexAdd%
    obj.polyIndicesOffset = currentPolyIndexAdd%
    obj.lineIndicesOffset = currentLineIndexAdd%
    obj.polysOffset = currentPolyAdd%

    OPEN objFile$ FOR INPUT AS #1

    ReadLineStringFromPlg 1, lbuf$
    ReadLineValues lbuf$, values(), 3

    obj.numVertices = VAL(values(1))
    obj.numPolys = VAL(values(2))
    obj.numPolyIndices = 0
    obj.numLineIndices = 0

    FOR i% = 1 TO obj.numVertices
        ReadLineStringFromPlg 1, lbuf$
        ReadLineValues lbuf$, values(), 3
        AddVertex VAL(values(0)), VAL(values(1)), VAL(values(2))
    NEXT i%

    FOR i% = 1 TO obj.numPolys
        ReadLineStringFromPlg 1, lbuf$
        ReadLineValues lbuf$, values(), maxIndexDataNum%    ' later I will deduce by reading second value the size, now I just read max
        numPoints% = VAL(values(1))
        obj.numPolyIndices = obj.numPolyIndices + 3 * (numPoints% - 2)

        IF numPoints% = 3 THEN
            AddTriangleIndices VAL(values(2)), VAL(values(3)), VAL(values(4))
        ELSE
            FOR j% = 0 TO numPoints% - 3
                AddTriangleIndices VAL(values(2)), VAL(values(3 + j%)), VAL(values(4 + j%))
            NEXT j%
        END IF
    NEXT i%

    CLOSE #1

    CenterObjectVertices obj

END SUB

SUB PlotPixel (x%, y%, col%)

    screenSeg& = VARSEG(vram%(0))

    DEF SEG = screenSeg& + segY%(y%)
    POKE 4 + x%, col%

END SUB

SUB PrepareEdgeList (x0%, y0%, x1%, y1%)

    IF y0% <> y1% THEN

        IF (y0% < y1%) THEN
            edgeIndex% = 0
            xs0% = x0%: ys0% = y0%
            xs1% = x1%: ys1% = y1%
        ELSE
            edgeIndex% = 1
            xs0% = x1%: ys0% = y1%
            xs1% = x0%: ys1% = y0%
        END IF

        xs0% = xs0% * FPRAST%
        xs1% = xs1% * FPRAST%
        dy% = ys1% - ys0%
        dx% = (xs1% - xs0%) \ dy%

        FOR y% = ys0% TO ys1% - 1
            IF y% >= 0 AND y% < SCRHEIGHT% THEN
                polyEdge(edgeIndex%, y%).x = xs0% \ FPRAST%
            END IF
            xs0% = xs0% + dx%
        NEXT y%

    END IF

END SUB

SUB ReadLineStringFromPlg (fileHandle%, bufDst$)

    DO
        LINE INPUT #fileHandle%, lbuf$
        IF LEFT$(lbuf$, 1) = "#" THEN lbuf$ = ""
    LOOP WHILE lbuf$ = ""

    bufDst$ = lbuf$

END SUB

SUB ReadLineValues (inp$, out$(), numVals%)

    FOR i% = 0 TO numVals% - 1
        inp$ = LTRIM$(inp$)
        index% = INSTR(inp$, " ")
        IF index% > 0 THEN
            out$(i%) = LEFT$(inp$, index% - 1)
            inp$ = RIGHT$(inp$, LEN(inp$) - index%)
        ELSE
            out$(i%) = inp$
        END IF
    NEXT i%

END SUB

SUB RenderObject3D (obj AS Object3D)

    SELECT CASE obj.renderType

        CASE renderTypeDots%
            RenderObject3Ddots obj

        CASE renderTypeWire%
            RenderObject3Dwire obj

        CASE renderTypePoly%
            SortObject3Dpoly obj
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

    indexOffset% = obj.polyIndicesOffset
    polyOffset% = obj.polysOffset

    FOR zSlot% = maxZbucket% TO minZbucket% STEP -1
        count% = zBucket%(zSlot%, 0)
        FOR n% = 1 TO count%
            i% = zBucket%(zSlot%, n%)
            polyIndex% = polyOffset% + (i% - indexOffset%) \ 3  ' I will think if it's worth it storing this too or recalculating. Probably not.

            IF ALTPOLY% = 0 THEN
                DrawPoly i%, polyIndex%
            ELSE
                DrawPolyAlt i%, polyIndex%
            END IF

        NEXT n%
    NEXT zSlot%

    leftoversCount% = zBucketLeftovers%(0)
    FOR n% = 1 TO leftoversCount%
        i% = zBucketLeftovers%(n%)
        polyIndex% = polyOffset% + (i% - indexOffset%) \ 3

        IF ALTPOLY% = 0 THEN
            DrawPoly i%, polyIndex%
        ELSE
            DrawPolyAlt i%, polyIndex%
        END IF
    NEXT n%

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
tt! = t!
't! = 0

    xt! = 0 'SIN(t!) * 48
    yt! = 0 'SIN(1.5 * t!) * 32
    zt! = 256 '+ SIN(1.25 * t!) * 96

    rx! = .7 * t!
    ry! = t!
    rz! = 1.25 * t!

    SELECT CASE currentObject%

        CASE objGenCube%
            zt! = 128

        CASE objGenDotCube%
            zt! = 256

        CASE objGenSphere%

        CASE objGenTorus%

        CASE objLoadTest%
            zt! = 1024

        CASE objLoadTeapot%
            yt! = 0
            zt! = 1024
            rx! = PI!
            ry! = 4 * tt!
            'rz! = 3 * tt!

        CASE objLoadMask%
            zt! = 160 + SIN(4 * tt!) * 48
            rx! = -PI! / 2 + SIN(6 * tt!) * .75
            ry! = PI!
            rz! = SIN(8 * tt!) * .75

        CASE ELSE

    END SELECT

    SetObjectPosition object(currentObject%), xt!, yt!, zt!
    SetObjectRotation object(currentObject%), rx!, ry!, rz!

    TransformObject3D object(currentObject%)
    RenderObject3D object(currentObject%)

END SUB

SUB ScaleObjectVertices (obj AS Object3D, scale!)

    offset% = obj.verticesOffset
    n% = obj.numVertices - 1

    FOR i% = 0 TO n%
        k% = i% + offset%

        objectVertices(k%).x = scale! * objectVertices(k%).x
        objectVertices(k%).y = scale! * objectVertices(k%).y
        objectVertices(k%).z = scale! * objectVertices(k%).z
    NEXT i%

END SUB

SUB SetCurrentObject (objNum%, mustGen%, mustLoad%)

    currentObject% = objNum%
    objectMustGen% = mustGen%
    objectMustLoad% = mustLoad%

END SUB

SUB SetDefaultPalette

OUT &H3C8, 0
OUT &H3C9, 0
OUT &H3C9, 0
OUT &H3C9, 0
FOR i% = 1 TO 127
    OUT &H3C9, (1 * i%) AND 63
    OUT &H3C9, (.75 * i%) AND 63
    OUT &H3C9, (2! * i%) AND 63
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

SUB SortObject3Dpoly (obj AS Object3D)

    ' Reset ZBUCKET indices
    FOR i% = 0 TO ZBUCKETS%
        zBucket%(i%, 0) = 0
    NEXT i%
    zBucketLeftovers%(0) = 0

    minZbucket% = ZBUCKETS%
    maxZbucket% = 0

    offset% = obj.polyIndicesOffset
    n% = offset% + obj.numPolyIndices - 1

    FOR i% = offset% TO n% STEP 3

        i0% = objectPolyIndices%(i%)
        i1% = objectPolyIndices%(i% + 1)
        i2% = objectPolyIndices%(i% + 2)

        z0% = screenVertices(i0%).z
        z1% = screenVertices(i1%).z
        z2% = screenVertices(i2%).z

        IF z0% > 0 AND z1% > 0 AND z2% > 0 THEN
            x0% = screenVertices(i0%).x
            x1% = screenVertices(i1%).x
            x2% = screenVertices(i2%).x
            y0% = screenVertices(i0%).y
            y1% = screenVertices(i1%).y
            y2% = screenVertices(i2%).y

            a% = (x0% - x1%) * (y2% - y1%) - (x2% - x1%) * (y0% - y1%)
            IF a% >= 0 THEN
                zIndex% = (z0% + z1% + z2%) \ (3 * ZBUCKETSIZE%)
                IF zIndex% > ZBUCKETS% THEN zIndex% = ZBUCKETS%
                slot% = zBucket%(zIndex%, 0) + 1
                IF slot% <= ZBUCKETPOLYS% THEN
                    zBucket%(zIndex%, slot%) = i%
                    zBucket%(zIndex%, 0) = slot%
                    IF zIndex% < minZbucket% THEN minZbucket% = zIndex%
                    IF zIndex% > maxZbucket% THEN maxZbucket% = zIndex%
                ELSE
                    leftoversIndex% = zBucketLeftovers%(0) + 1
                    IF leftoversIndex% <= LEFTOVERSSIZE% THEN
                        zBucketLeftovers%(leftoversIndex%) = i%
                        zBucketLeftovers%(0) = leftoversIndex%
                    END IF
                END IF
            END IF
        END IF

    NEXT i%

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

    PUT (0, 0), vram%, PSET

END SUB

