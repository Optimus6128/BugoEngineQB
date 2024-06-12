DECLARE SUB DrawEdgesTextured ()
DECLARE SUB DrawEdgesGouraud (col%)
TYPE Vertex3D
    x AS INTEGER
    y AS INTEGER
    z AS INTEGER
END TYPE

TYPE Element3D
    x AS INTEGER
    y AS INTEGER
    z AS INTEGER
    c AS INTEGER
    u AS INTEGER
    v AS INTEGER
END TYPE

TYPE Edge
    x AS INTEGER
    c AS INTEGER
    u AS INTEGER
    v AS INTEGER
END TYPE

TYPE Vector3D
    x AS SINGLE
    y AS SINGLE
    z AS SINGLE
END TYPE

TYPE IVector3D
    x AS INTEGER
    y AS INTEGER
    z AS INTEGER
END TYPE

TYPE Range3D
    min AS Vector3D
    max AS Vector3D
END TYPE


TYPE PolyData
    col AS INTEGER
    shade AS INTEGER
END TYPE

TYPE Object3D
    verticesOffset AS INTEGER
    polyIndicesOffset AS INTEGER
    lineIndicesOffset AS INTEGER
    polysOffset AS INTEGER
    polyNormalsOffset AS INTEGER
    vertexNormalsOffset AS INTEGER

    numVertices AS INTEGER
    numPolyIndices AS INTEGER
    numLineIndices AS INTEGER
    numPolys AS INTEGER

    renderType AS INTEGER

    position AS Vector3D
    rotation AS Vector3D
END TYPE

TYPE ObjGenParams
    index AS INTEGER
    size AS INTEGER
END TYPE


' ---- Common Consts ----

CONST SCRWIDTH% = 320
CONST SCRHEIGHT% = 200
CONST SCRWIDTH2% = SCRWIDTH% \ 2
CONST SCRHEIGHT2% = SCRHEIGHT% \ 2

CONST FALSE% = 0
CONST TRUE% = 1

CONST PI! = 3.14159265359#


' ---- Main Functions ----

DECLARE SUB InitPrecalcs ()

DECLARE SUB SetDefaultPalette ()
DECLARE SUB DisplayPalette ()

DECLARE SUB ClearVram ()
DECLARE SUB WriteVram ()
DECLARE SUB WaitForVsync ()


' ---- FX functions  ----

DECLARE SUB ClearBlurJunk ()
DECLARE SUB BlurVram ()
DECLARE SUB BlurVramX ()
DECLARE SUB BlurVramY ()


' ---- Vector Math functions ----

DECLARE SUB CalcRotMatrix (rot AS Vector3D)
DECLARE SUB InvertMatrix (srcMat!(), dstMat!())
DECLARE SUB MulMatWithVector3D (srcVec AS Vector3D, dstVec AS Vector3D, mat!())

DECLARE SUB SubIVec (vSrc1 AS IVector3D, vSrc2 AS IVector3D, vDst AS IVector3D)
DECLARE SUB DotIVec (vSrc1 AS IVector3D, vSrc2 AS IVector3D, iDst%)
DECLARE SUB CrossIVec (vSrc1 AS IVector3D, vSrc2 AS IVector3D, vDst AS IVector3D)
DECLARE SUB NormIVec (v AS IVector3D)


' ---- Object generation functions ----

DECLARE SUB GenObject3D (params AS ObjGenParams, obj AS Object3D)
DECLARE SUB GenObject3Dcube (params AS ObjGenParams, obj AS Object3D)
DECLARE SUB GenObject3DdotCube (params AS ObjGenParams, obj AS Object3D)
DECLARE SUB GenObject3Dsphere (params AS ObjGenParams, obj AS Object3D)
DECLARE SUB GenObject3Dtorus (params AS ObjGenParams, obj AS Object3D)

DECLARE SUB ResetElementAddIndices ()
DECLARE SUB AddVertex (x%, y%, z%)
DECLARE SUB AddLineIndices (i0%, i1%)
DECLARE SUB AddTriangleIndices (i0%, i1%, i2%)

DECLARE SUB CalcObjectPolyNormals (obj AS Object3D)

DECLARE SUB AddPolyNormal (x%, y%, z%)
DECLARE SUB AddVertexNormal (x%, y%, z%)

DECLARE SUB CalcObjectVerticesRange (obj AS Object3D, range AS ANY)
DECLARE SUB FindObjectScaleToFit (obj AS Object3D, desiredRange!, scale!)
DECLARE SUB ScaleObjectVertices (obj AS Object3D, scale!)

DECLARE SUB CenterObjectVertices (obj AS Object3D)
DECLARE SUB FlipPolygonOrientation (obj AS Object3D)


' ---- Object loading functions ----

DECLARE SUB LoadObject3D (objFile$, obj AS Object3D)

DECLARE SUB ReadLineStringFromPlg (fileHandle%, bufDst$)
DECLARE SUB ReadLineValues (inp$, out$(), numVals%)


' ---- Rendering functions  ----

DECLARE SUB RenderObject3Ddots (obj AS Object3D)
DECLARE SUB RenderObject3Dwire (obj AS Object3D)
DECLARE SUB RenderObject3Dpoly (obj AS Object3D)
DECLARE SUB SortObject3Dpoly (obj AS Object3D)

DECLARE SUB PlotPixel (x%, y%, col%)
DECLARE SUB DrawPoly (i%, polyIndex%)

DECLARE SUB PrepareEdgeListFlat (e0 AS Element3D, e1 AS Element3D)
DECLARE SUB PrepareEdgeListGouraud (e0 AS Element3D, e1 AS Element3D)
DECLARE SUB PrepareEdgeListTextured (e0 AS Element3D, e1 AS Element3D)

DECLARE SUB CalcObjectPolyLighting (obj AS Object3D, light AS ANY)

DECLARE SUB DrawEdges (col%)

' ---- Object transformation functions ----

DECLARE SUB SetObjectPosition (obj AS Object3D, px!, py!, pz!)
DECLARE SUB SetObjectRotation (obj AS Object3D, rx!, ry!, rz!)
DECLARE SUB RenderObject3D (obj AS Object3D)
DECLARE SUB TransformObject3D (obj AS Object3D)


' ---- Scene management functions ----

DECLARE SUB SetCurrentObject (objNum%, objFile$)
DECLARE SUB InitScene3D ()
DECLARE SUB RunScene3D (t!)



' ---- Engine Consts ----

CONST BLURSWITCH% = 0   ' blur will overflow in interpreter only compiled exe

CONST FPRAST% = 64
CONST FPNORM% = 64

CONST renderTypeDots% = 0
CONST renderTypeWire% = 1
CONST renderTypePoly% = 2

CONST ZBUCKETSIZE% = 16
CONST ZBUCKETMAX% = 2048
CONST ZBUCKETS% = ZBUCKETMAX% / ZBUCKETSIZE%
CONST ZBUCKETPOLYS% = 64
CONST LEFTOVERSSIZE% = 512


' ---- Zbucket arrays ----

DIM SHARED zBucket%(0 TO ZBUCKETS%, 0 TO ZBUCKETPOLYS%)
' Value 0 in ZBUCKETPOLYS% will really be the counter of num polys in list or next poly to add
DIM SHARED zBucketLeftovers%(0 TO LEFTOVERSSIZE%)
' Polys that don't fit in buckets anymore or over ZBUCKETMAX%
' Value 0 in this array will be again the current count

DIM SHARED minZbucket%
DIM SHARED maxZbucket%


' ---- Main arrays ----

'$DYNAMIC

DIM SHARED vram%(0 TO 32001)
vram%(0) = 320 * 8
vram%(1) = 200

'$STATIC


' ---- Object pool sizes ----

CONST maxObjVertices% = 1024
CONST avgObjVertexNum% = 256
CONST maxObjectsOnScreen% = 4
CONST maxVertices% = maxObjectsOnScreen% * avgObjVertexNum%
CONST maxPolys% = 2 * maxVertices%


' ---- Object arrays and engine shared variables ----

CONST objGenCube% = 0
CONST objGenDotCube% = 1
CONST objGenSphere% = 2
CONST objGenTorus% = 3
CONST objLoadTest% = 4
CONST objLoadTeapot% = 5
CONST objLoadMask% = 6
CONST numObjects% = 7

DIM SHARED object(0 TO numObjects% - 1) AS Object3D

DIM SHARED screenVertices(0 TO maxObjVertices% - 1) AS Element3D
DIM SHARED transformedVertexNormals(0 TO maxObjVertices% - 1) AS IVector3D

DIM SHARED objectVertices(0 TO maxVertices% - 1) AS Vertex3D
DIM SHARED objectPolyIndices%(0 TO 3 * maxPolys% - 1)
DIM SHARED objectLineIndices%(0 TO 6 * maxPolys% - 1)
DIM SHARED objectPolygons(0 TO maxPolys% - 1) AS PolyData

DIM SHARED objectPolyNormals(0 TO maxPolys% - 1) AS IVector3D
DIM SHARED objectVertexNormals(0 TO maxVertices% - 1) AS IVector3D

DIM SHARED currentVertexAdd%
DIM SHARED currentPolyIndexAdd%
DIM SHARED currentLineIndexAdd%
DIM SHARED currentPolyAdd%
DIM SHARED currentPolyNormalAdd%
DIM SHARED currentVertexNormalAdd%

DIM SHARED edgeYmin%, edgeYmax%
DIM SHARED polyEdge(0 TO 1, 0 TO SCRHEIGHT% - 1) AS Edge

DIM SHARED rotMat(0 TO 8) AS SINGLE

DIM SHARED light1 AS Vector3D


' ---- Object Selector ----

DIM SHARED currentObject%
DIM SHARED objectLoadFile$

CLS
FILES "*.plg"
PRINT
INPUT "Load: ", objFile$

IF objFile$ = "" THEN
    SetCurrentObject objGenCube%, ""
ELSE
    SetCurrentObject objLoadTest%, objFile$
END IF


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

    RunScene3D (2 * t!)

    IF BLURSWITCH% = 1 THEN
        BlurVramX
    END IF

    'WaitForVsync

    WriteVram

    'DisplayPalette

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

SUB AddPolyNormal (x%, y%, z%)

    objectPolyNormals(currentPolyNormalAdd%).x = x%
    objectPolyNormals(currentPolyNormalAdd%).y = y%
    objectPolyNormals(currentPolyNormalAdd%).z = z%

    currentPolyNormalAdd% = currentPolyNormalAdd% + 1

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

SUB AddVertexNormal (x%, y%, z%)

    objectVertexNormals(currentVertexNormalAdd%).x = x%
    objectVertexNormals(currentVertexNormalAdd%).y = y%
    objectVertexNormals(currentVertexNormalAdd%).z = z%

    currentVertexNormalAdd% = currentVertexNormalAdd% + 1

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

SUB CalcObjectPolyLighting (obj AS Object3D, light AS Vector3D)

    DIM rotLight AS Vector3D
    DIM iLight AS IVector3D
    DIM invRotMat!(8)

    InvertMatrix rotMat!(), invRotMat!()

    MulMatWithVector3D light, rotLight, invRotMat!()

    iLight.x = INT(rotLight.x * FPNORM%)
    iLight.y = INT(rotLight.y * FPNORM%)
    iLight.z = INT(rotLight.z * FPNORM%)

    offset% = obj.polyNormalsOffset
    n% = offset% + obj.numPolys - 1

    FOR i% = offset% TO n%
        DotIVec objectPolyNormals(i%), iLight, d%

        IF d% < 0 THEN d% = 0
        IF d% > FPNORM% THEN d% = FPNORM%

        objectPolygons(i%).shade = (63 * d%) \ FPNORM%
    NEXT i%

END SUB

SUB CalcObjectPolyNormals (obj AS Object3D)

    DIM norm AS IVector3D

    DIM a0 AS IVector3D
    DIM a1 AS IVector3D

    DIM v0 AS IVector3D
    DIM v1 AS IVector3D
    DIM v2 AS IVector3D

    offset% = obj.polyIndicesOffset
    n% = offset% + obj.numPolyIndices - 1

    FOR i% = offset% TO n% STEP 3

        i0% = objectPolyIndices%(i%)
        i1% = objectPolyIndices%(i% + 1)
        i2% = objectPolyIndices%(i% + 2)

        v0.x = objectVertices(i0%).x
        v0.y = objectVertices(i0%).y
        v0.z = objectVertices(i0%).z

        v1.x = objectVertices(i1%).x
        v1.y = objectVertices(i1%).y
        v1.z = objectVertices(i1%).z

        v2.x = objectVertices(i2%).x
        v2.y = objectVertices(i2%).y
        v2.z = objectVertices(i2%).z

        SubIVec v1, v0, a0
        SubIVec v2, v1, a1

        CrossIVec a0, a1, norm
        NormIVec norm

        AddPolyNormal norm.x, norm.y, norm.z

    NEXT i%

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

    range.min.x = INT(minX!)
    range.max.x = INT(maxX!)
    range.min.y = INT(minY!)
    range.max.y = INT(maxY!)
    range.min.z = INT(minZ!)
    range.max.z = INT(maxZ!)

END SUB

SUB CalcRotMatrix (rot AS Vector3D)

    cosxr! = COS(rot.x)
    cosyr! = COS(rot.y)
    coszr! = COS(rot.z)
    sinxr! = SIN(rot.x)
    sinyr! = SIN(rot.y)
    sinzr! = SIN(rot.z)

    ' Matrix values
    rotMat(0) = cosyr! * coszr!
    rotMat(1) = sinxr! * sinyr! * coszr! - cosxr! * sinzr!
    rotMat(2) = cosxr! * sinyr! * coszr! + sinxr! * sinzr!
    rotMat(3) = cosyr! * sinzr!
    rotMat(4) = cosxr! * coszr! + sinxr! * sinyr! * sinzr!
    rotMat(5) = -sinxr! * coszr! + cosxr! * sinyr! * sinzr!
    rotMat(6) = -sinyr!
    rotMat(7) = sinxr! * cosyr!
    rotMat(8) = cosxr! * cosyr!

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

SUB CrossIVec (vSrc1 AS IVector3D, vSrc2 AS IVector3D, vDst AS IVector3D)

    v1x& = vSrc1.x: v1y& = vSrc1.y: v1z& = vSrc1.z
    v2x& = vSrc2.x: v2y& = vSrc2.y: v2z& = vSrc2.z

    vx% = (v1y& * v2z&) \ FPNORM% - (v1z& * v2y&) \ FPNORM%
    vy% = (v1z& * v2x&) \ FPNORM% - (v1x& * v2z&) \ FPNORM%
    vz% = (v1x& * v2y&) \ FPNORM% - (v1y& * v2x&) \ FPNORM%

    vDst.x = vx%
    vDst.y = vy%
    vDst.z = vz%

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

SUB DotIVec (vSrc1 AS IVector3D, vSrc2 AS IVector3D, iDst%)
   
    v1x& = vSrc1.x: v1y& = vSrc1.y: v1z& = vSrc1.z
    v2x& = vSrc2.x: v2y& = vSrc2.y: v2z& = vSrc2.z

    iDst% = (v1x& * v2x&) \ FPNORM% + (v1y& * v2y&) \ FPNORM% + (v1z& * v2z&) \ FPNORM%

END SUB

SUB DrawEdges (col%)

    screenSeg& = VARSEG(vram%(0))
    ys% = edgeYmin% * (SCRWIDTH% \ 16)

    yEnd% = edgeYmax% - 1 ' one less edge bottom to avoid overdraw
    FOR y% = edgeYmin% TO yEnd%
        xs0% = polyEdge(0, y%).x
        xs1% = polyEdge(1, y%).x - 1 ' one less pixel right to avoid overdraw

        IF xs0% < 0 THEN xs0% = 0
        IF xs1% > SCRWIDTH% - 1 THEN xs1% = SCRWIDTH% - 1

        IF xs1% >= xs0% THEN
            DEF SEG = screenSeg& + ys%

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

        ys% = ys% + (SCRWIDTH% \ 16)
    NEXT y%

END SUB

SUB DrawEdgesGouraud (col%)

    screenSeg& = VARSEG(vram%(0))
    ys% = edgeYmin% * (SCRWIDTH% \ 16)

    yEnd% = edgeYmax% - 1 ' one less edge bottom to avoid overdraw
    FOR y% = edgeYmin% TO yEnd%
        xs0% = polyEdge(0, y%).x
        xs1% = polyEdge(1, y%).x - 1 ' one less pixel right to avoid overdraw

        c0% = (col% + polyEdge(0, y%).c) * FPRAST%
        c1% = (col% + polyEdge(1, y%).c) * FPRAST%
        IF xs1% > xs0% THEN dc% = (c1% - c0%) \ (xs1% - xs0%)

        IF xs0% < 0 THEN xs0% = 0
        IF xs1% > SCRWIDTH% - 1 THEN xs1% = SCRWIDTH% - 1

        IF xs1% >= xs0% THEN
            DEF SEG = screenSeg& + ys%

            FOR x% = xs0% TO xs1%
                POKE 4 + x%, c0% \ FPRAST%
                c0% = c0% + dc%
            NEXT x%

        END IF

        ys% = ys% + (SCRWIDTH% \ 16)
    NEXT y%

END SUB

SUB DrawEdgesTextured

    screenSeg& = VARSEG(vram%(0))
    ys% = edgeYmin% * (SCRWIDTH% \ 16)

    yEnd% = edgeYmax% - 1 ' one less edge bottom to avoid overdraw
    FOR y% = edgeYmin% TO yEnd%
        xs0% = polyEdge(0, y%).x
        xs1% = polyEdge(1, y%).x - 1 ' one less pixel right to avoid overdraw

        u0% = polyEdge(0, y%).u * FPRAST%: u1% = polyEdge(1, y%).u * FPRAST%
        v0% = polyEdge(0, y%).v * FPRAST%: v1% = polyEdge(1, y%).v * FPRAST%
        IF xs1% > xs0% THEN
            du% = (u1% - u0%) \ (xs1% - xs0%)
            dv% = (v1% - v0%) \ (xs1% - xs0%)
        END IF

        IF xs0% < 0 THEN xs0% = 0
        IF xs1% > SCRWIDTH% - 1 THEN xs1% = SCRWIDTH% - 1

        IF xs1% >= xs0% THEN
            DEF SEG = screenSeg& + ys%

            FOR x% = xs0% TO xs1%
                u% = u0% \ FPRAST
                v% = v0% \ FPRAST%
                POKE 4 + x%, u% XOR v%
                u0% = u0% + du%
                v0% = v0% + dv%
            NEXT x%

        END IF

        ys% = ys% + (SCRWIDTH% \ 16)
    NEXT y%

END SUB

SUB DrawPoly (i%, polyIndex%)

    i0% = objectPolyIndices%(i%)
    i1% = objectPolyIndices%(i% + 1)
    i2% = objectPolyIndices%(i% + 2)

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
        ' it used to be that I passed parameters as variables from top
        ' like 10-12 params instead of the screenVertices(ix%) which would seem to be faster
        ' But it dropped performance even for flat.
        ' However because this is more elegant than the previous, I'll keep it and revisit later to write the ugly version if I need the performance.

        'PrepareEdgeListFlat screenVertices(i0%), screenVertices(i1%)
        'PrepareEdgeListFlat screenVertices(i1%), screenVertices(i2%)
        'PrepareEdgeListFlat screenVertices(i2%), screenVertices(i0%)
        'col% = objectPolygons(polyIndex%).col + objectPolygons(polyIndex%).shade
        'DrawEdgesFlat col%

        'PrepareEdgeListGouraud screenVertices(i0%), screenVertices(i1%)
        'PrepareEdgeListGouraud screenVertices(i1%), screenVertices(i2%)
        'PrepareEdgeListGouraud screenVertices(i2%), screenVertices(i0%)
        'col% = objectPolygons(polyIndex%).col
        'DrawEdgesGouraud col%

        PrepareEdgeListTextured screenVertices(i0%), screenVertices(i1%)
        PrepareEdgeListTextured screenVertices(i1%), screenVertices(i2%)
        PrepareEdgeListTextured screenVertices(i2%), screenVertices(i0%)
        DrawEdgesTextured

    END IF

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
    obj.polyNormalsOffset = currentPolyNormalAdd%
    obj.vertexNormalsOffset = currentVertexNormalAdd%

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

    CalcObjectPolyNormals obj

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

SUB GenObject3DdotCube (params AS ObjGenParams, obj AS Object3D)

    obj.numVertices = 512
    obj.numPolys = 0
    obj.numPolyIndices = 0
    obj.numLineIndices = 0

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

' Just randomize polygon colors for now

    FOR i& = 0 TO maxPolys% - 1
        'c% = (32 + ((16 * i&) AND 63)) AND 255

        c% = (i& AND 1) * 64
        'c% = 0

        IF BLURSWITCH% = 1 THEN c% = c% AND 63

        objectPolygons(i&).col = c%
    NEXT i&

' Also randomize vertex gouraud coloring and coordinates for now

    FOR i% = 0 TO maxObjVertices% - 1
        screenVertices(i%).c = INT(RND * 255) AND 63
        screenVertices(i%).u = (i% * 8) AND 63 'INT(RND * 255) AND 255
        screenVertices(i%).v = (i% * 16) AND 127 'i% * INT(RND * 255) AND 255
    NEXT i%

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

        CASE objLoadTeapot%

        CASE objLoadMask%

        CASE ELSE

    END SELECT


    IF objectLoadFile$ = "" THEN
        genParams.index = currentObject%
        GenObject3D genParams, object(currentObject%)
    ELSE
        LoadObject3D objectLoadFile$, object(currentObject%)

        FindObjectScaleToFit object(currentObject%), defaultScaleRange!, scaleValue!
        ScaleObjectVertices object(currentObject%), scaleValue!

        IF mustFlipPolygonOrientation% THEN FlipPolygonOrientation object(currentObject%)
   
        CalcObjectPolyNormals object(currentObject%)
    END IF

END SUB

SUB InvertMatrix (srcMat!(), dstMat!())

    k% = 0
    FOR j% = 0 TO 2
        FOR i% = 0 TO 2
            dstMat!(k%) = srcMat!(3 * i% + j%)
            k% = k% + 1
        NEXT i%
    NEXT j%

END SUB

SUB LoadObject3D (objFile$, obj AS Object3D)

    CONST maxPolyPoints% = 32
    CONST maxIndexDataNum% = 2 + maxPolyPoints%

    DIM values(0 TO maxIndexDataNum% - 1) AS STRING

    obj.verticesOffset = currentVertexAdd%
    obj.polyIndicesOffset = currentPolyIndexAdd%
    obj.lineIndicesOffset = currentLineIndexAdd%
    obj.polysOffset = currentPolyAdd%
    obj.polyNormalsOffset = currentPolyNormalAdd%
    obj.vertexNormalsOffset = currentVertexNormalAdd%

    OPEN objFile$ FOR INPUT AS #1

    ReadLineStringFromPlg 1, lbuf$
    ReadLineValues lbuf$, values(), 3

    obj.numVertices = VAL(values(1))
    numPolys% = VAL(values(2))  ' PLG polys that can have more edges than triangles
    obj.numPolys = 0            ' as we subdivide to only triangles below we will be increasing this

    obj.numPolyIndices = 0
    obj.numLineIndices = 0

    FOR i% = 1 TO obj.numVertices
        ReadLineStringFromPlg 1, lbuf$
        ReadLineValues lbuf$, values(), 3
        AddVertex VAL(values(0)), VAL(values(1)), VAL(values(2))
    NEXT i%

    FOR i% = 1 TO numPolys%
        ReadLineStringFromPlg 1, lbuf$
        ReadLineValues lbuf$, values(), maxIndexDataNum%    ' later I will deduce by reading second value the size, now I just read max
        numPoints% = VAL(values(1))
        numTriangles% = numPoints% - 2
        obj.numPolys = obj.numPolys + numTriangles%
        obj.numPolyIndices = obj.numPolyIndices + 3 * numTriangles%

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

SUB MulMatWithVector3D (srcVec AS Vector3D, dstVec AS Vector3D, mat!())

        x! = srcVec.x
        y! = srcVec.y
        z! = srcVec.z

        dstVec.x = x! * mat(0) + y! * mat(3) + z! * mat(6)
        dstVec.y = x! * mat(1) + y! * mat(4) + z! * mat(7)
        dstVec.z = x! * mat(2) + y! * mat(5) + z! * mat(8)

END SUB

SUB NormIVec (v AS IVector3D)

    x& = v.x: y& = v.y: z& = v.z
    d& = SQR(x& * x& + y& * y& + z& * z&) ' need to figure fixed point sqrt, it's sometimes outside +/- FPNORM% in length I think

    IF d& > 0 THEN
        v.x = (x& * FPNORM%) \ d&
        v.y = (y& * FPNORM%) \ d&
        v.z = (z& * FPNORM%) \ d&
    ELSE
        v.x = 0
        v.y = 0
        v.z = 0
    END IF

END SUB

SUB PlotPixel (x%, y%, col%)

    screenSeg& = VARSEG(vram%(0))

    DEF SEG = screenSeg& + y% * 20
    POKE 4 + x%, col%

END SUB

SUB PrepareEdgeListFlat (e0 AS Element3D, e1 AS Element3D)

    y0% = e0.y
    y1% = e1.y

    IF y0% <> y1% THEN

        IF (y0% < y1%) THEN
            edgeIndex% = 0
            xs0% = e0.x: ys0% = y0%
            xs1% = e1.x: ys1% = y1%
        ELSE
            edgeIndex% = 1
            xs0% = e1.x: ys0% = y1%
            xs1% = e0.x: ys1% = y0%
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

SUB PrepareEdgeListGouraud (e0 AS Element3D, e1 AS Element3D)

    y0% = e0.y
    y1% = e1.y

    IF y0% <> y1% THEN

        IF (y0% < y1%) THEN
            edgeIndex% = 0
            xs0% = e0.x: ys0% = y0%: cs0% = e0.c
            xs1% = e1.x: ys1% = y1%: cs1% = e1.c
        ELSE
            edgeIndex% = 1
            xs0% = e1.x: ys0% = y1%: cs0% = e1.c
            xs1% = e0.x: ys1% = y0%: cs1% = e0.c
        END IF

        xs0% = xs0% * FPRAST%
        xs1% = xs1% * FPRAST%
        cs0% = cs0% * FPRAST%
        cs1% = cs1% * FPRAST%

        dy% = ys1% - ys0%
        dx% = (xs1% - xs0%) \ dy%
        dc% = (cs1% - cs0%) \ dy%

        FOR y% = ys0% TO ys1% - 1
            IF y% >= 0 AND y% < SCRHEIGHT% THEN
                polyEdge(edgeIndex%, y%).x = xs0% \ FPRAST%
                polyEdge(edgeIndex%, y%).c = cs0% \ FPRAST%
            END IF
            xs0% = xs0% + dx%
            cs0% = cs0% + dc%
        NEXT y%

    END IF

END SUB

SUB PrepareEdgeListTextured (e0 AS Element3D, e1 AS Element3D)

    y0% = e0.y
    y1% = e1.y

    IF y0% <> y1% THEN

        IF (y0% < y1%) THEN
            edgeIndex% = 0
            xs0% = e0.x: ys0% = y0%: us0% = e0.u: vs0% = e0.v
            xs1% = e1.x: ys1% = y1%: us1% = e1.u: vs1% = e1.v
        ELSE
            edgeIndex% = 1
            xs0% = e1.x: ys0% = y1%: us0% = e1.u: vs0% = e1.v
            xs1% = e0.x: ys1% = y0%: us1% = e0.u: vs1% = e0.v
        END IF

        xs0% = xs0% * FPRAST%
        xs1% = xs1% * FPRAST%
        us0% = us0% * FPRAST%
        us1% = us1% * FPRAST%
        vs0% = vs0% * FPRAST%
        vs1% = vs1% * FPRAST%

        dy% = ys1% - ys0%
        dx% = (xs1% - xs0%) \ dy%
        du% = (us1% - us0%) \ dy%
        dv% = (vs1% - vs0%) \ dy%

        FOR y% = ys0% TO ys1% - 1
            IF y% >= 0 AND y% < SCRHEIGHT% THEN
                polyEdge(edgeIndex%, y%).x = xs0% \ FPRAST%
                polyEdge(edgeIndex%, y%).u = us0% \ FPRAST%
                polyEdge(edgeIndex%, y%).v = vs0% \ FPRAST%
            END IF
            xs0% = xs0% + dx%
            us0% = us0% + du%
            vs0% = vs0% + dv%
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
            CalcObjectPolyLighting obj, light1
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
                PlotPixel x%, y%, 63
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

            DrawPoly i%, polyIndex%
        NEXT n%
    NEXT zSlot%

    leftoversCount% = zBucketLeftovers%(0)
    FOR n% = 1 TO leftoversCount%
        i% = zBucketLeftovers%(n%)
        polyIndex% = polyOffset% + (i% - indexOffset%) \ 3

        DrawPoly i%, polyIndex%
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
                    PlotPixel x%, y%, 63
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
    currentPolyNormalAdd% = 0
    currentVertexNormalAdd% = 0

END SUB

SUB RunScene3D (t!)

    xt! = 0 'SIN(t!) * 48
    yt! = 0 'SIN(1.5 * t!) * 32
    zt! = 256 '+ SIN(1.25 * t!) * 96

    rx! = .7 * t!
    ry! = t!
    rz! = 1.25 * t!

    light1.x = 0
    light1.y = 0
    light1.z = 1

    SELECT CASE currentObject%

        CASE objGenCube%
            zt! = 128
            'xt! = SIN(.5 * t!) * 64
            'yt! = SIN(.75 * t!) * 64

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
            ry! = 4 * t!

        CASE objLoadMask%
            zt! = 160 + SIN(4 * t!) * 48
            rx! = -PI! / 2 + SIN(6 * t!) * .75
            ry! = PI!
            rz! = SIN(8 * t!) * .75

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

SUB SetCurrentObject (objNum%, objFile$)

    currentObject% = objNum%
    objectLoadFile$ = objFile$

END SUB

SUB SetDefaultPalette

OUT &H3C8, 0
OUT &H3C9, 0
OUT &H3C9, 0
OUT &H3C9, 0

CONST palTest% = 0

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
                OUT &H3C9, i% \ 3
                OUT &H3C9, i% \ 4
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

        CASE ELSE

    END SELECT

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

            ' Will need to investigate if it affects performance that I use LONG INT to avoid certain overflows
            ' If polygon numbers are too little, problaby not affecting much
            dx0& = x0% - x1%
            dy0& = y2% - y1%
            dx1& = x2% - x1%
            dy1& = y0% - y1%
            a& = dx0& * dy0& - dx1& * dy1&
            IF a& >= 0 THEN
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

SUB SubIVec (vSrc1 AS IVector3D, vSrc2 AS IVector3D, vDst AS IVector3D)

    vDst.x = vSrc1.x - vSrc2.x
    vDst.y = vSrc1.y - vSrc2.y
    vDst.z = vSrc1.z - vSrc2.z

END SUB

SUB TransformObject3D (obj AS Object3D)

    CONST proj! = 256

    posX! = obj.position.x
    posY! = obj.position.y
    posZ! = obj.position.z

    CalcRotMatrix obj.rotation

    offset% = obj.verticesOffset
    n% = obj.numVertices - 1
    FOR i% = 0 TO n%

        k% = i% + offset%
        xo! = objectVertices(k%).x
        yo! = objectVertices(k%).y
        zo! = objectVertices(k%).z

        z! = xo! * rotMat(2) + yo! * rotMat(5) + zo! * rotMat(8)

        zi% = INT(z! + posZ!)
        screenVertices(i%).z = zi%
        IF zi% > 0 THEN
            x! = xo! * rotMat(0) + yo! * rotMat(3) + zo! * rotMat(6)
            y! = xo! * rotMat(1) + yo! * rotMat(4) + zo! * rotMat(7)

            screenVertices(i%).x = SCRWIDTH2% + INT((x! + posX!) * proj!) \ zi%
            screenVertices(i%).y = SCRHEIGHT2% - INT((y! + posY!) * proj!) \ zi%
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

