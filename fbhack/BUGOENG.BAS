#lang "fblite"

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

TYPE PolyMaterial
    baseColor AS INTEGER
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
    polyStyle AS INTEGER

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

CONST BENCHSECS% = 15 ' put something high to not exit or bench or say 15 secs to run alone and exit with fps
CONST CONTROLS% = FALSE% ' If false, auto rotate object in RunScene3D, else let the user control position/rotation

' ---- Main Functions ----

DECLARE SUB FFIX (Mode%)

DECLARE SUB InitPrecalcs ()

DECLARE SUB SetDefaultPalette ()
DECLARE SUB SetGradPal (c0%, c1%, r0%, g0%, b0%, r1%, g1%, b1%)

DECLARE SUB ClearVram ()
DECLARE SUB WriteVram ()
DECLARE SUB WaitForVsync ()

DECLARE SUB HandleInput (dt!)

' ---- FX functions  ----

DECLARE SUB ClearBlurJunk ()
DECLARE SUB BlurVram ()
DECLARE SUB BlurVramX ()
DECLARE SUB BlurVramY ()


' ---- Vector Math functions ----

DECLARE SUB CalcRotMatrix (rot AS Vector3D)
DECLARE SUB InvertMatrix (srcMat!(), dstMat!())
DECLARE SUB MulMatWithVector3D (srcVec AS Vector3D, dstVec AS Vector3D, mat!())

DECLARE SUB SetIVec (vec AS IVector3D, x%, y%, z%)
DECLARE SUB AddIVec (vSrc1 AS IVector3D, vSrc2 AS IVector3D, vDst AS IVector3D)
DECLARE SUB SubIVec (vSrc1 AS IVector3D, vSrc2 AS IVector3D, vDst AS IVector3D)
DECLARE SUB ScaleIVec (vec AS IVector3D, scale!)
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
DECLARE SUB CalcObjectVertexNormals (obj AS Object3D)

DECLARE SUB AddPolyNormal (x%, y%, z%)
DECLARE SUB AddVertexNormal (x%, y%, z%)

DECLARE SUB CalcObjectVerticesRange (obj AS Object3D, range AS Range3D)
DECLARE SUB FindObjectScaleToFit (obj AS Object3D, desiredRange!, scale!)
DECLARE SUB ScaleObjectVertices (obj AS Object3D, scaleX!, scaleY!, scaleZ!)

DECLARE SUB CenterObjectVertices (obj AS Object3D)
DECLARE SUB FlipPolygonOrientation (obj AS Object3D)

DECLARE SUB ChangeObjectPolyMaterials (obj AS Object3D, matIndex%)

' ---- Object loading functions ----

DECLARE SUB LoadObject3D (objFile$, obj AS Object3D)
DECLARE SUB LoadObject3Dplg (objFile$, obj AS Object3D)
DECLARE SUB LoadObject3D3do (objFile$, obj AS Object3D)

DECLARE SUB ReadLineStringFromPlg (fileHandle%, bufDst$)
DECLARE SUB ReadLineValues (inp$, out$(), numVals%)


' ---- Rendering functions  ----

DECLARE SUB RenderObject3Ddots (obj AS Object3D)
DECLARE SUB RenderObject3Dwire (obj AS Object3D)
DECLARE SUB RenderObject3Dpoly (obj AS Object3D)
DECLARE SUB SortObject3Dpoly (obj AS Object3D)

DECLARE SUB PlotPixel (x%, y%, col%)
DECLARE SUB DrawLine (x0%, y0%, x1%, y1%, c%)
DECLARE SUB DrawPoly (i%, polyOffset%, polyIndex%, polyStyle%)

DECLARE SUB CalcTriangleGrads (doGouraud%, doTextured%)

DECLARE SUB PrepareEdgeListsFlat ()
DECLARE SUB PrepareEdgeListsGouraud ()
DECLARE SUB PrepareEdgeListsTextured ()
DECLARE SUB PrepareEdgeListsTexturedGouraud ()

DECLARE SUB CalcObjectPolyLighting (obj AS Object3D, light AS Vector3D)
DECLARE SUB CalcObjectVertexLighting (obj AS Object3D, light AS Vector3D)

DECLARE SUB RotateObjectVertexNormals (obj AS Object3D, mat!())
DECLARE SUB CalcObjectEnvmapTC (obj AS Object3D)

DECLARE SUB DrawEdgesFlat (col%)
DECLARE SUB DrawEdgesGouraud (col%)
DECLARE SUB DrawEdgesTextured (col%)
DECLARE SUB DrawEdgesTexturedGouraud (col%)


' ---- Object transformation functions ----

DECLARE SUB SetObjectPosition (obj AS Object3D, px!, py!, pz!)
DECLARE SUB SetObjectRotation (obj AS Object3D, rx!, ry!, rz!)
DECLARE SUB RenderObject3D (obj AS Object3D)
DECLARE SUB TransformObject3D (obj AS Object3D)


' ---- Scene management functions ----

DECLARE SUB SetCurrentObject (objNum%, objFile$)
DECLARE SUB SetLightPosition (x!, y!, z!, index%)

DECLARE SUB InitScene3D ()
DECLARE SUB RunScene3D (t!)

DECLARE SUB GenEnvmapTex (num%)

' ---- Engine Consts ----

CONST BLURSWITCH% = 0   ' blur will overflow in interpreter only compiled exe

CONST FPRAST% = 64
CONST LFPRAST& = 65536  ' experimental for higher texture interpolator precision but slower along with two functions using it, might not use it
CONST FPNORM% = 64

CONST defaultScaleRange! = 1024

CONST renderTypeDots% = 0
CONST renderTypeWire% = 1
CONST renderTypePoly% = 2

CONST polyStyleFlat% = 0
CONST polyStyleGouraud% = 1
CONST polyStyleTextured% = 2
CONST polyStyleTexturedGouraud% = 3

CONST ZBUCKETSIZE% = 16
CONST ZBUCKETMAX% = 2048
CONST ZBUCKETS% = ZBUCKETMAX% / ZBUCKETSIZE%
CONST ZBUCKETPOLYS% = 64
CONST LEFTOVERSSIZE% = 512

' Some default values for now
CONST ENVTEXWIDTH% = 128
CONST ENVTEXHEIGHT% = 128

CONST MATERIALWIDTH% = 64
CONST DIFFUSEWIDTH% = 3 * MATERIALWIDTH% \ 4
CONST SPECULARWIDTH% = MATERIALWIDTH% - DIFFUSEWIDTH%


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

DIM SHARED vram%(0 TO 32007)
vram%(6) = 320 * 8
vram%(7) = 200

dim shared pixel as byte ptr

CONST DivFPRASTrange% = SCRWIDTH% * FPRAST%
CONST DivFPRASTneg% = -64 * FPRAST%

DIM SHARED DivFPRAST%(DivFPRASTneg% TO DivFPRASTrange% - 1)

DIM SHARED blobTex%(0 TO ENVTEXWIDTH% * ENVTEXHEIGHT% - 1)

'$STATIC


' ---- Object pool sizes ----

CONST maxObjVertices% = 2600
CONST maxObjPolys% = 2 * maxObjVertices%
CONST avgObjVertexNum% = maxObjVertices%
CONST maxObjectsOnScreen% = 1
CONST maxVertices% = maxObjectsOnScreen% * avgObjVertexNum%
CONST maxPolys% = 2 * maxVertices%
CONST maxPolyIndices% = 3 * maxPolys%
CONST maxLineIndices% = 6 * maxPolys%

CONST numLights% = 3

' ---- Object arrays and engine shared variables ----

CONST objGenCube% = 0
CONST objGenDotCube% = 1
CONST objGenSphere% = 2
CONST objGenTorus% = 3
CONST objLoadTest% = 4
CONST objLoadTeapot% = 5
CONST objLoadMask% = 6
CONST numObjects% = 7
CONST numMaterials = 16

DIM SHARED object(0 TO numObjects% - 1) AS Object3D

DIM SHARED material(0 TO numMaterials% - 1) AS PolyMaterial

DIM SHARED screenVertices(0 TO maxObjVertices% - 1) AS Element3D
DIM SHARED screenPolyShades%(0 TO maxObjPolys% - 1)
DIM SHARED transformedVertexNormals(0 TO maxObjVertices% - 1) AS IVector3D

DIM SHARED objectVertices(0 TO maxVertices% - 1) AS Vertex3D
DIM SHARED objectPolyIndices%(0 TO maxPolyIndices% - 1)
DIM SHARED objectLineIndices%(0 TO maxLineIndices% - 1)
DIM SHARED objectPolyMatIndices%(0 TO maxPolys% - 1)

DIM SHARED objectPolyNormals(0 TO maxPolys% - 1) AS IVector3D
DIM SHARED objectVertexNormals(0 TO maxVertices% - 1) AS IVector3D

DIM SHARED currentVertexAdd%
DIM SHARED currentPolyIndexAdd%
DIM SHARED currentLineIndexAdd%
DIM SHARED currentPolyAdd%
DIM SHARED currentPolyNormalAdd%
DIM SHARED currentVertexNormalAdd%

DIM SHARED edgeYmin%, edgeYmax%
DIM SHARED polyEdgeLeft(0 TO SCRHEIGHT% - 1) AS Edge
DIM SHARED polyEdgeRightX%(0 TO SCRHEIGHT% - 1)

DIM SHARED ey%(0 TO 3)
DIM SHARED ex%(0 TO 3)
DIM SHARED ec%(0 TO 3)
DIM SHARED eu%(0 TO 3)
DIM SHARED ev%(0 TO 3)

DIM SHARED gradC%, gradU%, gradV%

DIM SHARED rotMat(0 TO 8) AS SINGLE
DIM SHARED invRotMat(0 TO 8) AS SINGLE

DIM SHARED lights(0 TO numLights% - 1) AS Vector3D


' ---- Object Selector ----

DIM SHARED currentObject%
DIM SHARED objectLoadFile$

CLS
'FILES "*.plg"
'FILES "*.3do"
PRINT
INPUT "Load: ", objFile$

IF objFile$ = "" THEN
    SetCurrentObject objGenCube%, ""
ELSE
    SetCurrentObject objLoadTest%, objFile$
END IF


DIM SHARED quit%
quit% = FALSE%

' ---- Init ----

' Load FFIX to memory
'FFIX 0

InitPrecalcs

SCREEN 13,8,2

COLOR 63

SetDefaultPalette

'SetGradPal 0, 31, 15, 7, 31, 127, 63, 191
'SetGradPal 32, 63, 127, 63, 191, 255, 255, 255


ResetElementAddIndices
InitScene3D


' ---- Main Loop ----

freeMem& = FRE(-1)

nframe& = 0


t! = 0

timeStart! = TIMER
timeDiff! = 0
lastTime! = timeStart!
DO WHILE quit% = FALSE% AND timeElapsed! < BENCHSECS%

    ClearVram

    HandleInput timeDiff!

    pixel = screenptr

    RunScene3D (2 * t!)

    flp% = (flp% + 1) AND 1
    SCREENSET flp%, ((flp% + 1) AND 1)

    'WaitForVsync

    'WriteVram

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

SUB AddIVec (vSrc1 AS IVector3D, vSrc2 AS IVector3D, vDst AS IVector3D)

    vDst.x = vSrc1.x + vSrc2.x
    vDst.y = vSrc1.y + vSrc2.y
    vDst.z = vSrc1.z + vSrc2.z

END SUB

SUB AddLineIndices (i0%, i1%)

    IF currentLineIndexAdd% <= maxLineIndices% - 2 THEN
        objectLineIndices%(currentLineIndexAdd%) = i0%
        objectLineIndices%(currentLineIndexAdd% + 1) = i1%

        currentLineIndexAdd% = currentLineIndexAdd% + 2
    END IF

END SUB

SUB AddPolyNormal (x%, y%, z%)

    IF currentPolyNormalAdd% <= maxPolys% - 3 THEN
        objectPolyNormals(currentPolyNormalAdd%).x = x%
        objectPolyNormals(currentPolyNormalAdd%).y = y%
        objectPolyNormals(currentPolyNormalAdd%).z = z%

        currentPolyNormalAdd% = currentPolyNormalAdd% + 1
    END IF

END SUB

SUB AddTriangleIndices (i0%, i1%, i2%)

    IF currentPolyIndexAdd% <= maxPolyIndices% - 3 THEN
        objectPolyIndices%(currentPolyIndexAdd%) = i0%
        objectPolyIndices%(currentPolyIndexAdd% + 1) = i1%
        objectPolyIndices%(currentPolyIndexAdd% + 2) = i2%

        currentPolyIndexAdd% = currentPolyIndexAdd% + 3
    END IF

END SUB

SUB AddVertex (x%, y%, z%)

    IF currentVertexAdd% <= maxVertices% - 3 THEN
        objectVertices(currentVertexAdd%).x = x%
        objectVertices(currentVertexAdd%).y = y%
        objectVertices(currentVertexAdd%).z = z%

        currentVertexAdd% = currentVertexAdd% + 1
    END IF

END SUB

SUB AddVertexNormal (x%, y%, z%)

    IF currentVertexNormalAdd% <= maxVertices% - 3 THEN
        objectVertexNormals(currentVertexNormalAdd%).x = x%
        objectVertexNormals(currentVertexNormalAdd%).y = y%
        objectVertexNormals(currentVertexNormalAdd%).z = z%

        currentVertexNormalAdd% = currentVertexNormalAdd% + 1
    END IF

END SUB

SUB BlurVram

    ' Dirty blur two pixels at once (works only if colors are from 0 to 63)
    FOR i% = 8 + 2 + SCRWIDTH2% TO 8 + 2 + SCRWIDTH2% * (SCRHEIGHT% - 1) - 1
        vram%(i%) = ((vram%(i% - 1) + vram%(i% + 1) + vram%(i% - SCRWIDTH2%) + vram%(i% + SCRWIDTH2%)) \ 4) AND &H3F3F
    NEXT i%

    ClearBlurJunk

END SUB

SUB BlurVramX

    ' Dirty blur two pixels at once (works only if colors are from 0 to 127)
    FOR i% = 8 + 2 + SCRWIDTH% \ 2 TO 8 + 2 + (SCRWIDTH% \ 2) * (SCRHEIGHT% - 1) - 1
        vram%(i%) = ((vram%(i% - 1) + vram%(i% + 1)) \ 2) AND &H3F3F
    NEXT i%

    ClearBlurJunk

END SUB

SUB BlurVramY

    ' Dirty blur two pixels at once (works only if colors are from 0 to 127)
    FOR i% = 8 + 2 + SCRWIDTH2% TO 8 + 2 + SCRWIDTH2% * (SCRHEIGHT% - 1) - 1
        vram%(i%) = ((vram%(i% - SCRWIDTH2%) + vram%(i% + SCRWIDTH2%)) \ 2) AND &H3F3F
    NEXT i%

    ClearBlurJunk

END SUB

SUB CalcObjectEnvmapTC (obj AS Object3D)

    CONST HALFENVTEXWIDTH% = ENVTEXWIDTH% \ 2
    CONST HALFENVTEXHEIGHT% = ENVTEXHEIGHT% \ 2

    CONST ENVMULX% = ENVTEXWIDTH% \ 2
    CONST ENVMULY% = ENVTEXHEIGHT% \ 2

    n% = obj.numVertices - 1

    FOR i% = 0 TO n%

        vnX% = transformedVertexNormals(i%).x
        vnY% = transformedVertexNormals(i%).y
        vnZ% = transformedVertexNormals(i%).z

        IF vnZ% < 0 THEN vnZ% = 0

        u% = HALFENVTEXWIDTH% * FPRAST% + (vnX% * ENVMULX%) \ (FPRAST% \ FPNORM%)
        v% = HALFENVTEXHEIGHT% * FPRAST% + (vnY% * ENVMULY%) \ (FPRAST% \ FPNORM%)
        IF u% < 0 THEN u% = 0
        IF u% > ENVTEXWIDTH% * FPRAST% - 1 THEN u% = ENVTEXWIDTH% * FPRAST% - 1
        IF v% < 0 THEN v% = 0
        IF v% > ENVTEXHEIGHT% * FPRAST% - 1 THEN v% = ENVTEXHEIGHT% * FPRAST% - 1

        screenVertices(i%).u = u%
        screenVertices(i%).v = v%

    NEXT i%

END SUB

SUB CalcObjectPolyLighting (obj AS Object3D, light AS Vector3D)

    DIM rotLight AS Vector3D
    DIM iLight AS IVector3D

    MulMatWithVector3D light, rotLight, invRotMat()
    SetIVec iLight, INT(rotLight.x * FPNORM%), INT(rotLight.y * FPNORM%), INT(rotLight.z * FPNORM%)

    offset% = obj.polyNormalsOffset
    n% = obj.numPolys - 1

    FOR i% = 0 TO n%
        DotIVec objectPolyNormals(offset% + i%), iLight, d%

        IF d% < 0 THEN d% = 0
        IF d% > FPNORM% THEN d% = FPNORM%

        screenPolyShades%(i%) = ((MATERIALWIDTH% - 1) * d%) \ FPNORM%
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

SUB CalcObjectVertexLighting (obj AS Object3D, light AS Vector3D)

    DIM rotLight AS Vector3D
    DIM iLight AS IVector3D

    MulMatWithVector3D light, rotLight, invRotMat()
    SetIVec iLight, INT(rotLight.x * FPNORM%), INT(rotLight.y * FPNORM%), INT(rotLight.z * FPNORM%)

    offset% = obj.vertexNormalsOffset
    n% = obj.numVertices - 1

    FOR i% = 0 TO n%
        DotIVec objectVertexNormals(offset% + i%), iLight, d%

        IF d% < 0 THEN d% = 0
        IF d% > FPNORM% THEN d% = FPNORM%

        screenVertices(i%).c = ((DIFFUSEWIDTH% - 1) * d%) * (FPRAST% \ FPNORM%)
    NEXT i%

END SUB

SUB CalcObjectVertexNormals (obj AS Object3D)

    piOffset% = obj.polyIndicesOffset
    pnOffset% = obj.polyNormalsOffset
    n% = offset% + obj.numPolyIndices - 1

    DIM norm AS IVector3D

    FOR j% = 0 TO obj.numVertices - 1
        SetIVec norm, 0, 0, 0
        pCount% = 0

        polyNormalIndex% = pnOffset%
        FOR i% = piOffset% TO n% STEP 3

            i0% = objectPolyIndices%(i%)
            i1% = objectPolyIndices%(i% + 1)
            i2% = objectPolyIndices%(i% + 2)

            IF j% = i0% OR j% = i1% OR j% = i2% THEN
                AddIVec norm, objectPolyNormals(polyNormalIndex%), norm
                pCount% = pCount% + 1
            END IF

            polyNormalIndex% = polyNormalIndex% + 1

        NEXT i%

        IF pCount% > 0 THEN
            ScaleIVec norm, 1! / pCount%
            NormIVec norm
            AddVertexNormal norm.x, norm.y, norm.z
        END IF
    NEXT j%

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

SUB CalcTriangleGrads (doGouraud%, doTextured%)

    dys0& = ey%(0) - ey%(2)
    dys1& = ey%(1) - ey%(2)
    dd& = (ex%(1) - ex%(2)) * dys0& - (ex%(0) - ex%(2)) * dys1&

    IF dd& = 0 THEN EXIT SUB

    IF doGouraud% = TRUE% THEN
        gradC% = (((ec%(1) - ec%(2)) * dys0& - (ec%(0) - ec%(2)) * dys1&) * FPRAST%) \ dd&
    END IF

    IF doTextured% = TRUE% THEN
        gradU% = (((eu%(1) - eu%(2)) * dys0& - (eu%(0) - eu%(2)) * dys1&) * FPRAST%) \ dd&
        gradV% = (((ev%(1) - ev%(2)) * dys0& - (ev%(0) - ev%(2)) * dys1&) * FPRAST%) \ dd&
    END IF

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

SUB ChangeObjectPolyMaterials (obj AS Object3D, matIndex%)

    FOR i% = 0 TO obj.numPolys - 1
        objectPolyMatIndices%(i%) = matIndex%
    NEXT i%

END SUB

SUB ClearBlurJunk

    ' Clear junk at top scanline
    FOR i% = 8 + 2 TO 8 + 2 + SCRWIDTH2% - 1
        vram%(i%) = 0
    NEXT i%

    ' Clear junk at bottom scanline
    FOR i% = 8 + 2 + SCRWIDTH2% * (SCRHEIGHT% - 1) TO 8 + 2 + SCRWIDTH2% * SCRHEIGHT% - 1
        vram%(i%) = 0
    NEXT i%

END SUB

SUB ClearVram

    'REDIM vram%(0 TO 32007)

    'vram%(6) = 320 * 8
    'vram%(7) = 200
    pixel=screenptr
    for i&=0 to 64000
        pixel[i&]=0
    next i&

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

SUB DotIVec (vSrc1 AS IVector3D, vSrc2 AS IVector3D, iDst%)
   
    v1x& = vSrc1.x: v1y& = vSrc1.y: v1z& = vSrc1.z
    v2x& = vSrc2.x: v2y& = vSrc2.y: v2z& = vSrc2.z

    iDst% = (v1x& * v2x&) \ FPNORM% + (v1y& * v2y&) \ FPNORM% + (v1z& * v2z&) \ FPNORM%

END SUB

SUB DrawEdgesFlat (col%)

    'screenSeg& = VARSEG(vram%(0)) + 1
    'pixel = screenptr
    ys% = edgeYmin% * SCRWIDTH% '(SCRWIDTH% \ 16)

    yEnd% = edgeYmax% - 1 ' one less edge bottom to avoid overdraw
    FOR y% = edgeYmin% TO yEnd%
        xs0% = polyEdgeLeft(y%).x
        xs1% = polyEdgeRightX%(y%)

        IF xs0% < 0 THEN xs0% = 0
        IF xs1% > FPRAST% * (SCRWIDTH% - 1) THEN xs1% = FPRAST% * (SCRWIDTH% - 1)

        IF xs1% > xs0% THEN

            xs0% = DivFPRAST%(xs0%)
            xs1% = DivFPRAST%(xs1%) - 1 ' one pixel less right to avoid overdraw

            'DEF SEG = screenSeg& + ys%
            pixel = screenptr + ys%

            'Original per pixel loop
            FOR x% = xs0% TO xs1%
                'POKE x%, col%
                pixel[x%] = col%
            NEXT x%

            ' Split loop to do two POKEs in one loop unroled and previous the left/right single byte edges if needed
            'IF xs0% AND 1 THEN
                'POKE xs0%, col%
                'xs0% = xs0% + 1
            'END IF

            'IF NOT xs1% AND 1 THEN
                'POKE xs1%, col%
                'xs1% = xs1% - 1
            'END IF

            'FOR x% = xs0% TO xs1% STEP 2
                'POKE x%, col%
                'POKE x% + 1, col%
            'NEXT x%

        END IF

        ys% = ys% + SCRWIDTH% '(SCRWIDTH% \ 16)
    NEXT y%

END SUB

SUB DrawEdgesGouraud (col%)

    'screenSeg& = VARSEG(vram%(0)) + 1
    'ys% = edgeYmin% * (SCRWIDTH% \ 16)

    'pixel = screenptr
    ys% = edgeYmin% * SCRWIDTH%

    col% = col% * FPRAST%

    yEnd% = edgeYmax% - 1 ' one less edge bottom to avoid overdraw
    FOR y% = edgeYmin% TO yEnd%
        xs0% = polyEdgeLeft(y%).x
        xs1% = polyEdgeRightX%(y%)

        IF xs1% >= xs0% THEN
            IF xs0% >= 0 OR xs1% >= 0 THEN
                c0% = col% + polyEdgeLeft(y%).c

                IF xs0% < 0 THEN
                    c0% = c0% + DivFPRAST%(-xs0%) * gradC%
                    xs0% = 0
                END IF

                IF xs1% > FPRAST% * (SCRWIDTH% - 1) THEN xs1% = FPRAST% * (SCRWIDTH% - 1)

                IF xs1% >= xs0% THEN
                    'DEF SEG = screenSeg& + ys%
                    pixel = screenptr + ys%

                    xs0% = DivFPRAST%(xs0%)
                    xs1% = DivFPRAST%(xs1%)

                    FOR x% = xs0% TO xs1% - 1     ' one less pixel to avoid edge overdraw
                        pixel[x%] = DivFPRAST%(c0%)
                        c0% = c0% + gradC%
                    NEXT x%

                END IF
            END IF
        END IF

        ys% = ys% + SCRWIDTH%
    NEXT y%

END SUB

SUB DrawEdgesTextured (col%)

'pixel = screenptr
ys% = edgeYmin% * SCRWIDTH%

    yEnd% = edgeYmax% - 1 ' one less edge bottom to avoid overdraw
    FOR y% = edgeYmin% TO yEnd%
        xs0% = polyEdgeLeft(y%).x
        xs1% = polyEdgeRightX%(y%)

        IF xs1% >= xs0% THEN
            IF xs0% >= 0 OR xs1% >= 0 THEN
                u0% = polyEdgeLeft(y%).u
                v0% = polyEdgeLeft(y%).v

                IF xs0% < 0 THEN
                    u0% = u0% + DivFPRAST%(-xs0%) * gradU%
                    v0% = v0% + DivFPRAST%(-xs0%) * gradV%
                    xs0% = 0
                END IF

                IF xs1% > FPRAST% * (SCRWIDTH% - 1) THEN xs1% = FPRAST% * (SCRWIDTH% - 1)

                IF xs1% >= xs0% THEN
                    'DEF SEG = screenSeg& + ys%
                    pixel = screenptr + ys%

                    xs0% = DivFPRAST%(xs0%)
                    xs1% = DivFPRAST%(xs1%)

                    FOR x% = xs0% TO xs1% - 1       ' one less pixel to avoid edge overdraw
                        u% = DivFPRAST%(u0%)
                        v% = DivFPRAST%(v0%)
                        pixel[x%] = col% + blobTex%(((v% AND (ENVTEXHEIGHT% - 1)) * ENVTEXWIDTH%) + (u% AND (ENVTEXWIDTH% - 1)))
                        u0% = u0% + gradU%
                        v0% = v0% + gradV%
                    NEXT x%

                END IF
            END IF
        END IF

        ys% = ys% + SCRWIDTH%
    NEXT y%

END SUB

SUB DrawEdgesTexturedGouraud (col%)

    'screenSeg& = VARSEG(vram%(0)) + 1
    'ys% = edgeYmin% * (SCRWIDTH% \ 16)

    'pixel = screenptr
    ys% = edgeYmin% * SCRWIDTH%

    yEnd% = edgeYmax% - 1 ' one less edge bottom to avoid overdraw
    FOR y% = edgeYmin% TO yEnd%
        xs0% = polyEdgeLeft(y%).x
        xs1% = polyEdgeRightX%(y%)

        IF xs1% >= xs0% THEN
            IF xs0% >= 0 OR xs1% >= 0 THEN
                c0% = polyEdgeLeft(y%).c

                u0% = polyEdgeLeft(y%).u
                v0% = polyEdgeLeft(y%).v

                IF xs0% < 0 THEN
                    c0% = c0% + DivFPRAST%(-xs0%) * gradC%
                    u0% = u0% + DivFPRAST%(-xs0%) * gradU%
                    v0% = v0% + DivFPRAST%(-xs0%) * gradV%
                    xs0% = 0
                END IF

                IF xs1% > FPRAST% * (SCRWIDTH% - 1) THEN xs1% = FPRAST% * (SCRWIDTH% - 1)

                IF xs1% >= xs0% THEN
                    'DEF SEG = screenSeg& + ys%
                    pixel = screenptr + ys%

                    xs0% = DivFPRAST%(xs0%)
                    xs1% = DivFPRAST%(xs1%)

                    FOR x% = xs0% TO xs1% - 1       ' one less pixel to avoid edge overdraw
                        u% = DivFPRAST%(u0%)
                        v% = DivFPRAST%(v0%)
                        pixel[x%] = col% + DivFPRAST%(c0%) + blobTex%(((v% AND (ENVTEXHEIGHT% - 1)) * ENVTEXWIDTH%) + (u% AND (ENVTEXWIDTH% - 1)))
                        c0% = c0% + gradC%
                        u0% = u0% + gradU%
                        v0% = v0% + gradV%
                    NEXT x%

                END IF
            END IF
        END IF

        ys% = ys% + SCRWIDTH%
    NEXT y%

END SUB

SUB DrawLine (x0%, y0%, x1%, y1%, c%)

    'screenSeg& = VARSEG(vram%(0)) + 1
    'pixel = screenptr

    dx% = x1% - x0%
    dy% = y1% - y0%

    lx% = ABS(dx%)
    ly% = ABS(dy%)

    n% = 1
    IF ly% < lx% THEN
        IF dx% <> 0 THEN l% = (dy% * FPRAST%) \ dx%
        IF x0% > x1% THEN n% = -1
        ns% = n% * l%

        yy% = y0% * FPRAST%
        FOR x% = x0% TO x1% STEP n%
            'DEF SEG = screenSeg& + DivFPRAST%(yy%) * 20
            pixel = screenptr + DivFPRAST%(yy%) * 320
            'POKE x%, c%
            pixel[x%] = c%
            yy% = yy% + ns%
        NEXT x%
    ELSE
        IF dy% <> 0 THEN l% = (dx% * FPRAST%) \ dy%
        IF y0% > y1% THEN n% = -1
        ns% = n% * l%

        xx% = x0% * FPRAST%
        FOR y% = y0% TO y1% STEP n%
            'DEF SEG = screenSeg& + y% * 20
            pixel = screenptr + y% * 320
            'POKE DivFPRAST%(xx%), c% - 4 * stopper%
            pixel[DivFPRAST%(xx%)] = c% - 4 * stopper%
            xx% = xx% + ns%
        NEXT y%
    END IF

END SUB

SUB DrawPoly (i%, polyOffset%, polyIndex%, polyStyle%)

    i0% = objectPolyIndices%(i%)
    i1% = objectPolyIndices%(i% + 1)
    i2% = objectPolyIndices%(i% + 2)

    ex%(0) = screenVertices(i0%).x
    ex%(1) = screenVertices(i1%).x
    ex%(2) = screenVertices(i2%).x
    ey%(0) = screenVertices(i0%).y \ FPRAST%
    ey%(1) = screenVertices(i1%).y \ FPRAST%
    ey%(2) = screenVertices(i2%).y \ FPRAST%

    IF polyStyle% AND polyStyleGouraud% <> 0 THEN
        ec%(0) = screenVertices(i0%).c
        ec%(1) = screenVertices(i1%).c
        ec%(2) = screenVertices(i2%).c
    END IF

    IF polyStyle% AND polyStyleTextured% <> 0 THEN
        eu%(0) = screenVertices(i0%).u
        eu%(1) = screenVertices(i1%).u
        eu%(2) = screenVertices(i2%).u
        ev%(0) = screenVertices(i0%).v
        ev%(1) = screenVertices(i1%).v
        ev%(2) = screenVertices(i2%).v
    END IF

    min% = ey%(0)
    max% = min%
    IF ey%(1) < min% THEN min% = ey%(1) ELSE IF ey%(1) > max% THEN max% = ey%(1)
    IF ey%(2) < min% THEN min% = ey%(2) ELSE IF ey%(2) > max% THEN max% = ey%(2)
    edgeYmin% = min%
    edgeYmax% = max%

    IF edgeYmin% < 0 THEN edgeYmin% = 0
    IF edgeYmax% > SCRHEIGHT% - 1 THEN edgeYmax% = SCRHEIGHT% - 1

    IF edgeYmax% > edgeYmin% THEN

        matIndex% = objectPolyMatIndices%(polyOffset% + polyIndex%)
        colBase% = material(matIndex%).baseColor

        SELECT CASE polyStyle%

            CASE polyStyleFlat%
                PrepareEdgeListsFlat
                DrawEdgesFlat colBase% + screenPolyShades%(polyIndex%)

            CASE polyStyleGouraud%
                PrepareEdgeListsGouraud
                CalcTriangleGrads TRUE%, FALSE%
                DrawEdgesGouraud colBase%

            CASE polyStyleTextured%
                PrepareEdgeListsTextured
                CalcTriangleGrads FALSE%, TRUE%
                DrawEdgesTextured colBase%

            CASE polyStyleTexturedGouraud%
                PrepareEdgeListsTexturedGouraud
                CalcTriangleGrads TRUE%, TRUE%
                DrawEdgesTexturedGouraud colBase%

            CASE ELSE
        END SELECT
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
        k% = objectPolyIndices%(i%)
        objectPolyIndices%(i%) = objectPolyIndices%(i% + 2)
        objectPolyIndices%(i% + 2) = k%
    NEXT i%

END SUB

SUB GenEnvmapTex (num%)

    SELECT CASE num%

        CASE 0
            i% = 0
            FOR y% = 0 TO ENVTEXHEIGHT% - 1
                yc% = y% - ENVTEXHEIGHT% \ 2
                FOR x% = 0 TO ENVTEXWIDTH% - 1
                    xc% = x% - ENVTEXWIDTH% \ 2
                    r& = SQR(xc% * xc% + yc% * yc%)
                    r& = r& * r&
                    c& = SPECULARWIDTH% - 1
                    IF r& > 0 THEN
                        c& = 1024 \ r&
                    END IF

                    IF c& < 0 THEN c& = 0
                    IF c& > SPECULARWIDTH% - 1 THEN c& = SPECULARWIDTH% - 1

                    blobTex%(i%) = c&
                    i% = i% + 1
                NEXT x%
            NEXT y%

        CASE 1
            i% = 0
            FOR y% = 0 TO ENVTEXHEIGHT% - 1
                FOR x% = 0 TO ENVTEXWIDTH% - 1
                    c% = (SIN(x% * .5) + COS(y% * .375) + SIN(x% * .625 + y% * .25)) * SPECULARWIDTH% / 2 + SPECULARWIDTH% / 2

                    IF c% < 0 THEN c% = 0
                    IF c% > SPECULARWIDTH% - 1 THEN c% = SPECULARWIDTH% - 1

                    blobTex%(i%) = c%
                    i% = i% + 1
                NEXT x%
            NEXT y%

        CASE ELSE

    END SELECT

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
    CalcObjectVertexNormals obj

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

SUB HandleInput (dt!)

    CONST zBase! = defaultScaleRange!
    CONST minAllowedZ! = defaultScaleRange! / 2
    CONST maxAllowedZ! = 4 * defaultScaleRange! - minAllowedZ!
    CONST posMove! = 16
    CONST zoomMove! = 64
    CONST rotSpeed! = .05

    STATIC posX!, posY!, posZ!
    STATIC rotX!, rotY!, rotZ!
    STATIC rotVelX!, rotVelY!, rotVelZ!

    STATIC currentMatIndex%


    key$ = INKEY$

    IF key$ <> "" THEN
        IF ASC(key$) = 27 THEN
            quit% = TRUE%
            EXIT SUB
        END IF
    END IF

    SELECT CASE key$

        CASE "q", "Q"
            object(currentObject%).renderType = renderTypeDots%

        CASE "w", "W"
            object(currentObject%).renderType = renderTypeWire%

        CASE "e", "E"
            object(currentObject%).renderType = renderTypePoly%

        CASE "a", "A"
            object(currentObject%).polyStyle = polyStyleFlat%

        CASE "s", "S"
            object(currentObject%).polyStyle = polyStyleGouraud%

        CASE "d", "D"
            object(currentObject%).polyStyle = polyStyleTextured%

        CASE "f", "F"
            object(currentObject%).polyStyle = polyStyleTexturedGouraud%

        CASE "p", "P"
            currentMatIndex% = (currentMatIndex% + 1) AND 3
            ChangeObjectPolyMaterials object(currentObject%), currentMatIndex%

        CASE "z", "Z"
            rotX! = rotX! + PI! / 2

        CASE "x", "X"
            rotY! = rotY! + PI! / 2

        CASE "y", "Y"
            rotZ! = rotZ! + PI! / 2

    CASE ELSE

    END SELECT

    IF CONTROLS% = TRUE% THEN

        SELECT CASE key$

            CASE "4"
                rotVelY! = rotVelY! - rotSpeed!

            CASE "6"
                rotVelY! = rotVelY! + rotSpeed!

            CASE "8"
                rotVelX! = rotVelX! - rotSpeed!

            CASE "2"
                rotVelX! = rotVelX! + rotSpeed!

            CASE "7"
                rotVelZ! = rotVelZ! - rotSpeed!

            CASE "9"
                rotVelZ! = rotVelZ! + rotSpeed!

            CASE "+"
                posZ! = posZ! - zoomMove!

            CASE "-"
                posZ! = posZ! + zoomMove!

            CASE "j", "J"
                posX! = posX! - posMove!

            CASE "l", "L"
                posX! = posX! + posMove!

            CASE "i", "I"
                posY! = posY! + posMove!

            CASE "k", "K"
                posY! = posY! - posMove!

            CASE "5"
                rotVelX! = 0: rotVelY! = 0: rotVelZ! = 0

            CASE "0"
                posX! = 0: posY! = 0: posZ! = 0
                rotX! = 0: rotY! = 0: rotZ! = 0
                rotVelX! = 0: rotVelY! = 0: rotVelZ! = 0

            CASE ELSE

        END SELECT

        IF zBase! + posZ! < minAllowedZ! THEN posZ! = minAllowedZ! - zBase!
        IF zBase! + posZ! > maxAllowedZ! THEN posZ! = maxAllowedZ! - zBase!

        SetObjectPosition object(currentObject%), posX!, posY!, zBase! + posZ!
        SetObjectRotation object(currentObject%), rotX!, rotY!, rotZ!

        rotX! = rotX! + dt! * rotVelX!
        rotY! = rotY! + dt! * rotVelY!
        rotZ! = rotZ! + dt! * rotVelZ!

    END IF

END SUB

SUB InitPrecalcs

' LUT to divide by FPRAST% as qbasic doesn't convert integer division by power of two to shifts

    FOR i% = DivFPRASTneg% TO DivFPRASTrange% - 1
        IF i% > 0 THEN
            DivFPRAST%(i%) = i% \ FPRAST%
        ELSE
            DivFPRAST%(i%) = 0
        END IF
    NEXT i%

' Some materials

    FOR i% = 0 TO 3
        material(i%).baseColor = i% * MATERIALWIDTH%
    NEXT i%

' Texture for envmap

    GenEnvmapTex 0

END SUB

SUB InitScene3D

    DIM genParams AS ObjGenParams

    object(currentObject%).renderType = renderTypePoly%
    object(currentObject%).polyStyle = polyStyleFlat%

    SELECT CASE currentObject%

        CASE objGenCube%
            genParams.size = 256

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
        ScaleObjectVertices object(currentObject%), scaleValue!, scaleValue!, scaleValue!

        CalcObjectPolyNormals object(currentObject%)
        CalcObjectVertexNormals object(currentObject%)
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

    ' Should detect file extension, also later do something with orientation and different 3DO file formats or crashes

    ext$ = RIGHT$(objFile$, 3)

    SELECT CASE ext$

        CASE "plg"
            LoadObject3Dplg objFile$, obj

        CASE "3do"
            LoadObject3D3do objFile$, obj

        CASE ELSE

    END SELECT

END SUB

SUB LoadObject3D3do (objFile$, obj AS Object3D)

    fileVersion% = 2

    obj.verticesOffset = currentVertexAdd%
    obj.polyIndicesOffset = currentPolyIndexAdd%
    obj.lineIndicesOffset = currentLineIndexAdd%
    obj.polysOffset = currentPolyAdd%
    obj.polyNormalsOffset = currentPolyNormalAdd%
    obj.vertexNormalsOffset = currentVertexNormalAdd%

    OPEN objFile$ FOR BINARY AS #1

    DIM ndts AS short
    DIM nlns AS short
    DIM npls AS short

    DIM i0 as short
    DIM i1 as short
    DIM i2 as short

    GET #1, , ndts
    GET #1, , nlns
    GET #1, , npls

    a& = ndts: b& = nlns: c& = npls
    s& = a& * 3 + b& * 2 * 2 + c& * 3 * 2 + 6
    flen& = LOF(1)
    IF flen& <> s& THEN
        s& = a& * 3 * 4 + b& * 3 * 2 + 4  ' what if lines are missing and 4 bytes per vertex
        IF s& = flen& THEN
            fileVersion% = 1
            npls = nlns
            nlns = 0
            SEEK #1, SEEK(1) - 2
        END IF
    END IF

    obj.numVertices = ndts
    obj.numPolys = npls
    obj.numPolyIndices = npls * 3
    obj.numLineIndices = nlns * 2

    IF fileVersion% = 1 THEN
        FOR i% = 0 TO ndts - 1
            GET #1, , x&
            GET #1, , y&
            GET #1, , z&
            AddVertex INT(x& \ 65536), INT(y& \ 65536), INT(z& \ 65536)
        NEXT i%
    ELSE
        c$ = " "

        FOR i% = 0 TO ndts - 1
            GET #1, , c$: x% = ASC(c$) - 128
            GET #1, , c$: y% = ASC(c$) - 128
            GET #1, , c$: z% = ASC(c$) - 128
            AddVertex x%, y%, z%
        NEXT i%
    END IF

    FOR i% = 0 TO nlns - 1
        GET #1, , i0
        GET #1, , i1
        AddLineIndices int(i0), int(i1)
    NEXT i%

    FOR i = 0 TO npls - 1
        GET #1, , i0
        GET #1, , i1
        GET #1, , i2
        AddTriangleIndices int(i0), int(i1), int(i2)
    NEXT i

    CLOSE #1

    CenterObjectVertices obj

    ScaleObjectVertices obj, 1, -1, 1   ' all come flipped on Y

    ' hack for certain objects
    IF NOT objFile$ = "duck.3do" AND NOT objFile$ = "duck2.3do" THEN
        FlipPolygonOrientation obj
    END IF

END SUB

SUB LoadObject3Dplg (objFile$, obj AS Object3D)

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

        dstVec.x = x! * mat!(0) + y! * mat!(3) + z! * mat!(6)
        dstVec.y = x! * mat!(1) + y! * mat!(4) + z! * mat!(7)
        dstVec.z = x! * mat!(2) + y! * mat!(5) + z! * mat!(8)

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

    'screenSeg& = VARSEG(vram%(0)) + 1

    'DEF SEG = screenSeg& + y% * 20
    'POKE x%, col%
    pixel[y% * 320 +x%] = col%

END SUB

SUB PrepareEdgeListsFlat

    FOR n% = 0 TO 2
        i0% = n%: i1% = n% + 1
        IF i1% = 3 THEN i1% = 0

        ys0% = ey%(i0%): ys1% = ey%(i1%)

        IF ys0% <> ys1% THEN
            IF (ys0% < ys1%) THEN
                isEdgeLeft% = TRUE%
                xs0% = ex%(i0%)
                xs1% = ex%(i1%)
            ELSE
                isEdgeLeft% = FALSE%
                SWAP ys0%, ys1%
                xs0% = ex%(i1%)
                xs1% = ex%(i0%)
            END IF

            dy% = ys1% - ys0%
            dx% = (xs1% - xs0%) \ dy%

            IF isEdgeLeft% = TRUE THEN
                FOR y% = ys0% TO ys1% - 1
                    IF y% >= 0 AND y% < SCRHEIGHT% THEN
                        polyEdgeLeft(y%).x = xs0%
                    END IF
                    xs0% = xs0% + dx%
                NEXT y%
            ELSE
                FOR y% = ys0% TO ys1% - 1
                    IF y% >= 0 AND y% < SCRHEIGHT% THEN
                        polyEdgeRightX%(y%) = xs0%
                    END IF
                    xs0% = xs0% + dx%
                NEXT y%
            END IF

        END IF
    NEXT n%

END SUB

SUB PrepareEdgeListsGouraud

    FOR n% = 0 TO 2
        i0% = n%: i1% = n% + 1
        IF i1% = 3 THEN i1% = 0

        ys0% = ey%(i0%): ys1% = ey%(i1%)

        IF ys0% <> ys1% THEN
            IF (ys0% < ys1%) THEN
                xs0% = ex%(i0%): cs0% = ec%(i0%)

                dy% = ys1% - ys0%
                dx% = (ex%(i1%) - xs0%) \ dy%
                dc% = (ec%(i1%) - cs0%) \ dy%

                FOR y% = ys0% TO ys1% - 1
                    IF y% >= 0 AND y% < SCRHEIGHT% THEN
                        polyEdgeLeft(y%).x = xs0%
                        polyEdgeLeft(y%).c = cs0%
                    END IF
                    xs0% = xs0% + dx%
                    cs0% = cs0% + dc%
                NEXT y%
            ELSE
                xs0% = ex%(i1%)
                dx% = (ex%(i0%) - xs0%) \ (ys0% - ys1%)

                FOR y% = ys1% TO ys0% - 1
                    IF y% >= 0 AND y% < SCRHEIGHT% THEN
                        polyEdgeRightX%(y%) = xs0%
                    END IF
                    xs0% = xs0% + dx%
                NEXT y%
            END IF
       
        END IF
    NEXT n%

END SUB

SUB PrepareEdgeListsTextured

    FOR n% = 0 TO 2
        i0% = n%: i1% = n% + 1
        IF i1% = 3 THEN i1% = 0

        ys0% = ey%(i0%): ys1% = ey%(i1%)

        IF ys0% <> ys1% THEN
            IF (ys0% < ys1%) THEN
                xs0% = ex%(i0%): us0% = eu%(i0%): vs0% = ev%(i0%)

                dy% = ys1% - ys0%
                dx% = (ex%(i1%) - xs0%) \ dy%
                du% = (eu%(i1%) - us0%) \ dy%
                dv% = (ev%(i1%) - vs0%) \ dy%

                FOR y% = ys0% TO ys1% - 1
                    IF y% >= 0 AND y% < SCRHEIGHT% THEN
                        polyEdgeLeft(y%).x = xs0%
                        polyEdgeLeft(y%).u = us0%
                        polyEdgeLeft(y%).v = vs0%
                    END IF
                    xs0% = xs0% + dx%
                    us0% = us0% + du%
                    vs0% = vs0% + dv%
                NEXT y%

            ELSE
                xs0% = ex%(i1%)
                dx% = (ex%(i0%) - xs0%) \ (ys0% - ys1%)

                FOR y% = ys1% TO ys0% - 1
                    IF y% >= 0 AND y% < SCRHEIGHT% THEN
                        polyEdgeRightX%(y%) = xs0%
                    END IF
                    xs0% = xs0% + dx%
                NEXT y%
            END IF

        END IF
    NEXT n%

END SUB

SUB PrepareEdgeListsTexturedGouraud

    FOR n% = 0 TO 2
        i0% = n%: i1% = n% + 1
        IF i1% = 3 THEN i1% = 0

        ys0% = ey%(i0%): ys1% = ey%(i1%)

        IF ys0% <> ys1% THEN
            IF (ys0% < ys1%) THEN
                xs0% = ex%(i0%): cs0% = ec%(i0%)
                us0% = eu%(i0%): vs0% = ev%(i0%)

                dy% = ys1% - ys0%
                dx% = (ex%(i1%) - xs0%) \ dy%
                dc% = (ec%(i1%) - cs0%) \ dy%
                du% = (eu%(i1%) - us0%) \ dy%
                dv% = (ev%(i1%) - vs0%) \ dy%

                FOR y% = ys0% TO ys1% - 1
                    IF y% >= 0 AND y% < SCRHEIGHT% THEN
                        polyEdgeLeft(y%).x = xs0%
                        polyEdgeLeft(y%).c = cs0%
                        polyEdgeLeft(y%).u = us0%
                        polyEdgeLeft(y%).v = vs0%
                    END IF
                    xs0% = xs0% + dx%
                    cs0% = cs0% + dc%
                    us0% = us0% + du%
                    vs0% = vs0% + dv%
                NEXT y%

            ELSE
                xs0% = ex%(i1%)
                dx% = (ex%(i0%) - xs0%) \ (ys0% - ys1%)

                FOR y% = ys1% TO ys0% - 1
                    IF y% >= 0 AND y% < SCRHEIGHT% THEN
                        polyEdgeRightX%(y%) = xs0%
                    END IF
                    xs0% = xs0% + dx%
                NEXT y%
            END IF

        END IF
    NEXT n%

END SUB

SUB ReadLineStringFromPlg (fileHandle%, bufDst$)

    DO
        LINE INPUT #fileHandle%, lbuf$
        IF LEFT$(lbuf$, 1) = "#" THEN lbuf$ = ""
    LOOP WHILE lbuf$ = ""

    bufDst$ = lbuf$

END SUB

SUB ReadLineValues (inpa$, outa$(), numVals%)

    FOR i% = 0 TO numVals% - 1
        inpa$ = LTRIM$(inpa$)
        index% = INSTR(inpa$, " ")
        IF index% > 0 THEN
            outa$(i%) = LEFT$(inpa$, index% - 1)
            inpa$ = RIGHT$(inpa$, LEN(inpa$) - index%)
        ELSE
            outa$(i%) = inpa$
        END IF
    NEXT i%

END SUB

SUB RenderObject3D (obj AS Object3D)

    ' Set basic VideoSeg to our buffer in case I want to use LINE,PSET,CIRCLE,etc
    'screenSeg& = VARSEG(vram%(0)) + 1

    SELECT CASE obj.renderType

        CASE renderTypeDots%
            RenderObject3Ddots obj

        CASE renderTypeWire%
            RenderObject3Dwire obj

        CASE renderTypePoly%
            SortObject3Dpoly obj

            SELECT CASE obj.polyStyle
                CASE polyStyleFlat%
                    CalcObjectPolyLighting obj, lights(0)
                CASE polyStyleGouraud%
                    CalcObjectVertexLighting obj, lights(0)
                CASE polyStyleTextured%
                    RotateObjectVertexNormals obj, rotMat()
                    CalcObjectEnvmapTC obj
                CASE polyStyleTexturedGouraud%
                    CalcObjectVertexLighting obj, lights(0)
                    RotateObjectVertexNormals obj, rotMat()
                    CalcObjectEnvmapTC obj
                CASE ELSE
            END SELECT

            RenderObject3Dpoly obj

        CASE ELSE
            PRINT "Render Type doesn't exist.."
    END SELECT

END SUB

SUB RenderObject3Ddots (obj AS Object3D)

    'screenSeg& = VARSEG(vram%(0)) + 1

    n% = obj.numVertices - 1
    
    FOR i% = 0 TO n%
      
        z% = screenVertices(i%).z

        IF z% > 0 THEN
            x% = screenVertices(i%).x
            y% = screenVertices(i%).y

            IF x% >= 0 AND x% < SCRWIDTH% * FPRAST% AND y% >= 0 AND y% < SCRHEIGHT% * FPRAST% THEN

                'DEF SEG = screenSeg& + DivFPRAST%(y%) * 20
                'POKE DivFPRAST%(x%), 63
                pixel[DivFPRAST%(y%) * 320 + DivFPRAST%(x%)] = 63

            END IF
        END IF

    NEXT i%

END SUB

SUB RenderObject3Dpoly (obj AS Object3D)

    indexOffset% = obj.polyIndicesOffset
    polyOffset% = obj.polysOffset
    polyStyle% = obj.polyStyle

    FOR zSlot% = maxZbucket% TO minZbucket% STEP -1
        count% = zBucket%(zSlot%, 0)
        FOR n% = 1 TO count%
            i% = zBucket%(zSlot%, n%)
            polyIndex% = (i% - indexOffset%) \ 3
            DrawPoly i%, polyOffset%, polyIndex%, polyStyle%
        NEXT n%
    NEXT zSlot%

    leftoversCount% = zBucketLeftovers%(0)
    FOR n% = 1 TO leftoversCount%
        i% = zBucketLeftovers%(n%)
        polyIndex% = (i% - indexOffset%) \ 3
        DrawPoly i%, polyOffset%, polyIndex%, polyStyle%
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

            IF x0% >= 0 AND x0% < SCRWIDTH% * FPRAST% AND x1% >= 0 AND x1% < SCRWIDTH% * FPRAST% THEN
                IF y0% >= 0 AND y0% < SCRHEIGHT% * FPRAST% AND y1% >= 0 AND y1% < SCRHEIGHT% * FPRAST% THEN

                    x0% = DivFPRAST%(x0%)
                    y0% = DivFPRAST%(y0%)
                    x1% = DivFPRAST%(x1%)
                    y1% = DivFPRAST%(y1%)

                    DrawLine x0%, y0%, x1%, y1%, 63
               
                END IF
            END IF

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

SUB RotateObjectVertexNormals (obj AS Object3D, mat!())

    vnOffset% = obj.vertexNormalsOffset
    n% = obj.numVertices - 1

    FOR i% = 0 TO n%

        x! = objectVertexNormals(vnOffset%).x
        y! = objectVertexNormals(vnOffset%).y
        z! = objectVertexNormals(vnOffset%).z

        transformedVertexNormals(i%).x = INT(x! * mat!(0) + y! * mat!(3) + z! * mat!(6))
        transformedVertexNormals(i%).y = INT(x! * mat!(1) + y! * mat!(4) + z! * mat!(7))
        transformedVertexNormals(i%).z = INT(x! * mat!(2) + y! * mat!(5) + z! * mat!(8))

        vnOffset% = vnOffset% + 1

    NEXT i%

END SUB

SUB RunScene3D (t!)

    IF CONTROLS% <> TRUE% THEN
        xt! = 0 'SIN(t!) * 48
        yt! = 0 'SIN(1.5 * t!) * 32
        zt! = 256 '+ SIN(1.25 * t!) * 96

        rx! = .7 * t!
        ry! = t!
        rz! = 1.25 * t!

        SELECT CASE currentObject%

            CASE objGenCube%
                zt! = defaultScaleRange!

            CASE objGenDotCube%
                zt! = 256

            CASE objGenSphere%

            CASE objGenTorus%

            CASE objLoadTest%
                zt! = defaultScaleRange!

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
    END IF

    SetLightPosition 0, 0, 1, 0

    TransformObject3D object(currentObject%)
    RenderObject3D object(currentObject%)

END SUB

SUB ScaleIVec (vec AS IVector3D, scale!)

    vec.x = INT(vec.x * scale!)
    vec.y = INT(vec.y * scale!)
    vec.z = INT(vec.z * scale!)

END SUB

SUB ScaleObjectVertices (obj AS Object3D, scaleX!, scaleY!, scaleZ!)

    offset% = obj.verticesOffset
    n% = obj.numVertices - 1

    FOR i% = 0 TO n%
        k% = i% + offset%

        objectVertices(k%).x = scaleX! * objectVertices(k%).x
        objectVertices(k%).y = scaleY! * objectVertices(k%).y
        objectVertices(k%).z = scaleZ! * objectVertices(k%).z
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

            SetGradPal 0, DIFFUSEWIDTH% - 1, r0%, g0%, b0%, r1%, g1%, b1%
            SetGradPal DIFFUSEWIDTH%, MATERIALWIDTH% - 1, r1%, g1%, b1%, r2%, g2%, b2%
           
            SetGradPal MATERIALWIDTH%, MATERIALWIDTH% + DIFFUSEWIDTH% - 1, b0%, g0%, r0%, b1%, g1%, r1%
            SetGradPal MATERIALWIDTH% + DIFFUSEWIDTH%, MATERIALWIDTH% + MATERIALWIDTH% - 1, b1%, g1%, r1%, b2%, g2%, r2%
           
            SetGradPal 2 * MATERIALWIDTH%, 2 * MATERIALWIDTH% + DIFFUSEWIDTH% - 1, r0%, b0%, g0%, r1%, b1%, g1%
            SetGradPal 2 * MATERIALWIDTH% + DIFFUSEWIDTH%, 2 * MATERIALWIDTH% + MATERIALWIDTH% - 1, r1%, b1%, g1%, r2%, b2%, g2%
           
            SetGradPal 3 * MATERIALWIDTH%, 3 * MATERIALWIDTH% + DIFFUSEWIDTH% - 1, g0%, r0%, b0%, g1%, r1%, b1%
            SetGradPal 3 * MATERIALWIDTH% + DIFFUSEWIDTH%, 3 * MATERIALWIDTH% + MATERIALWIDTH% - 1, g1%, r1%, b1%, g2%, r2%, b2%




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

SUB SetIVec (vec AS IVector3D, x%, y%, z%)

    vec.x = x%
    vec.y = y%
    vec.z = z%

END SUB

SUB SetLightPosition (x!, y!, z!, index%)

    lights(index%).x = x!
    lights(index%).y = y!
    lights(index%).z = z!

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
            x0& = screenVertices(i0%).x
            x1& = screenVertices(i1%).x
            x2& = screenVertices(i2%).x
            y0& = screenVertices(i0%).y
            y1& = screenVertices(i1%).y
            y2& = screenVertices(i2%).y

            ' Will need to investigate if it affects performance that I use LONG INT to avoid certain overflows
            ' If polygon numbers are too little, problaby not affecting much
            dx0& = x0& - x1&
            dy0& = y2& - y1&
            dx1& = x2& - x1&
            dy1& = y0& - y1&
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

    CONST PROJ! = 256 * FPRAST%

    posX! = obj.position.x
    posY! = obj.position.y
    posZ! = obj.position.z

    CalcRotMatrix obj.rotation
    InvertMatrix rotMat(), invRotMat()

    offset% = obj.verticesOffset
    n% = obj.numVertices - 1
    FOR i% = 0 TO n%

        k% = i% + offset%
        xo! = objectVertices(k%).x
        yo! = objectVertices(k%).y
        zo! = objectVertices(k%).z

        z! = xo! * rotMat(2) + yo! * rotMat(5) + zo! * rotMat(8) + posZ!

        zi% = INT(z!)
        screenVertices(i%).z = zi%
        IF zi% > 0 THEN
            x! = xo! * rotMat(0) + yo! * rotMat(3) + zo! * rotMat(6)
            y! = xo! * rotMat(1) + yo! * rotMat(4) + zo! * rotMat(7)

            screenVertices(i%).x = SCRWIDTH2% * FPRAST% + INT(((x! + posX!) * PROJ!) / z!)
            screenVertices(i%).y = SCRHEIGHT2% * FPRAST% - INT(((y! + posY!) * PROJ!) / z!)
        END IF

    NEXT i%

END SUB

SUB WaitForVsync

    WAIT &H3DA, 8
    WAIT &H3DA, 8, 8

END SUB

SUB WriteVram

    PUT (0, 0), vram%(6), PSET

END SUB

