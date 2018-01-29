Subroutine testSurfMesh
    Implicit None

    ! Call testEllipsoid()

    ! Call testHemiSphere()

    ! Call testParaPipe()

    ! Call testSemiEllipsoid()

    ! Call testSphere()

    Call testSurfMeshDict()

End Subroutine

Subroutine testSurfMeshDict
    Implicit None
    Class(typSurfMesh), allocatable :: surfMesh
    Type(typDictionaryPtr)          :: fileDict

    Call fileDict%initialize("./", "surfMesh", ".dict")

    Call initialize(surfMesh, fileDict%subDict("surfaceMesh"))

End Subroutine

Subroutine testEllipsoid
    Implicit None
    Type(typEllipsoidSurfMesh) :: ellipsoid
    Type(typDictionaryPtr)  :: fileDict

    Call fileDict%initialize("./", "surfMesh", ".dict")

    Call ellipsoid%initialize(fileDict%subDict("ellipsoid"))

    Call ellipsoid%writeVTK("ellipsoid")

End Subroutine


Subroutine testHemiSphere
    Implicit None
    Type(typHemiSphereSurfMesh) :: hemisphere
    Type(typDictionaryPtr)  :: fileDict

    Call fileDict%initialize("./", "surfMesh", ".dict")

    Call hemisphere%initialize(fileDict%subDict("hemisphere"))

    Call hemisphere%writeVTK("hemisphere")

End Subroutine

Subroutine testParaPipe
    Implicit None
    Type(typParaPipeMesh)   :: paraPipe
    Type(typDictionaryPtr)  :: fileDict

    Call fileDict%initialize("./", "surfMesh", ".dict")

    Call paraPipe%initialize(fileDict%subDict("paraPipe"))

    Call paraPipe%writeVTK("paraPipe")

End Subroutine

Subroutine testSemiEllipsoid
    Implicit None
    Type(typSemiEllipsoidSurfMesh)   :: semiEllipsoid
    Type(typDictionaryPtr)  :: fileDict

    Call fileDict%initialize("./", "surfMesh", ".dict")

    Call semiEllipsoid%initialize(fileDict%subDict("semiEllipsoid"))

    Call semiEllipsoid%writeVTK("semiEllipsoid")

End Subroutine

Subroutine testSphere
    Implicit None
    Type(typSphereSurfMesh) :: sphere
    Type(typDictionaryPtr)  :: fileDict

    Call fileDict%initialize("./", "surfMesh", ".dict")

    Call sphere%initialize(fileDict%subDict("sphere"))

    Call sphere%writeVTK("sphere")

End Subroutine
