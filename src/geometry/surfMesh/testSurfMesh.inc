Subroutine testSurfMesh
    Implicit None

    Call testEllipsoid()

End Subroutine

Subroutine testEllipsoid
    Implicit None
    Type(typEllipsoidSurfMesh) :: ellipsoid
    Type(typDictionaryPtr)  :: fileDict

    Call fileDict%initialize("./", "surfMesh", ".dict")

End Subroutine
