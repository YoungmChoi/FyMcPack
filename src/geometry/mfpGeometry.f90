!!*------------------------------------------------------------------*!!
!!  Project : mfpDataStructure
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Module
!!      General
!!
!!  Description
!!      FyMC Pack General Module
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!  Module : FyMcPack Module General
!! ------------------------------------------------------------------ !!
Module  mfpGeometry

!!  Dependency ------------------------------------------------------ !!

    use mfpGlobal

!!  Variable Declaration -------------------------------------------- !!

Implicit None

private

!!  Module Variable ------------------------------------------------- !!

    integer, parameter :: strMaxLength = CHAR_LEN

    integer, parameter :: sLTableDefaultSize = 50

    logical, parameter :: sLTableSearchErrrorStop = .TRUE.

    integer, parameter :: fileLineLength = 2000

!!  Header Files ---------------------------------------------------- !!

    Include "baseGeometry/point.typ"

    Include "baseGeometry/panel.typ"

    Include "surfMesh/surfMesh.typ"

Contains

!!  Procedure Files ------------------------------------------------- !!

    Include "baseGeometry/point.inc"

    Include "baseGeometry/panel.inc"

    Include "surfMesh/surfMesh.inc"

!!  Subroutine Test Script ------------------------------------------ !!

    ! Include "string/testString.inc"
    !
    ! Include "dataArray/testDataArray.inc"
    !
    ! Include "sLookUpTable/testSLTable.inc"
    !
    ! Include "dictionary/testDictionary.inc"

End Module
