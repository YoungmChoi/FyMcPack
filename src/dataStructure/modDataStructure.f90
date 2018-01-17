!!*------------------------------------------------------------------*!!
!!  Project : modDataStructure
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Module
!!      data Structure
!!
!!  Description
!!      Fortran Data Structure
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!  Module : modDataStructure
!! ------------------------------------------------------------------ !!
Module  modDataStructure

!!  Dependency ------------------------------------------------------ !!

    use modGlobal

!!  Variable Declaration -------------------------------------------- !!

Implicit None

private

!!  Module Variable ------------------------------------------------- !!

    integer, parameter :: strMaxLength = CHAR_LEN

    integer, parameter :: sLTableDefaultSize = 50

    logical, parameter :: sLTableSearchErrrorStop = .TRUE.

!!  Header Files ---------------------------------------------------- !!

    Include "string/string.typ"

    Include "dataArray/dataArray.typ"

    Include "sLookUpTable/sLookUpTable.typ"

    public :: testDataArray, testString, testSLTable

Contains

!!  Procedure Files ------------------------------------------------- !!

    Include "string/string.inc"

    Include "dataArray/dataArray.inc"

    Include "sLookUpTable/sLookUpTable.inc"

!!  Subroutine Test Script ------------------------------------------ !!

    Include "string/testString.inc"

    Include "dataArray/testDataArray.inc"

    Include "sLookUpTable/testSLTable.inc"

End Module
