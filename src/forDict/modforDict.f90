!!*------------------------------------------------------------------*!!
!!  Project : modforDict
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Module
!!      forDict
!!
!!  Description
!!      This moduleholds fortran dictionary type
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!


!! ------------------------------------------------------------------ !!
!!  Module : modforDict
!! ------------------------------------------------------------------ !!
Module modforDict

!!  Dependency ------------------------------------------------------ !!

    use modGlobal

    use modDataStructure

!!  Variable Declaration -------------------------------------------- !!

Implicit None

    Private

!!  Module Variable ------------------------------------------------- !!

    !!- Initial Size of Dictionary
    Integer, parameter :: dictDefaultSize = 50

    !!- Dictionary Character Length
    Integer, parameter :: dictCharLen = CHAR_LEN

!!  Header Files ---------------------------------------------------- !!

    Include "forDict.typ"

!!  Procedure Files ------------------------------------------------- !!

Contains

    Include "forDict.inc"

!!  Subroutine Test Script ------------------------------------------ !!

    ! Include "testLLink.inc"

!! ------------------------------------------------------------------ !!
End Module
!! ------------------------------------------------------------------ !!
