!!*------------------------------------------------------------------*!!
!!  Project : mfpDataStructure
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Module
!!      Special Function
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

    Module mfpSpecialFunction

!!  Dependency ------------------------------------------------------ !!

        use mfpGlobal

!!  Variable Declaration -------------------------------------------- !!

    Implicit None

    private

    !! Public

        PUBLIC :: xBetaX2m1

        PUBLIC :: testGx

    Contains

!!  Procedure Files ------------------------------------------------- !!

#include "gammaFunction/xBetax2m1.inc"

!! ------------------------------------------------------------------ !!
    End Module
!! ------------------------------------------------------------------ !!
