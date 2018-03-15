!!*------------------------------------------------------------------*!!
!!  FyMc Pack
!!*------------------------------------------------------------------*!!
!!  BSD 3-Clause License
!!
!!  Copyright (c) 2018, YoungMyung Choi
!!  All rights reserved.
!!
!!  Redistribution and use in source and binary forms, with or without
!!  modification, are permitted provided that the following conditions are met:
!!
!!  * Redistributions of source code must retain the above copyright notice, this
!!    list of conditions and the following disclaimer.
!!
!!  * Redistributions in binary form must reproduce the above copyright notice,
!!    this list of conditions and the following disclaimer in the documentation
!!    and/or other materials provided with the distribution.
!!
!!  * Neither the name of the copyright holder nor the names of its
!!    contributors may be used to endorse or promote products derived from
!!    this software without specific prior written permission.
!!
!!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
!!  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
!!  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!!  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
!!  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
!!  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
!!  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
!!  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
!!  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
!!  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!!
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Module
!!      Geometry
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
    use mfpGeneral

!!  Variable Declaration -------------------------------------------- !!

Implicit None

private

!!  Module Variable ------------------------------------------------- !!

    Include "../mathConstant/gaussQuadrature.inc"

!!  Header Files ---------------------------------------------------- !!

    Include "baseGeometry/point.typ"

    Include "baseGeometry/panel.typ"

    Include "surfMesh/surfMesh.typ"

    !! Public

    Public :: testSurfMesh

Contains

!!  Procedure Files ------------------------------------------------- !!

    Include "auxiliary/getGaussCoordinate.inc"

    Include "baseGeometry/point.inc"

    Include "baseGeometry/panel.inc"

    Include "surfMesh/surfMesh.inc"

!!  Subroutine Test Script ------------------------------------------ !!

    Include "surfMesh/testSurfMesh.inc"

End Module
