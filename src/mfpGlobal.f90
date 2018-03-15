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

Module mfpGlobal

Implicit None

    Integer,Parameter :: FIO_UNIT = 100

    Integer,parameter :: RP = kind(1.d0)

    Integer,parameter :: CHAR_LEN = 300

    Real(RP),parameter :: pi = 4.d0 * datan(1.d0)

    Real(RP),parameter :: gravi = 9.81_RP

    Complex(RP), Parameter :: ai = (0.0_RP, 1.0_RP)

End Module

!!*------------------------------------------------------------------*!!
