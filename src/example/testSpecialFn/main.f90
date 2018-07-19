Program Main

    use mfpGlobal
    use mfpSpecialFunction

Implicit None
    integer,parameter  :: nX = 5000
    Real(RP) :: xMin, xMax
    Real(RP),dimension(nX) :: X, Y
    integer  :: ii

    ! write(*,*) "Test Gamma Related Function"

    ! Call testGx()


    xMin = 0.1;
    xMax = 300.0;
    Call testBetaFunction(xMin, xMax, nX, X, Y)

    do ii = 1,nX
        write(*,*) ii, X(ii), Y(ii)
    Enddo

End Program

#include "gammaRelated/testXbeta2xm1.inc"
