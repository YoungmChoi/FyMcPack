!!-----------------------------------------------------------
Function xBetaX2m1(x) result(y)
!!
!!  Compute g(x) = x * beta(x/2) - 1
!!
!!   - beta(x) = 0.5 * ( psi ( (x+1)/2 ) - psi (x/2) )
!!
!!     where psi is diGamma function
!!
!!  Programmed by Young-Myung Choi, Ecole Centrale de Nantes.
!!                  Date : 2018-06-18
!!
!!-----------------------------------------------------------
    Implicit None
    Real(RP), intent(in)  :: x
    Real(RP) :: y
    integer  :: k
    real(RP) :: arg
!!-----------------------------------------------------------
    if (x.lt.0.0) then
        write(*,*) "    [Error] g(x) = x * beta(x/2) - 1, the argument should be positive or zero."
        write(*,*) "        Given x : ", x
        stop
    end if

    y = 0.0_RP
    if (x.le.1.6) then
        arg = 1.0_RP
        do k = 1,17
            arg = arg / ( x / k + 2.0_RP)
            y = y + arg
        enddo
    else if (x.le.4.8) then
        arg = 1.0_RP
        do k = 1,10
            arg = arg / ( x / k + 2.0_RP)
            y = y + arg
        enddo
    else if (x.le.8.8) then
        arg = 1.0_RP
        do k = 1,6
            arg = arg / ( x / k + 2.0_RP)
            y = y + arg
        enddo
    else if (x.le.20.9) then
        arg = 1.0_RP
        do k = 1,4
            arg = arg / ( x / k + 2.0_RP)
            y = y + arg
        enddo
    else if (x.le.52.0) then
        arg = 1.0_RP
        do k = 1,2
            arg = arg / ( x / k + 2.0_RP)
            y = y + arg
        enddo
    else
        y = 1.0_RP / (x + 2.0_RP)
    end if
!!-----------------------------------------------------------
End Function
!!-----------------------------------------------------------

Subroutine testGx()

    integer,parameter :: nX = 100
    Real(RP) :: xMin, xMax
    Real(RP) :: dx
    integer  :: ii
    Real(RP),dimension(nX) :: X, Y

    xMin = 0.0001
    xMax = 10

    dx = (xMax - xMin) / (nX-1)

    X(1) = xMin
    do ii = 2, nX
        X(ii) = X(ii-1) + dx
    enddo

    do ii = 1,nX
        Y(ii) = xBetaX2m1(X(ii))
    enddo

    do ii = 1, nX
        write(*,*) ii, X(ii), Y(ii)
    enddo


End Subroutine
