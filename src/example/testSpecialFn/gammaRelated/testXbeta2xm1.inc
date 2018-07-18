    Subroutine testBetaFunction(xMin, xMax, nX, X, Y)
        use mfpGlobal
        use mfpSpecialFunction
    Implicit None
        integer, intent(in)  :: nX
        Real(RP), intent(in) :: xMin, xMax
        Real(RP) :: dx
        integer  :: ii
        Real(RP),dimension(nX) :: X, Y

        dx = (xMax - xMin) / (nX-1)

        X(1) = xMin
        do ii = 2, nX
            X(ii) = X(ii-1) + dx
        enddo

        do ii = 1,nX
            Y(ii) = xBetaX2m1(X(ii))
        enddo

    End Subroutine
