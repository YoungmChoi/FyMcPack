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
!!  Absolute error is 1e-6. The coefficients of polynomial are optimized to
!!  redouce computational time.
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
    if (x.le.3.2) then
        y = ( 2.5183787e+07_RP           +   2.8445912e+07_RP * x**1    &
          +   1.5340072e+07_RP * x**2    +   5.1966628e+06_RP * x**3    &
          +   1.2326758e+06_RP * x**4    +   2.1687105e+05_RP * x**5    &
          +   2.9308338e+04_RP * x**6    +   3.1126168e+03_RP * x**7    &
          +   2.6375950e+02_RP * x**8    +   1.8010666e+01_RP * x**9    &
          +   9.9674611e-01_RP * x**10 ) / ( 2.5183799e+07_RP           &
          +   4.5901991e+07_RP * x**1    +   3.6800485e+07_RP * x**2    &
          +   1.7504446e+07_RP * x**3    +   5.5966554e+06_RP * x**4    &
          +   1.2883320e+06_RP * x**5    +   2.2288951e+05_RP * x**6    &
          +   2.9824241e+04_RP * x**7    +   3.1481107e+03_RP * x**8    &
          +   2.6573363e+02_RP * x**9    +   1.8099701e+01_RP * x**10   &
          +   x**11 )
    else if (x.le.7.2) then
        y = ( 1.0209461e+10_RP          +   1.0181215e+10_RP * x**1     &
          +   4.8004070e+09_RP * x**2   +   1.3985014e+09_RP * x**3     &
          +   2.7921610e+08_RP * x**4   +   4.0285405e+07_RP * x**5     &
          +   4.3289028e+06_RP * x**6   +   3.5238224e+05_RP * x**7     &
          +   2.1900354e+04_RP * x**8   +   1.0392362e+03_RP * x**9     &
          +   3.7357712e+01_RP * x**10  +   9.9949381e-01_RP * x**11 )  &
          / ( 1.0209617e+10_RP          +   1.7257974e+10_RP * x**1     &
          +   1.2564187e+10_RP * x**2   +   5.3113761e+09_RP * x**3     &
          +   1.4750615e+09_RP * x**4   +   2.8761904e+08_RP * x**5     &
          +   4.0977913e+07_RP * x**6   +   4.3722593e+06_RP * x**7     &
          +   3.5444878e+05_RP * x**8   +   2.1974839e+04_RP * x**9     &
          +   1.0412322e+03_RP * x**10  +   3.7396240e+01_RP * x**11    &
          +   x**12 )
    else if (x.le.37.0) then
        y = (x**9 + 110.0_RP * x**8 + 5278.0_RP * x**7 + 144980.0_RP*x**6     &
          +  2513824.0_RP*x**5 + 28577120.0_RP * x**4 + 213718992.0_RP * x**3 &
          +  1020977760.0_RP *x*x + 2866510080.0_RP * x + 3712262400.0_RP)    &
          / (x**10 + 110.0_RP*x**9 + 5280.0_RP*x**8 + 145200.0_RP*x**7        &
          + 2524368.0_RP*x**6 + 28865760.0_RP*x**5 + 218683520.0_RP*x**4      &
          + 1076416000.0_RP*x**3 + 3264915456.0_RP*x**2 + 5441863680.0_RP*x   &
          + 3715891200.0_RP)
    else if (x.le.179.2) then
        y = (x**3 + 20.0_RP * x*x + 138.0_RP * x + 360.0_RP) &
          / (x**4 + 20.0_RP * x**3 + 140.0_RP * x * x + 400.0_RP*x + 384.0_RP)
    else
        y = (x + 6.0_RP) / (x*x + 6.0_RP *x + 8.0_RP)
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
