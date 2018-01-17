Subroutine testString
    Implicit None
    Character(len=CHAR_LEN) :: tmpChar

    Type(typString) :: tmpString1, tmpString2

    Type(typString), allocatable :: args(:)
    ! Character(len=CHAR_LEN),allocatable :: args(:)
    integer :: nargs, iarg

    integer :: tmpInt
    Real    :: tmpReal

    tmpChar = "ab cd"
    Call tmpString1%set("7.5")
    Call tmpString2%set("3.0")

    tmpInt = tmpString1%getInt(.FALSE.)
    tmpReal = tmpString2%getReal()

    write(*,*) tmpInt
    write(*,*) tmpReal

    ! Call tmpString%set("abc def")
    !
    ! Call parse(tmpChar, " ", args, nargs)
    !
    ! !Call parse(tmpString, " ", args, nargs)
    !
    ! write(*,*) tmpChar
    !
    ! ! do iarg = 1, nargs
    ! !     write(*,*) args(iarg)
    ! ! enddo
    !
    ! do iarg = 1, nargs
    !     write(*,*) iarg, args(iarg)%get()
    ! enddo

end Subroutine
