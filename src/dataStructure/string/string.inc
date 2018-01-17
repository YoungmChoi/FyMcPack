Subroutine setString(this, string, isTrim)
    Implicit None
    class(typString), intent(inout) :: this
    Character(len=*), intent(in) :: string
    logical,optional :: isTrim
    integer :: nChar

    this%str_ = ""

    nChar = sizeof(string)/ sizeof("a")

    if (nChar.gt.strMaxLength) then
        write(*,*) "    [WARNING] setString(string), string is longer than strMaxLength."
        nChar = strMaxLength
    end if

    this%nStr_ = nChar
    this%str_(1:this%nStr_) = string

    if (present(isTrim)) then
        if (isTrim) then
            this%str_ = ""
            nChar = sizeof(trim(string))/ sizeof("a")
            this%nStr_ = nChar
            this%str_(1:this%nStr_) = trim(string)
        end if
    end if

End Subroutine

Subroutine clearString(this)
    Implicit None
    class(typString), intent(inout) :: this
    this%str_=""
    this%nStr_=0
End Subroutine

function getString(this) result(string)
    Implicit None
    class(typString), intent(inout) :: this
    character(len=this%nStr_) :: string
    string = this%str_(1:this%nStr_)
End function

function getStringLower(this) result(string)
    !
    !  Dr. George Benthiem's string subroutine
    !
    !       See. http://gbenthien.net/about.html
    !
    ! Removes backslash (\) characters. Double backslashes (\\) are replaced
    ! by a single backslash.
    !
    !   Modified by Y.M Choi
    !
    class(typString), intent(inout) :: this
    character(len=this%nStr_)       :: string
    CHARACTER          :: ch
    INTEGER,PARAMETER  :: DUC = ICHAR('A') - ICHAR('a')
    INTEGER            :: i
    DO i = 1, this%nStr_
        ch = this%str_(i:i)
        IF (ch >= 'A'.AND.ch <= 'Z') ch = CHAR(ICHAR(ch)-DUC)
        string(i:i) = ch
    END DO
end function

function getStringUpper(this)  RESULT (string)
    !
    !  Dr. George Benthiem's string subroutine
    !
    !       See. http://gbenthien.net/about.html
    !
    ! Removes backslash (\) characters. Double backslashes (\\) are replaced
    ! by a single backslash.
    !
    !   Modified by Y.M Choi
    !
    class(typString), intent(inout) :: this
    character(len=this%nStr_)       :: string
    CHARACTER          :: ch
    INTEGER,PARAMETER  :: DUC = ICHAR('A') - ICHAR('a')
    INTEGER            :: i
    DO i = 1, this%nStr_
        ch = this%str_(i:i)
        IF (ch >= 'a'.AND.ch <= 'z') ch = CHAR(ICHAR(ch)+DUC)
        string(i:i) = ch
    END DO

END FUNCTION

function isStringStringEqual(orgin, target, isStrongCompare) result(isEqual)
    Implicit None
    class(typString), intent(in) :: orgin
    type(typString), intent(in)  :: target
    logical, intent(in),optional :: isStrongCompare
    logical :: isEqual
    isEqual = isCharCharEqual(orgin%str_, target%str_, isStrongCompare)
end function

function isStringCharEqual(orgin, target, isStrongCompare) result(isEqual)
    Implicit None
    class(typString), intent(in) :: orgin
    Character(len=*), intent(in)  :: target
    logical, intent(in),optional :: isStrongCompare
    logical :: isEqual
    isEqual = isCharCharEqual(orgin%str_, target, isStrongCompare)
end function

function isCharStringEqual(orgin, target, isStrongCompare) result(isEqual)
    Implicit None
    Character(len=*), intent(in)  :: orgin
    class(typString), intent(in) :: target
    logical, intent(in),optional :: isStrongCompare
    logical :: isEqual
    isEqual = isCharCharEqual(orgin, target%str_, isStrongCompare)
end function

function isCharCharEqual(orgin, target, isStrongCompare) result(isEqual)
    Implicit None
    Character(len=*), intent(in) :: orgin
    Character(len=*), intent(in) :: target
    logical, intent(in),optional :: isStrongCompare
    logical :: isEqual

    isEqual = .FALSE.

    if (present(isStrongCompare)) then
        if (isStrongCompare) then
            if (trim(orgin).eq.trim(target)) isEqual = .TRUE.
        else
            if (trim(lowerChar(orgin)).eq.trim( lowerChar(target))) isEqual = .TRUE.
        end if
    else
        if (trim(lowerChar(orgin)).eq.trim( lowerChar(target))) isEqual = .TRUE.
    end if

end function

subroutine parseStringString(this, delims, args, nargs)
    Implicit None
    class(typString), intent(inout) :: this
    character(len=*)                :: delims
    type(typString),allocatable     :: args(:)
    integer, intent(out)            :: nargs
    character(len=strMaxLength),allocatable :: tmpArgs(:)
    integer :: istr
    Call parseCharChar(this%str_, delims, tmpArgs, nargs)

    if (allocated(args)) deallocate(args)
    allocate(args(nargs))

    do istr = 1, nargs
        Call args(istr)%set(tmpArgs(istr),.TRUE.)
    enddo

    if (allocated(tmpArgs)) deallocate(tmpArgs)

end subroutine

subroutine parseStringChar(this, delims, args, nargs)
    Implicit None
    class(typString), intent(inout) :: this
    character(len=*)                :: delims
    character(len=*),allocatable    :: args(:)
    integer, intent(out)            :: nargs

    if (allocated(args)) deallocate(args)
    Call parseCharChar(this%str_, delims, args, nargs)

end subroutine

subroutine parseCharString(str, delims, args, nargs)
    Implicit None
    character(len=*) :: str,delims
    type(typString),allocatable     :: args(:)
    integer, intent(out)            :: nargs
    character(len=strMaxLength),allocatable :: tmpArgs(:)
    integer :: istr

    Call parseCharChar(str, delims, tmpArgs, nargs)

    if (allocated(args)) deallocate(args)
    allocate(args(nargs))

    do istr = 1, nargs
        Call args(istr)%set(tmpArgs(istr),.TRUE.)
    enddo

    if (allocated(tmpArgs)) deallocate(tmpArgs)

end subroutine

subroutine parseCharChar(str, delims, args, nargs)
    !
    !  Dr. George Benthiem's string subroutine
    !
    !       See. http://gbenthien.net/about.html
    !
    ! Parses the string 'str' into arguments args(1), ..., args(nargs) based on
    ! the delimiters contained in the string 'delims'. Preceding a delimiter in
    ! 'str' by a backslash (\) makes this particular instance not a delimiter.
    ! The integer output variable nargs contains the number of arguments found.

    character(len=*) :: str,delims
    character(len=len_trim(str)) :: strsav
    character(len=*),allocatable :: args(:)
    character(len=len_trim(str)),allocatable :: tempArgs(:)
    integer, intent(out) :: nargs
    integer :: i, k, lenstr, na
    character(len=1) :: dummy

    strsav=str
    call compactChar(str)
    na = sizeof(str) / sizeof(dummy)
    if (na.le.0) then
        nargs = 0
        return
    endif

    if (allocated(args)) deallocate(args)
    if (allocated(tempArgs)) deallocate(tempArgs)

    allocate(args(na), tempArgs(na))
    !na=size(args)
    do i=1,na
        args(i)=' '
    end do
    nargs=0
    lenstr=len_trim(str)
    if(lenstr==0) return
    k=0

    do
        if(len_trim(str) == 0) exit
        nargs=nargs+1
        call splitChar(str,delims,args(nargs))
        call removebkslChar(args(nargs))
        call compactChar(args(nargs))
    end do
    tempArgs = args
    deallocate(args)
    allocate(args(nargs))
    do i = 1, nargs
        args(i) = tempArgs(i)
    enddo
    deallocate(tempArgs)
    str=strsav

end subroutine

!**********************************************************************

subroutine compactChar(str)
    !
    !  Dr. George Benthiem's string subroutine
    !
    !       See. http://gbenthien.net/about.html
    !
    ! Converts multiple spaces and tabs to single spaces; deletes control characters;
    ! removes initial spaces.
    character(len=*):: str
    character(len=1):: ch
    character(len=len_trim(str)):: outstr
    integer :: k, i, lenstr, ich, isp

    str=adjustl(str)
    lenstr=len_trim(str)
    outstr=' '
    isp=0
    k=0
    do i=1,lenstr
        ch=str(i:i)
        ich=iachar(ch)
        select case(ich)
        case(9,32)     ! space or tab character
            if(isp==0) then
                k=k+1
                outstr(k:k)=' '
            end if
            isp=1
        case(33:)      ! not a space, quote, or control character
            k=k+1
            outstr(k:k)=ch
            isp=0
        end select
    end do
    str=adjustl(outstr)
end subroutine

!**********************************************************************

subroutine splitChar(str,delims,before,sep)
    !
    !  Dr. George Benthiem's string subroutine
    !
    !       See. http://gbenthien.net/about.html
    !
    ! Routine finds the first instance of a character from 'delims' in the
    ! the string 'str'. The characters before the found delimiter are
    ! output in 'before'. The characters after the found delimiter are
    ! output in 'str'. The optional output character 'sep' contains the
    ! found delimiter. A delimiter in 'str' is treated like an ordinary
    ! character if it is preceded by a backslash (\). If the backslash
    ! character is desired in 'str', then precede it with another backslash.

    character(len=*) :: str,delims,before
    character,optional :: sep
    logical :: pres
    character :: ch,cha
    integer :: i, k, ibsl, lenstr, ipos, iposa

    pres=present(sep)
    str=adjustl(str)
    call compactChar(str)
    lenstr=len_trim(str)
    if(lenstr == 0) return        ! string str is empty
    k=0
    ibsl=0                        ! backslash initially inactive
    before=' '
    do i=1,lenstr
        ch=str(i:i)
        if(ibsl == 1) then          ! backslash active
            k=k+1
            before(k:k)=ch
            ibsl=0
            cycle
        end if
        if(ch == '\') then          ! backslash with backslash inactive
            k=k+1
            before(k:k)=ch
            ibsl=1
            cycle
        end if
        ipos=index(delims,ch)
        if(ipos == 0) then          ! character is not a delimiter
            k=k+1
            before(k:k)=ch
            cycle
        end if
        if(ch /= ' ') then          ! character is a delimiter that is not a space
            str=str(i+1:)
            if(pres) sep=ch
            exit
        end if
        cha=str(i+1:i+1)            ! character is a space delimiter
        iposa=index(delims,cha)
        if(iposa > 0) then          ! next character is a delimiter
            str=str(i+2:)
            if(pres) sep=cha
            exit
        else
            str=str(i+1:)
            if(pres) sep=ch
            exit
        end if
    end do
    if(i >= lenstr) str=''
    str=adjustl(str)              ! remove initial spaces
    return

end subroutine

!**********************************************************************

subroutine removebkslChar(str)
    !
    !  Dr. George Benthiem's string subroutine
    !
    !       See. http://gbenthien.net/about.html
    !
    ! Removes backslash (\) characters. Double backslashes (\\) are replaced
    ! by a single backslash.
    Implicit None
    character(len=*):: str
    character(len=1):: ch
    character(len=len_trim(str))::outstr
    integer :: i, k, ibsl, lenstr

    str=adjustl(str)
    lenstr=len_trim(str)
    outstr=' '
    k=0
    ibsl=0                        ! backslash initially inactive

    do i=1,lenstr
        ch=str(i:i)
        if(ibsl == 1) then          ! backslash active
            k=k+1
            outstr(k:k)=ch
            ibsl=0
            cycle
        end if
        if(ch == '\') then          ! backslash with backslash inactive
            ibsl=1
            cycle
        end if
        k=k+1
        outstr(k:k)=ch              ! non-backslash with backslash inactive
    end do

    str=adjustl(outstr)

end subroutine

FUNCTION UpperChar(s1)  RESULT (s2)
    CHARACTER(*)       :: s1
    CHARACTER(LEN(s1)) :: s2
    CHARACTER          :: ch
    INTEGER,PARAMETER  :: DUC = ICHAR('A') - ICHAR('a')
    INTEGER            :: i

    DO i = 1,LEN(s1)
        ch = s1(i:i)
        IF (ch >= 'a'.AND.ch <= 'z') ch = CHAR(ICHAR(ch)+DUC)
        s2(i:i) = ch
    END DO
END FUNCTION

FUNCTION LowerChar(s1)  RESULT (s2)
    CHARACTER(*)       :: s1
    CHARACTER(LEN(s1)) :: s2
    CHARACTER          :: ch
    INTEGER,PARAMETER  :: DUC = ICHAR('A') - ICHAR('a')
    INTEGER            :: i

    DO i = 1,LEN(s1)
        ch = s1(i:i)
        IF (ch >= 'A'.AND.ch <= 'Z') ch = CHAR(ICHAR(ch)-DUC)
        s2(i:i) = ch
    END DO
END FUNCTION

function getStringInt(this, isWeek) result(value)
    Implicit None
    Class(typString), intent(in) :: this
    integer :: value
    logical, optional :: isWeek
    character(len=CHAR_LEN) :: dummyStr
    integer  :: io
    real(rp) :: valueR

    if (present(isWeek)) then
        if (isWeek.EQV..FALSE.) then
            valueR = getStringReal(this)
            value = int(valueR)

        end if
    else
        dummyStr = trim(this%str_)
        read(dummyStr, *, iostat = io) value
        if (io.ne.0) then
            write(*,*) "    [ERROR] string:getInt(), conversion failed."
            write(*,*) "      given string : ", trim(this%str_)
            value = 0.0_RP
            stop
        end if
    end if

end function

function getStringReal(this) result(value)
    Implicit None
    Class(typString), intent(in) :: this
    real(rp) :: value
    real     :: dummyReal
    integer  :: io
    character(len=CHAR_LEN) :: dummyStr
    dummyStr = trim(this%str_)
    read(dummyStr, *, iostat = io) dummyReal
    value = dummyReal
    if (io.ne.0) then
        write(*,*) "    [ERROR] string:getReal(), conversion failed."
        write(*,*) "      given string : ", trim(this%str_)
        value = 0.0_RP
        stop
    end if
end function

subroutine copyString(origin, target)
Implicit None
    Class(typString), intent(inout) :: origin
    Type(typString), intent(in)     :: target

    origin%nStr_ = target%nStr_
    origin%str_  = target%str_

end subroutine
