subroutine initializeSLtable(this, tableSize, name, strongSearch)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    Integer, optional :: tableSize
    Character(len=*),optional :: name
    logical,optional :: strongSearch

    this%nTable = sLTableDefaultSize

    if (present(tableSize)) then
        if (tableSize.ge.1) then
            this%nTable = tableSize
        end if
    end if

    allocate(this%keys(this%nTable))
    allocate(this%dataArray(this%nTable))

    this%nKey   = 0
    this%nTable = sLTableDefaultSize

    this%name = " "
    if (present(name)) then
        this%name = trim(name)
    end if

    this%strongSearch = .FALSE.
    if (present(name)) then
        this%strongSearch = strongSearch
    end if

end

subroutine pushSLTableReal(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    real, intent(in)                      :: value
    integer :: keyIndex
    logical :: isExist

    Call pushSLTableRealRP(this, key, real(value, rp))

end subroutine

subroutine pushSLTableRealArray(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    real, dimension(:), intent(in)        :: value
    real(rp), allocatable                 :: tmpValue(:)
    integer :: keyIndex
    logical :: isExist
    integer :: nData
    nData = size(value)
    allocate(tmpValue(nData))
    tmpValue = real(value, rp)
    Call pushSLTableRealRPArray(this, key, tmpValue)
    deallocate(tmpValue)

end subroutine

subroutine pushSLTableRealRP(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    real(rp), intent(in)                  :: value
    integer :: keyIndex
    logical :: isExist

    Call this%searchKeyIndex(key, keyIndex, isExist)

    if (isExist) then
        Call this%dataArray(keyIndex)%set(real(value, rp))
    else
        if (keyIndex.gt.this%nTable) then
            Call this%reallocateSLTable(keyIndex)
        end if

        Call this%keys(keyIndex)%set(key, .TRUE.)
        Call this%dataArray(keyIndex)%set(value)
        this%nkey = this%nkey + 1
    end if

end subroutine

subroutine pushSLTableRealRPArray(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    real(rp), allocatable, intent(in)     :: value(:)
    integer :: keyIndex
    logical :: isExist

    Call this%searchKeyIndex(key, keyIndex, isExist)

    if (isExist) then
        Call this%dataArray(keyIndex)%set(value)
    else
        if (keyIndex.gt.this%nTable) then
            Call this%reallocateSLTable(keyIndex)
        end if

        Call this%keys(keyIndex)%set(key, .TRUE.)
        Call this%dataArray(keyIndex)%set(value)
        this%nkey = this%nkey + 1
    end if

end subroutine


subroutine pushSLTableInt(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    integer, intent(in)                   :: value
    integer :: keyIndex
    logical :: isExist

    Call this%searchKeyIndex(key, keyIndex, isExist)

    if (isExist) then
        Call this%dataArray(keyIndex)%set(real(value, rp))
    else
        if (keyIndex.gt.this%nTable) then
            Call this%reallocateSLTable(keyIndex)
        end if

        Call this%keys(keyIndex)%set(key, .TRUE.)
        Call this%dataArray(keyIndex)%set(value)
        this%nkey = this%nkey + 1
    end if

end subroutine

subroutine pushSLTableIntArray(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    integer, allocatable, intent(in)      :: value(:)
    integer :: keyIndex
    logical :: isExist

    Call this%searchKeyIndex(key, keyIndex, isExist)

    if (isExist) then
        Call this%dataArray(keyIndex)%set(value)
    else
        if (keyIndex.gt.this%nTable) then
            Call this%reallocateSLTable(keyIndex)
        end if

        Call this%keys(keyIndex)%set(key, .TRUE.)
        Call this%dataArray(keyIndex)%set(value)
        this%nkey = this%nkey + 1
    end if

end subroutine

subroutine pushSLTableChar(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    character(len=*), intent(in)          :: value
    integer :: keyIndex
    logical :: isExist

    Call this%searchKeyIndex(key, keyIndex, isExist)

    if (isExist) then
        Call this%dataArray(keyIndex)%set(value)
    else
        if (keyIndex.gt.this%nTable) then
            Call this%reallocateSLTable(keyIndex)
        end if

        Call this%keys(keyIndex)%set(key, .TRUE.)
        Call this%dataArray(keyIndex)%set(value)
        this%nkey = this%nkey + 1
    end if

end subroutine

subroutine pushSLTableCharArray(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    character(len=CHAR_LEN), allocatable, intent(in) :: value(:)
    integer :: keyIndex
    logical :: isExist

    Call this%searchKeyIndex(key, keyIndex, isExist)

    if (isExist) then
        Call this%dataArray(keyIndex)%set(value)
    else
        if (keyIndex.gt.this%nTable) then
            Call this%reallocateSLTable(keyIndex)
        end if

        Call this%keys(keyIndex)%set(key, .TRUE.)
        Call this%dataArray(keyIndex)%set(value)
        this%nkey = this%nkey + 1
    end if

end subroutine


subroutine pushSLTableLogical(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    logical, intent(in)          :: value
    integer :: keyIndex
    logical :: isExist

    Call this%searchKeyIndex(key, keyIndex, isExist)

    if (isExist) then
        Call this%dataArray(keyIndex)%set(value)
    else
        if (keyIndex.gt.this%nTable) then
            Call this%reallocateSLTable(keyIndex)
        end if

        Call this%keys(keyIndex)%set(key, .TRUE.)
        Call this%dataArray(keyIndex)%set(value)
        this%nkey = this%nkey + 1
    end if

end subroutine

subroutine pushSLTableLogicalArray(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    logical, allocatable, intent(in) :: value(:)
    integer :: keyIndex
    logical :: isExist

    Call this%searchKeyIndex(key, keyIndex, isExist)

    if (isExist) then
        Call this%dataArray(keyIndex)%set(value)
    else
        if (keyIndex.gt.this%nTable) then
            Call this%reallocateSLTable(keyIndex)
        end if

        Call this%keys(keyIndex)%set(key, .TRUE.)
        Call this%dataArray(keyIndex)%set(value)
        this%nkey = this%nkey + 1
    end if

end subroutine

function getSLTableReal(this, key, ivar) result(value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    integer,optional                      :: ivar
    real(RP)                              :: value

    integer :: keyIndex
    logical :: isExist

    integer :: nData, idata

    type(typString),allocatable     :: parsedString(:)
    integer :: nargs

    Call this%searchKeyIndex(key, keyIndex, isExist)

    value = 0.0_RP
    if (isExist) then

        nData = this%dataArray(keyIndex)%nData

        if (nData.ge.1) then
            if (this%dataArray(keyIndex)%isNum) then

                if (present(ivar)) then
                    write(*,*) nData, ivar

                    if ((ivar.le.nData).and.(ivar.ge.1)) then
                        value = this%dataArray(keyIndex)%arrR(ivar)
                    else
                        write(*,*) "    [ERROR] getSLTableString(key, ivar), key : ", trim(key)
                        write(*,*) "            ivar is over than the size of array."
                        write(*,*) "              ivar : ", ivar, " nArray : ", nData
                        stop
                    end if
                else
                    value = this%dataArray(keyIndex)%arrR(1)
                end if

            else if (this%dataArray(keyIndex)%isStr) then

                if (present(ivar)) then

                    if (nData.eq.1) then

                        Call parse(this%dataArray(keyIndex)%arrS(1)," ", parsedString, nargs)

                        if (ivar.le.nargs) then
                            value = parsedString(ivar)%getReal()
                        else
                            write(*,*) "    [ERROR] getSLTableString(key, ivar), key : ", trim(key)
                            write(*,*) "            ivar is over than the size of array."
                            write(*,*) "              ivar : ", ivar, " nArray : ", nargs
                            stop
                        end if

                        do idata = 1,nData
                            Call parsedString(idata)%destroy()
                        enddo

                        deallocate(parsedString)

                    else

                        if ((ivar.le.nData).and.(ivar.ge.1)) then
                            value = this%dataArray(keyIndex)%arrS(ivar)%getReal()
                        else
                            write(*,*) "    [ERROR] getSLTableString(key, ivar), key : ", trim(key)
                            write(*,*) "            ivar is over than the size of array."
                            write(*,*) "              ivar : ", ivar, " nArray : ", nData
                            stop
                        end if

                    end if

                else
                    value = this%dataArray(keyIndex)%arrS(1)%getReal()
                end if

            else
                write(*,*) "    [ERROR] getSLTableReal, key : ", trim(key)
                write(*,*) "            data is not number nor number type string."
                stop
            end if
        else
            write(*,*) "    [ERROR] getSLTableReal, key : ", trim(key)
            write(*,*) "            the size of data is zero."
            stop
        end if
    else
        call this%printSearchError(key)
    end if

end function

subroutine getSLTableRealArray(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    real(rp), allocatable, intent(out)        :: value(:)

    integer :: keyIndex
    logical :: isExist
    integer :: nData, idata

    type(typString),allocatable     :: parsedString(:)
    integer :: nargs


    Call this%searchKeyIndex(key, keyIndex, isExist)
    if (isExist) then
        nData = this%dataArray(keyIndex)%nData

        if (nData.ge.1) then
            if (this%dataArray(keyIndex)%isNum) then

                !!!... Allocate Array
                if (allocated(value)) deallocate(value)
                allocate(value(nData))

                value = this%dataArray(keyIndex)%arrR

            else if (this%dataArray(keyIndex)%isStr) then

                if (nData.eq.1) then

                    Call parse(this%dataArray(keyIndex)%arrS(1)," ", parsedString, nargs)
                    nData = nargs

                    !!!... Allocate Array
                    if (allocated(value)) deallocate(value)
                    allocate(value(nData))

                    do idata = 1,nData
                        value(idata) = parsedString(idata)%getReal()
                    enddo

                    do idata = 1,nData
                        Call parsedString(idata)%destroy()
                    enddo

                    deallocate(parsedString)
                else

                    !!!... Allocate Array
                    if (allocated(value)) deallocate(value)
                    allocate(value(nData))

                    do idata = 1,nData
                        value(idata) = this%dataArray(keyIndex)%arrS(idata)%getReal()
                    enddo
                end if

            else
                write(*,*) "    [ERROR] getSLTableRealArray, key : ", trim(key)
                write(*,*) "            data is not number nor number type string."
                stop
            end if
        else
            write(*,*) "    [ERROR] getSLTableRealArray, key : ", trim(key)
            write(*,*) "            the size of data is zero."
            stop
        end if
    else
        call this%printSearchError(key)
    end if

end subroutine

function getSLTableInt(this, key, ivar) result(value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    integer,optional                      :: ivar
    integer                               :: value
    value = int(this%getReal(key, ivar))
end function

subroutine getSLTableIntArray(this, key, value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    integer, allocatable, intent(out)     :: value(:)
    real(rp), allocatable :: rValue(:)
    integer :: nData
    Call this%getRealArray(key, rValue)
    nData = sizeof(rValue) / sizeof(1.0_RP)
    if (allocated(value)) deallocate(value)
    allocate(value(nData))
    value = int(rValue)
    deallocate(rValue)
end subroutine

function getSLTableString(this, key, ivar) result(value)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    integer, optional                     :: ivar
    type(typString)                       :: value
    integer :: keyIndex
    logical :: isExist
    integer :: nData

    Call this%searchKeyIndex(key, keyIndex, isExist)

    if (isExist) then
        nData = this%dataArray(keyIndex)%nData
        if (nData.ge.1) then
            if (this%dataArray(keyIndex)%isStr) then
                if (present(ivar)) then
                    if ((ivar.le.nData).and.(ivar.ge.1)) then
                        value = this%dataArray(keyIndex)%arrS(ivar)
                    else
                        write(*,*) "    [ERROR] getSLTableString(key, ivar), key : ", trim(key)
                        write(*,*) "            ivar is over than the size of array."
                        write(*,*) "              ivar : ", ivar, " nArray : ", nData
                        stop
                    end if
                else
                    value = this%dataArray(keyIndex)%arrS(1)
                end if
            else
                write(*,*) "    [ERROR] getSLTableString, key : ", trim(key)
                write(*,*) "            data is not string type."
                stop
            end if
        else
            write(*,*) "    [ERROR] getSLTableReal, key : ", trim(key)
            write(*,*) "            the size of data is zero."
            stop
        end if
    else
        call this%printSearchError(key)
    end if

end function

subroutine getSLTableStringArray(this, key, value, delims)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    character(len=*),optional             :: delims

    type(typString), allocatable          :: value(:)

    integer :: keyIndex
    logical :: isExist
    integer :: nData, idata

    type(typString),allocatable     :: parsedString(:)
    integer :: nargs

    Call this%searchKeyIndex(key, keyIndex, isExist)
    if (isExist) then
        nData = this%dataArray(keyIndex)%nData

        if (nData.ge.1) then
            if (this%dataArray(keyIndex)%isStr) then

                if (nData.eq.1) then

                    if (present(delims)) then
                        Call parse(this%dataArray(keyIndex)%arrS(1),delims, value, nargs)
                    else
                        if (allocated(value)) deallocate(value)
                        allocate(value(1))
                        value = this%dataArray(keyIndex)%arrS(1)
                    end if
                else
                    !!!... Allocate Array
                    if (allocated(value)) deallocate(value)
                    allocate(value(nData))

                    do idata = 1,nData
                        value(idata) = this%dataArray(keyIndex)%arrS(idata)
                    enddo
                end if
            else
                write(*,*) "    [ERROR] getSLTableStringArray, key : ", trim(key)
                write(*,*) "            data is not string type."
                stop
            end if
        else
            write(*,*) "    [ERROR] getSLTableStringArray, key : ", trim(key)
            write(*,*) "            the size of data is zero."
            stop
        end if
    else
        call this%printSearchError(key)
    end if

end subroutine


subroutine reallocateSLTable(this, tableSize)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    integer, intent(in)                   :: tableSize

    !! Local Variables
    type(typString), allocatable          :: tmpKeys(:)
    type(typDataArray), allocatable       :: tmpDataArray(:)
    integer :: ikey
    integer :: tmpNKey, tmpNTable

    if (tableSize.eq.0) then
        !!... destroy dynamic array
        Call this%destroy()

    else if (tableSize.ne.this%nTable) then

        allocate(tmpKeys(tableSize))
        allocate(tmpDataArray(tableSize))

        tmpNKey = this%nKey

        if (this%nKey.ge.tableSize) then
            tmpNKey = tableSize
        end if

        if (tableSize.lt.this%nTable) then

            do ikey = 1, tableSize
                tmpKeys(ikey)      = this%keys(ikey)
                tmpDataArray(ikey) = this%dataArray(ikey)
            end do
        else

            do ikey = 1, this%nTable
                tmpKeys(ikey)      = this%keys(ikey)
                tmpDataArray(ikey) = this%dataArray(ikey)
            end do

        end if

        Call this%destroy()

        this%nKey   = tmpNKey
        this%nTable = tableSize

        allocate(this%keys(this%nTable))
        allocate(this%dataArray(this%nTable))

        do ikey = 1, this%nTable
            this%keys(ikey)      = tmpKeys(ikey)
            this%dataArray(ikey) = tmpDataArray(ikey)
        end do

    end if

end subroutine

subroutine destroySLTable(this)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    integer :: ikey

    do ikey = 1, this%nTable
        Call this%keys(ikey)%destroy()
        Call this%dataArray(ikey)%destroy()
    end do

    if (allocated(this%keys)) deallocate(this%keys)
    if (allocated(this%dataArray)) deallocate(this%dataArray)

    this%nKey   = 0
    this%nTable = 0

end subroutine

subroutine printSLTable(this)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    integer :: ikey, idata, nData

    if (this%nKey.ge.1) then
        write(*,*) "LookUpTable : ", trim(this%name)
        write(*,*) "{"
        do ikey = 1, this%nKey

            nData = this%dataArray(ikey)%nData

            !!... Write Number
            if (this%dataArray(ikey)%isNum) then
                if(this%dataArray(ikey)%isInt) then
                    if (nData.eq.1) then
                        write(*,1021) trim(this%keys(ikey)%get()), &
                                      this%dataArray(ikey)%arrI(1)
                    else
                        write(*,1020) trim(this%keys(ikey)%get()), nData
                        write(*,*) "    ("
                        do idata = 1,nData
                            write(*,1022) this%dataArray(ikey)%arrI(idata)
                        enddo
                        write(*,*) "    )"
                    end if
                else
                    if (nData.eq.1) then
                        write(*,1011) trim(this%keys(ikey)%get()), &
                                      this%dataArray(ikey)%arrR(1)
                    else
                        write(*,1010) trim(this%keys(ikey)%get()), nData
                        write(*,*) "    ("
                        do idata = 1,nData
                            write(*,1012) this%dataArray(ikey)%arrR(idata)
                        enddo
                        write(*,*) "    )"
                    end if
                endif
            end if

            !!... Write Character
            if (this%dataArray(ikey)%isStr) then
                if (nData.eq.1) then
                    write(*,1111) trim(this%keys(ikey)%get()), &
                                  trim(this%dataArray(ikey)%arrS(1)%get())
                else
                    write(*,1110) trim(this%keys(ikey)%get()), nData
                    write(*,*) "    ("
                    do idata = 1,nData
                        write(*,1112) trim(this%dataArray(ikey)%arrS(idata)%get())
                    enddo
                    write(*,*) "    )"
                end if
            end if

            !!... Write Logical
            if (this%dataArray(ikey)%isLog) then
                if (nData.eq.1) then
                    write(*,1211) trim(this%keys(ikey)%get()), &
                                  this%dataArray(ikey)%arrL(1)
                else
                    write(*,1210) trim(this%keys(ikey)%get()), nData
                    write(*,*) "    ("
                    do idata = 1,nData
                        write(*,1212) this%dataArray(ikey)%arrL(idata)
                    enddo
                    write(*,*) "    )"
                end if
            end if

        end do
        write(*,*) "}"
    else
        write(*,*) "LookUpTable"
        write(*,*) "}"
        write(*,*) "    empty."
        write(*,*) "}"
    end if

    1010 format(5x,a,3x,"numVector",3x,i6)
    1011 format(5x,a,3x,"num",f15.6)
    1012 format(7x,f15.6)

    1020 format(5x,a,3x,"intVector",3x,i6)
    1021 format(5x,a,3x,"int",i15)
    1022 format(7x,i15)

    1110 format(5x,a,3x,"charVector",3x,a)
    1111 format(5x,a,3x,"char",3x,"[ ",a," ]")
    1112 format(10x,5x,"[ ",a," ]")

    1210 format(5x,a,3x,"logVector",3x,l2)
    1211 format(5x,a,3x,"log",3x,l2)
    1212 format(10x,5x,l2)

end subroutine

subroutine searchKeyIndex(this, key, keyIndex, isExist)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    character(len=*), intent(in)          :: key
    integer,intent(out)                   :: keyIndex
    logical,intent(out)                   :: isExist

    integer :: ikey

    !!... Optimization is needed !!!
    isExist = .FALSE.
    do ikey = 1, this%nKey
        if (isStringEqual(trim(key), this%keys(ikey), this%strongSearch)) then
            isExist = .TRUE.
            keyIndex = ikey
        end if
    enddo

    if (isExist.EQV..FALSE.) then
        keyIndex = this%nKey + 1
    end if

end subroutine

subroutine printSearchError(this, key)
    Implicit None
    class(typSLookUpTable), intent(inout) :: this
    Character(len=*), intent(in) :: key
    write(*,*) " Unknown Key : ", key, " in LookUpTable ", trim(this%name)
end subroutine
