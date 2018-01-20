Subroutine testDictionary()
    Implicit None
    Character(len=CHAR_LEN) :: dictName

    ! Call testPointer()

    Call testFileMakeDict()

    ! Call testMakeDict()

    ! Call mkSLTable(SLTable)
    !
    ! ! Call tmpDict%initialize(dictName)
    !
    ! Call tmpDict%initialize(dictName, SLTable)

end subroutine

! Subroutine testPointer()
!     Implicit None
!
!     integer, target  :: a1, a2
!     integer, pointer :: pa, pb
!
!     a1 = 1
!     a2 = 2
!
!     pa => a1
!     pb => a2
!
!     write(*,*) a1, a2, pa, pb
!
!     pa = 3
!
!     write(*,*) a1, a2, pa, pb
!
! End Subroutine

Subroutine testFileMakeDict()
    Implicit None

    type(typDictionary), pointer :: dict

    type(typSLookUpTable), pointer :: slTable

    integer :: fid
    character(len=CHAR_LEN) ::fileDir
    character(len=CHAR_LEN) ::fileName
    character(len=CHAR_LEN) ::fileExt
    character(len=CHAR_LEN) ::filePath
    integer :: ios

    integer :: nLine, iLine, subNline
    character(len = fileLineLength) :: tmpLine
    character(len = fileLineLength),allocatable :: charArray(:)
    character(len = fileLineLength),allocatable :: subCharArray(:)
    character(len = fileLineLength),allocatable :: tmpCharArray(:)

    integer :: id

    fid      = 101

    fileDir  = ""
    fileName = "testDict"
    fileExt  = ""

    filePath = trim(fileDir)//trim(fileName)//trim(fileExt)

    open(unit   = fid,        &
         file   = filePath,   &
         status = 'old', &
         iostat = ios)

    ! Call dict%initialize(fileName, fid)

    !! Call readMakeCharArrayFromFile(fid, "{", "}", CharArray, nLine)

    Call readMakeCharArrayFromFile(fid, CharArray, nLine, " ", " ")

    ! !! Call readMakeCharArrayFromCharArray(charArray, subCharArray, subNline, "{","}")
    ! Call readMakeCharArrayFromCharArray(charArray, subCharArray, subNline, "{","")

    ! do iLine = 1,nLine
    !     write(*,*) trim(CharArray(iLine))
    ! enddo
    !
    ! do iLine = 1,subNline
    !     write(*,*) trim(subCharArray(iLine))
    ! enddo

    ! nLine = 0
    ! !! File Read
    ! do
    !     !!... Read Line and exit if file end
    !     read(fid, "(a)", iostat = ios) tmpLine; if (ios < 0) exit
    !     nLine = nLine + 1
    !     if (nLine.eq.1) then
    !         allocate( charArray(1) )
    !         charArray(1) = tmpLine
    !     else
    !         Call move_alloc(charArray, tmpCharArray)
    !         allocate( charArray(nLine) )
    !         charArray(1:nLine-1) = tmpCharArray
    !         charArray(nLine)     = tmpLine
    !     end if
    ! enddo

    if (nLine.ge.1) then
        Call initDict_NameFileCharArray(fileName, charArray, id)
        ! Call dict%sLTable%print()

        Call dictDB(1)%sLTable%print()
        Call dictDB(2)%sLTable%print()

        Call dictDB(1)%subDictTable%print()
        Call dictDB(2)%subDictTable%print()

    end if

End Subroutine


Subroutine writeErrorFileDict(errorMessage, fileLine, lineNumber)
    Implicit None
    CHARACTER(len=*), intent(in) :: errorMessage
    CHARACTER(len=*), optional   :: fileLine
    integer, optional            :: lineNumber

    write(*,*) ""
    write(*,1000) trim(errorMessage)
    write(*,*) ""
    if (present(fileLine)) then
    write(*,1001) trim(fileLine)
    write(*,*) ""
    end if
    if (present(lineNumber)) then
    write(*,1002) lineNumber
    end if
    stop

1000 format("    [ERROR] ",a)
1001 format(a)
1002 format("    n-th line : ", i10 )

End Subroutine

Subroutine testShowIndex()
    Implicit None
    integer :: fid

    character(len=CHAR_LEN) ::fileDir
    character(len=CHAR_LEN) ::fileName
    character(len=CHAR_LEN) ::fileExt
    character(len=CHAR_LEN) ::filePath

    character(len=fileLineLength) :: tmpLine, modLine

    integer :: ios

    integer :: nArg
    character(len = CHAR_LEN), allocatable :: cArg(:), scArg(:), lcArg(:)
    integer, allocatable                   :: iArg(:)
    real(RP), allocatable                  :: rArg(:)
    logical,allocatable                    :: numFlag(:)
    integer,allocatable                    :: scFlag(:)
    logical :: iflag

    fid      = 101

    fileDir  = ""
    fileName = "testDict"
    fileExt  = ""

    filePath = trim(fileDir)//trim(fileName)//trim(fileExt)


    open(unit   = fid,        &
         file   = filePath,   &
         status = 'old', &
         iostat = ios)

    DO
        read(fid, "(a)", iostat = ios) tmpLine
        IF (ios < 0) EXIT
        ! write(*,*) strHeader(trim(tmpLine)), findLineEnd(tmpLine), "    ",trim(tmpLine)

        modLine = insertSpaceSpecialCharacter(tmpLine)
        Call splitLine(modLine, nArg, cArg, scArg, lcArg, iArg, rArg, numFlag, scFlag, iflag)

        if (nArg.ge.1) write(*,*) nArg, trim(modLine)
        if (nArg.ge.1) write(*,*) numFlag, scFlag
        if (nArg.ge.1) write(*,*) "----------------------------"

    END DO

    close(fid)

End Subroutine

Subroutine addSaveArray(nSaveArray, &
                        saveStrArray, saveNumArray, saveNumFlagArray, saveFlagArray, &
                        tmpStrArray,  tmpNumArray,  tmpNumFlagArray, tmpFlagArray)
    Implicit None
    Integer, intent(inout)                               :: nSaveArray
    Character(len=CHAR_LEN), allocatable, intent(inout)  :: saveStrArray(:)
    real(rp), allocatable, intent(inout)                 :: saveNumArray(:)
    logical, allocatable, intent(inout)                  :: saveNumFlagArray(:)
    integer, allocatable, intent(inout)                  :: saveFlagArray(:)

    Character(len=CHAR_LEN), dimension(:), intent(in)  :: tmpStrArray
    real(rp), dimension(:), intent(in)                 :: tmpNumArray
    logical, dimension(:), intent(in)                  :: tmpNumFlagArray
    integer, dimension(:), intent(in)                  :: tmpFlagArray

    !! Local Save
    integer                                 :: nTmpArray

    Character(len=CHAR_LEN), allocatable :: tmpSaveStrArr(:)
    Real(RP), allocatable                :: tmpSaveNumArr(:)
    logical, allocatable                 :: tmpSaveNumFlagArr(:)
    integer, allocatable                 :: tmpSaveFlagArr(:)

    nTmpArray = sizeof(tmpNumFlagArray)/sizeof(.TRUE.)

    if (nSaveArray.eq.0) then
        if (allocated(saveStrArray))     deallocate(saveStrArray)
        if (allocated(saveNumArray))     deallocate(saveNumArray)
        if (allocated(saveNumFlagArray)) deallocate(saveNumFlagArray)
        if (allocated(saveFlagArray)) deallocate(saveFlagArray)

        if (nTmpArray.ge.1) then
            saveStrArray     = tmpStrArray
            saveNumArray     = tmpNumArray
            saveNumFlagArray = tmpNumFlagArray
            saveFlagArray    = tmpFlagArray
            nSaveArray = nTmpArray
        end if
    else
        if (nTmpArray.ge.1) then
            if (allocated(tmpSaveStrArr))     deallocate(tmpSaveStrArr)
            if (allocated(tmpSaveNumArr))     deallocate(tmpSaveNumArr)
            if (allocated(tmpSaveNumFlagArr)) deallocate(tmpSaveNumFlagArr)
            if (allocated(tmpSaveFlagArr))    deallocate(tmpSaveFlagArr)

            Call move_alloc(saveStrArray,     tmpSaveStrArr)
            Call move_alloc(saveNumArray,     tmpSaveNumArr)
            Call move_alloc(saveNumFlagArray, tmpSaveNumFlagArr)
            Call move_alloc(saveFlagArray,    tmpSaveFlagArr)

            allocate(saveStrArray(nSaveArray + nTmpArray), &
                     saveNumArray(nSaveArray + nTmpArray), &
                     saveNumFlagArray(nSaveArray + nTmpArray), &
                     saveFlagArray(nSaveArray + nTmpArray))

            saveStrArray(1:nSaveArray)     = tmpSaveStrArr(1:nSaveArray)
            saveNumArray(1:nSaveArray)     = tmpSaveNumArr(1:nSaveArray)
            saveNumFlagArray(1:nSaveArray) = tmpSaveNumFlagArr(1:nSaveArray)
            saveFlagArray(1:nSaveArray)    = tmpSaveFlagArr(1:nSaveArray)

            saveStrArray(nSaveArray+1:nSaveArray + nTmpArray)     = tmpStrArray(1:nTmpArray)
            saveNumArray(nSaveArray+1:nSaveArray + nTmpArray)     = tmpNumArray(1:nTmpArray)
            saveNumFlagArray(nSaveArray+1:nSaveArray + nTmpArray) = tmpNumFlagArray(1:nTmpArray)
            saveFlagArray(nSaveArray+1:nSaveArray + nTmpArray)    = tmpFlagArray(1:nTmpArray)

            nSaveArray = nSaveArray + nTmpArray

            deallocate(tmpSaveStrArr, tmpSaveNumArr, tmpSaveNumFlagArr, tmpSaveFlagArr)
        end if
    end if

End Subroutine

Subroutine mkSLTable(SLTable)
    Type(typSLookUpTable) :: SLTable

    Call SLTable%initialize()

    Call SLTable%push("a", 123)
    Call SLTable%push("b", "is a")

End Subroutine

Subroutine readMakeCharArrayFromCharArray(charArray, subCharArray, nLine, charStart, charEnd)
    Implicit None
    !Character(len=fileLineLength), allocatable   :: CharArray(:)
    Character(len=fileLineLength), dimension(:)  :: CharArray
    Character(len=fileLineLength), allocatable   :: subCharArray(:)
    integer, optional                            :: nLine
    Character(len=*), optional                   :: charStart, charEnd
    !! ----------------------------------------------------------------------
    logical :: readUntilEnd
    logical :: isCharActive
    Character(len=fileLineLength) :: tmpLine, modLine
    Character(len=fileLineLength), allocatable  :: tmpCharArray(:)
    integer :: nn, nTotalLine
    integer :: nSubLine
    !! ----------------------------------------------------------------------
    if (allocated(subCharArray)) deallocate(subCharArray)
    if (allocated(tmpCharArray)) deallocate(tmpCharArray)

    nTotalLine = sizeof(CharArray) / sizeof(tmpLine)

    if (nTotalLine.ge.1) then

        readUntilEnd = .FALSE.
        if ( present(charEnd) ) then
            if (len_trim(charEnd).eq.0) readUntilEnd = .TRUE.
        end if

        if ( present(charStart) ) then
            isCharActive = .FALSE.
            if (len_trim(charStart).eq.0) isCharActive = .TRUE.
        else
            isCharActive = .TRUE.
        end if

        nSubLine = 0
        do nn = 1, nTotalLine
            tmpLine = charArray(nn)
            modLine = tmpLine
            Call compactChar(modLine)

            if ( present(charStart) ) then
                if (isStringEqual(modLine, charStart)) isCharActive = .TRUE.
            end if

            if (isCharActive) then
                nSubLine = nSubLine + 1
                if (nSubLine.eq.1) then
                    allocate( subCharArray(1) )
                    subCharArray(1) = tmpLine
                else
                    Call move_alloc(subCharArray, tmpCharArray)

                    allocate( subCharArray(nSubLine) )
                    subCharArray(1:nSubLine-1) = tmpCharArray
                    subCharArray(nSubLine)     = tmpLine

                end if
            end if

            if ((readUntilEnd.EQV..FALSE.).AND.isCharActive) then
                if ( present(charEnd) ) then
                    if (isStringEqual(modLine, charEnd,.TRUE.)) exit
                end if
            end if

        enddo

        if (present(nLine)) nLine = nSubLine
        if (allocated(tmpCharArray)) deallocate(tmpCharArray)

    else
        if (present(nLine)) nLine = 0
    end if

End Subroutine

Subroutine readMakeCharArrayFromFile(fileIndex, subCharArray, nLine, charStart, charEnd, fRewind)
    Implicit None
    Integer, intent(in)                         :: fileIndex
    Character(len=fileLineLength), allocatable  :: subCharArray(:)
    integer, optional                           :: nLine
    Character(len=*), optional                  :: charStart, charEnd
    logical, optional                           :: fRewind
    !! ----------------------------------------------------------------------
    logical :: isCharActive
    integer :: ios
    Character(len=fileLineLength) :: tmpLine
    Character(len=fileLineLength), allocatable  :: tmpCharArray(:)
    integer :: nn
    logical :: readUntilEnd
    !! ----------------------------------------------------------------------

    if (allocated(subCharArray)) deallocate(subCharArray)
    if (allocated(tmpCharArray)) deallocate(tmpCharArray)

    readUntilEnd = .FALSE.
    if ( present(charEnd) ) then
        if (len_trim(charEnd).eq.0) readUntilEnd = .TRUE.
    end if

    if ( present(charStart) ) then
        isCharActive = .FALSE.
        if (len_trim(charStart).eq.0) isCharActive = .TRUE.
    else
        isCharActive = .TRUE.
    end if

    nn = 0
    !! File Read
    do
        !!... Read Line and exit if file end
        read(fileIndex, "(a)", iostat = ios) tmpLine; if (ios < 0) exit

        if ( present(charStart) ) then
            if (isStringEqual(tmpLine, charStart,.TRUE.)) isCharActive = .TRUE.
        end if

        if (isCharActive) then
            nn = nn + 1
            if (nLine.eq.1) then
                allocate( subCharArray(1) )
                subCharArray(1) = tmpLine
            else
                Call move_alloc(subCharArray, tmpCharArray)
                allocate( subCharArray(nn) )
                subCharArray(1:nn-1) = tmpCharArray
                subCharArray(nn)     = tmpLine
            end if
        end if

        if ((readUntilEnd.EQV..FALSE.).AND.isCharActive) then
            if ( present(charEnd) ) then
                if (isStringEqual(tmpLine, charEnd,.TRUE.)) exit
            end if
        end if

    enddo

    if (present(nLine)) nLine = nn
    if (allocated(tmpCharArray)) deallocate(tmpCharArray)

End Subroutine
