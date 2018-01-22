Subroutine testDictionary()
    Implicit None
    Character(len=CHAR_LEN) :: dictName


    ! Call testReadCharArray()

    Call testFileMakeDict()

    ! Call testMakeDict()

    ! Call mkSLTable(SLTable)
    !
    ! ! Call tmpDict%initialize(dictName)
    !
    ! Call tmpDict%initialize(dictName, SLTable)

end subroutine

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

    Call readMakeCharArrayFromFile(fid, CharArray, nLine, " ", " ")

    if (nLine.ge.1) then
        Call initDict_NameCharArray(fileName, charArray, id)

        Call dictDB(1)%sLTable%print()
        Call dictDB(2)%sLTable%print()
        Call dictDB(3)%sLTable%print()
        Call dictDB(4)%sLTable%print()
        Call dictDB(5)%sLTable%print()

        Call dictDB(1)%subDictTable%print()
        Call dictDB(2)%subDictTable%print()

        Call dictDB(3)%subDictTable%print()
        Call dictDB(5)%subDictTable%print()

    end if

End Subroutine

Subroutine testReadCharArray()
    Implicit None
    integer :: fid
    character(len=CHAR_LEN) ::fileDir
    character(len=CHAR_LEN) ::fileName
    character(len=CHAR_LEN) ::fileExt
    character(len=CHAR_LEN) ::filePath
    integer :: ios
    character(len = fileLineLength),allocatable :: charArray(:)
    character(len = fileLineLength),allocatable :: subCharArray(:)
    integer :: nLine, iLine,jLine
    integer :: subNline

    fid      = 101

    fileDir  = ""
    fileName = "testDict"
    fileExt  = ""

    filePath = trim(fileDir)//trim(fileName)//trim(fileExt)

    open(unit   = fid,        &
         file   = filePath,   &
         status = 'old', &
         iostat = ios)

    Call readMakeCharArrayFromFile(fid, CharArray, nLine, " ", " ")

    do iLine = 1, nLine
        write(*,*) iLine, trim(charArray(iLine))
    enddo

    Call readMakeCharArrayFromCharArray(charArray, subCharArray, subNline, "{","}", .true.)

    do jLine = 1, subNline
        write(*,*) jLine, trim(subCharArray(jLine))
    enddo

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

Subroutine readMakeCharArrayFromCharArray(charArray, subCharArray, nLine, charStart, charEnd, isSubSet)
    Implicit None
    !Character(len=fileLineLength), allocatable   :: CharArray(:)
    Character(len=fileLineLength), dimension(:)  :: CharArray
    Character(len=fileLineLength), allocatable   :: subCharArray(:)
    integer, optional                            :: nLine
    Character(len=*), optional                   :: charStart, charEnd
    logical, optional                            :: isSubSet
    !! ----------------------------------------------------------------------
    logical :: readUntilEnd
    logical :: isCharActive
    Character(len=fileLineLength) :: tmpLine, modLine
    Character(len=fileLineLength), allocatable  :: tmpCharArray(:)
    integer :: nn, nTotalLine
    integer :: nSubLine

    integer :: iSub

    logical :: subSetOption
    !! ----------------------------------------------------------------------
    if (allocated(subCharArray)) deallocate(subCharArray)
    if (allocated(tmpCharArray)) deallocate(tmpCharArray)

    nTotalLine = sizeof(CharArray) / sizeof(tmpLine)

    subSetOption = .FALSE.
    iSub = 0

    if (present(isSubSet)) subSetOption = isSubSet

    if (nTotalLine.ge.1) then

        readUntilEnd = .FALSE.
        if ( present(charEnd) ) then
            if (len_trim(charEnd).eq.0) then
                readUntilEnd = .TRUE.
                subSetOption = .FALSE.
            end if
        end if

        if ( present(charStart) ) then
            isCharActive = .FALSE.
            if (len_trim(charStart).eq.0) then
                isCharActive = .TRUE.
                subSetOption = .FALSE.
            end if
        else
            isCharActive = .TRUE.
        end if

        nSubLine = 0
        do nn = 1, nTotalLine
            tmpLine = charArray(nn)
            modLine = tmpLine
            Call compactChar(modLine)

            if ( present(charStart) ) then
                if (isStringEqual(modLine, charStart)) then
                    isCharActive = .TRUE.
                    if (subSetOption) iSub = iSub + 1
                end if

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
                    if (isStringEqual(modLine, charEnd,.TRUE.)) then

                        if (subSetOption) then
                            iSub = iSub - 1
                            if (iSub.eq.0) exit
                        else
                            exit
                        end if
                     end if
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


Recursive Subroutine initDict_NameCharArray(dictName, charArray, dictID, parentDictID)
    Implicit None
    Character(len=CHAR_LEN), intent(in)                    :: dictName
    Character(len=fileLineLength),dimension(:), intent(in) :: charArray
    Integer, intent(out)                                   :: dictID
    integer, optional                                      :: parentDictID
    !! integer,intent(in)                  :: fileIOIndex
    !! Local Variables --------------------------------------------------------
    character(len = fileLineLength) :: tmpLine, modLine
    integer :: iLine, jLine, nLine

    !!... Argument Information
    integer :: nArg, nArgValid
    character(len = CHAR_LEN), allocatable :: cArg(:), scArg(:), lcArg(:)
    integer, allocatable                   :: iArg(:)
    real(RP), allocatable                  :: rArg(:)
    logical,allocatable                    :: numFlag(:)
    integer,allocatable                    :: scFlag(:)
    logical :: iflag

    !!... temporary Saved Array
    integer :: nSaveData, nTmpData
    character(len = CHAR_LEN), allocatable :: tmpSaveCArg(:)
    real(RP), allocatable                  :: tmpSaveRArg(:)
    logical,allocatable                    :: tmpSaveNumFlag(:)
    integer,allocatable                    :: tmpSaveFlag(:)

    !! Multi Line Operation
    logical :: isKeyActive, isArrayActive, isDictActive, isKeyExist
    character(len=CHAR_LEN)                :: keySaved, shortKey

    !! Sub Dict
    Character(len=fileLineLength),allocatable :: subDictCharArray(:)
    integer :: nLineSubDict
    integer :: subDictID, shortKeyDictID
    !! Dummy Integer
    integer :: ii
    character(len = CHAR_LEN), allocatable :: tmpKeyCArg(:)
    real(RP), allocatable                  :: tmpKeyRArg(:)
    integer :: tmpParentDictID
    !! Subroutine Start -------------------------------------------------------

    Call initializeDictDataBase(dictName, dictID, parentDictID)

    nLine = sizeof(charArray) / sizeof(tmpLine)

    isKeyActive   = .FALSE.
    isDictActive  = .FALSE.
    isArrayActive = .FALSE.

    iLine = 0
    do
        iLine = iLine + 1
        if (iLine.gt.nLine) exit

        !! Get Line
        tmpLine = charArray(iLine)

        !! Modify Line to be separated correctly
        modLine = insertSpaceSpecialCharacter(tmpLine)

        !! Split Line
        Call splitLine(modLine, nArg, cArg, scArg, lcArg, iArg, rArg, numFlag, scFlag, iflag)

        nArgValid = nArg
        !! No blank line
        if (nArg.ge.1) then
            !! Get Valid Line
            do ii = 1, nArg
                if (scFlag(ii).lt.0) exit
            enddo
            nArgValid = ii - 1
        end if

        !!... KeyWord Search
        if (nArgValid.ge.1) then
            shortKeyDictID = 0

            !! Keyword Start
            if (isKeyActive.EQV..FALSE.) then

                !! Word only can be keyword
                if (scFlag(1).eq.1) then

                    keySaved    = cArg(1);      isKeyActive = .TRUE.

                    Call compactChar(keySaved)

                    !! If Keyword and End Exist
                    if (scFlag(nArgValid).eq.0) then

                        !! Key, Value, End in One Line
                        !!   - format : key  val1 ;
                        if (nArgValid.eq.3) then

                            !! Check Value
                            if ( isWord(scFlag(2:2)) ) then

                                if (numFlag(2)) then
                                    Call dictDB(dictID)%sLTable%push(keySaved, rArg(2))   !! Push Number Value
                                else
                                    Call dictDB(dictID)%sLTable%push(keySaved, cArg(2))   !! Push Char Value
                                end if

                                keySaved = "";              isKeyActive = .FALSE.

                            !! Short Key
                            else if (scFlag(2).eq.122) then

                                shortKey = cArg(2)
                                shortKey = shortKey(2:)

                                isKeyExist = .FALSE.
                                tmpParentDictID = dictID
                                do

                                    !! Search Dictionary in parent dictionary
                                    isKeyExist = dictDB(tmpParentDictID)%sLTable%keyExist(shortKey)

                                    if (isKeyExist) then

                                        if (dictDB(dictID)%sLTable%isNumber(shortKey)) then

                                            !! Get Real Array
                                            Call dictDB(dictID)%sLTable%getRealArray(shortKey, tmpKeyRArg)

                                            !! Push Real Array
                                            Call dictDB(dictID)%sLTable%push(keySaved, tmpKeyRArg)

                                        else if (dictDB(dictID)%sLTable%isCharacter(shortKey)) then

                                            !! Get Character Array
                                            Call dictDB(dictID)%sLTable%getCharArray(shortKey, tmpKeyCArg)

                                            !! Push Character Array
                                            Call dictDB(dictID)%sLTable%push(keySaved, tmpKeyCArg)
                                        else
                                            Call writeErrorFileDict("Wrong Short Key Type is defined. ", shortKey)
                                        end if

                                        keySaved = "";              isKeyActive = .FALSE.

                                        exit

                                    end if

                                    !! Search Dictionary by up shooting direction ( subSubDict -> subDict -> dict )
                                    tmpParentDictID = dictDB(tmpParentDictID)%parentDictID

                                    if (tmpParentDictID.eq.0) then
                                        Call writeErrorFileDict("Short Key is not defined previously. : ", tmpLine)
                                        exit
                                    end if

                                end do

                                if (isKeyExist.EQV..FALSE.) then
                                    Call writeErrorFileDict("Short Key is not defined previously. : ", tmpLine)
                                end if

                            !! Write Error
                            else
                                Call writeErrorFileDict("Wrong value", tmpLine)
                            end if

                        !! Key, Array, End in One Line
                        !!   - format : key  ( val1 val2 ... valN ) ;
                        else
                            if ((nArgValid.ge.5).and.isWord(scFlag(3:nArgValid-2))) then
                                if (isAllTrue(numFlag(3:nArgValid-2))) then
                                    !! Push Number Value
                                    Call dictDB(dictID)%sLTable%push(keySaved, rArg(3:nArgValid-2))
                                else
                                    !! Push Char Value
                                    Call dictDB(dictID)%sLTable%push(keySaved, cArg(3:nArgValid-2))
                                end if
                                keySaved = "";              isKeyActive = .FALSE.
                            else
                                Call writeErrorFileDict("Wrong contents in array.", tmpLine)
                            end if
                        end if

                    !! If Keyword exist, but end not exist
                    else

                        !! If Argument is larger than 2.
                        if (nArgValid.ge.2) then

                            if (isWord(scFlag(2:2))) then

                                if (scFlag(3).eq.0) then
                                    nSaveData = 0
                                    Call addSaveArray(nSaveData,        &
                                                      tmpSaveCArg,      &
                                                      tmpSaveRArg,      &
                                                      tmpSaveNumFlag,   &
                                                      tmpSaveFlag,      &
                                                      cArg(2:2),        &
                                                      rArg(2:2),        &
                                                      numFlag(2:2),     &
                                                      scFlag(2:2))

                                else
                                    Call writeErrorFileDict("Wrong Format. ; should be located after the value.", tmpLine)
                                end if

                            !! Dictionary Start
                            elseif (scFlag(2).eq.1101) then

                                Call writeErrorFileDict(" Dictionary character { should be separated.", tmpLine)

                            !! Array Start
                            elseif ((scFlag(2).eq.1201).or.(scFlag(2).eq.1001)) then

                                !! [ERROR] If Array End is Given
                                !!   - format : key ( val val2 ... val 3)
                                if ((scFlag(nArgValid).eq.1202).or.(scFlag(nArgValid).eq.1002)) then
                                    Call writeErrorFileDict("Wrong Format. ; should be located after the value.", tmpLine)

                                !! If Array End is Not given
                                !!   - format : key ( val val2 ... val 3
                                else if (nArgValid.eq.2) then
                                    nSaveData     = 0
                                    isArrayActive = .TRUE.
                                else
                                    if (isWord( scFlag(3:nArgValid) )) then
                                        nTmpData = nArgValid - 2
                                        !! Save Data
                                        if (nTmpData.ge.1) then
                                            nSaveData     = 0
                                            Call addSaveArray(nSaveData,            &
                                                              tmpSaveCArg,          &
                                                              tmpSaveRArg,          &
                                                              tmpSaveNumFlag,       &
                                                              tmpSaveFlag,          &
                                                              cArg(3:nArgValid),    &
                                                              rArg(3:nArgValid),    &
                                                              numFlag(3:nArgValid), &
                                                              scFlag(3:nArgValid))
                                            isArrayActive = .TRUE.
                                        end if
                                    else
                                        Call writeErrorFileDict("Wrong line description for array. ", tmpLine)
                                    end if

                                end if

                            !! Error : Dictionary End
                            else if (scFlag(2).eq.1102) then
                                Call writeErrorFileDict(" Dictionary character } should be separated.", tmpLine)
                            !! Error : Array End
                            else if ((scFlag(2).eq.1202).or.(scFlag(2).eq.1002)) then
                                Call writeErrorFileDict("Wrong Format. Wrong Array End.", tmpLine)
                            !! Error : Wrong line
                            else
                                Call writeErrorFileDict("Wrong contents in array. ", tmpLine)
                            end if

                        end if

                    end if


                !! Keyword End
                else if (scFlag(1).eq.0) then

                    Call writeErrorFileDict("Wrong Format. Wrong End.", tmpLine)

                !! Dictionary Input
                else if  (scFlag(1).eq.122) then

                    if (nArgValid.eq.1) then

                        shortKey   = cArg(1)
                        shortKey   = shortKey(2:)

                        isKeyExist = .FALSE.
                        tmpParentDictID = dictID
                        do
                            !! Search Dictionary by up shooting direction ( subSubDict -> subDict -> dict )
                            tmpParentDictID = dictDB(tmpParentDictID)%parentDictID

                            if (tmpParentDictID.eq.0) then
                                Call writeErrorFileDict("Short Key is not defined previously. : ", tmpLine)
                                exit
                            end if

                            !! Search Dictionary in parent dictionary
                            isKeyExist = dictDB(tmpParentDictID)%subDictTable%keyExist(shortKey)

                            if (isKeyExist) then

                                !! Get Dictionary ID
                                Call dictDB(tmpParentDictID)%subDictTable%getInt(shortKey, shortKeyDictID)

                                !! Add Dictionary
                                Call dictDB(dictID)%addDict( dictDB(shortKeyDictID) )
                                keySaved = "";              isKeyActive = .FALSE.

                                exit

                            end if

                        end do

                        if (isKeyExist.EQV..FALSE.) then
                            Call writeErrorFileDict("No dictionary is defined previously. ", tmpLine)
                        end if

                    else
                        Call writeErrorFileDict("Wrong End. No end after dictionary short key.", tmpLine)
                    end if

                end if

            else
                if (nArgValid.eq.1) then

                    !! Add Word to Array
                    if (isWord(scFlag(1:1))) then
                        if (isArrayActive) then
                            Call addSaveArray(nSaveData,            &
                                              tmpSaveCArg,          &
                                              tmpSaveRArg,          &
                                              tmpSaveNumFlag,       &
                                              tmpSaveFlag,          &
                                              cArg(1:1),    &
                                              rArg(1:1),    &
                                              numFlag(1:1), &
                                              scFlag(1:1))
                        else
                            Call writeErrorFileDict("Bad line. Key : ", trim(keySaved))
                        end if

                    !! Dictionary Start
                    else if (scFlag(1).eq.1101) then

                        iLine = iLine - 1

                        !!... Need Add
                        Call readMakeCharArrayFromCharArray(charArray(iLine:), &
                                                            subDictCharArray, &
                                                            nLineSubDict, &
                                                            "{",&
                                                            "}" ,.TRUE.)

                        if (nLineSubDict.ge.1) then

                            !! Call Sub Dict
                            Call initDict_NameCharArray(keySaved, &
                                                        subDictCharArray(2:nLineSubDict),&
                                                        subDictID, &
                                                        dictID)

                            !! Skip line
                            iLine = iLine + nLineSubDict

                            !! Add SubDict
                            Call dictDB(dictID)%subDictTable%push(keySaved, subDictID)
                            isKeyActive = .FALSE.;       keySaved=""

                        end if

                    !! Dictionary End
                    else if (scFlag(1).eq.1102) then

                        Call writeErrorFileDict("Bad line. Key : ", trim(keySaved))

                    !! Array Start
                    else if ((scFlag(1).eq.1201).or.(scFlag(1).eq.1001)) then

                        if (isArrayActive.EQV..FALSE.) then
                            nSaveData     = 0
                            isArrayActive = .TRUE.
                        else
                            Call writeErrorFileDict("Wrong Format. Bad array start (Another array exist.)", tmpLine)
                        end if

                    !! Array End
                    else if ((scFlag(1).eq.1202).or.(scFlag(1).eq.1002)) then
                        Call writeErrorFileDict("Wrong Format. Bad array end", tmpLine)

                    !! Bad Line
                    else
                        Call writeErrorFileDict("Bad line.", tmpLine)
                    end if

                else if (nArgValid.ge.2) then

                    !! Word Start
                    if (isWord(scFlag(1:1))) then

                        if (scFlag(nArgValid).eq.0) then

                            if (isArrayActive) then

                                if ((scFlag(nArgValid-1).eq.1202).or.(scFlag(nArgValid-1).eq.1002)) then

                                    nTmpData = nArgValid - 2
                                    !! Save Data
                                    if (nTmpData.ge.1) then
                                        Call addSaveArray(nSaveData,            &
                                                          tmpSaveCArg,          &
                                                          tmpSaveRArg,          &
                                                          tmpSaveNumFlag,       &
                                                          tmpSaveFlag,          &
                                                          cArg(1:nArgValid-2),    &
                                                          rArg(1:nArgValid-2),    &
                                                          numFlag(1:nArgValid-2), &
                                                          scFlag(1:nArgValid-2))
                                    end if

                                    !! Push
                                    if ( isWord(tmpSaveFlag) ) then
                                        if (isAllTrue(tmpSaveNumFlag)) then
                                            !! Push Number Value
                                            Call dictDB(dictID)%sLTable%push(keySaved, tmpSaveRArg)
                                        else
                                            !! Push Char Value
                                            Call dictDB(dictID)%sLTable%push(keySaved, tmpSaveCArg)
                                        end if
                                        keySaved = "";              isKeyActive = .FALSE.
                                    else
                                        Call writeErrorFileDict("Wrong contents in array. key : ", keySaved)
                                    end if
                                    isArrayActive = .FALSE.

                                else
                                    Call writeErrorFileDict("Wrong Format. Bad array end", tmpLine)
                                end if

                            else
                                Call writeErrorFileDict("Bad line.", tmpLine)
                            end if

                        else if ((scFlag(nArgValid).eq.1202).or.(scFlag(nArgValid).eq.1002)) then
                            Call writeErrorFileDict("Wrong Format. Bad array end", tmpLine)

                        else

                            !!! Add Array
                            if (isArrayActive) then
                                if (isWord(scFlag)) then
                                    nTmpData = nArgValid - 2
                                    !! Save Data
                                    if (nTmpData.ge.1) then
                                        Call addSaveArray(nSaveData,        &
                                                          tmpSaveCArg,      &
                                                          tmpSaveRArg,      &
                                                          tmpSaveNumFlag,   &
                                                          tmpSaveFlag,      &
                                                          cArg,             &
                                                          rArg,             &
                                                          numFlag,          &
                                                          scFlag)
                                    end if
                                else
                                    Call writeErrorFileDict("Bad line.", tmpLine)
                                end if
                            end if

                        end if

                    !! Dictionary Start
                    else if (scFlag(1).eq.1101) then

                        Call writeErrorFileDict(" Dictionary character { should be separated.", tmpLine)

                    !! Dictionary End
                    else if (scFlag(1).eq.1102) then

                        Call writeErrorFileDict(" Dictionary character } should be separated.", tmpLine)

                    !! Array Start
                    else if ((scFlag(1).eq.1201).or.(scFlag(1).eq.1001)) then

                        if (isArrayActive.EQV..FALSE.) then

                            if (scFlag(nArgValid).eq.0) then

                                !! Corret Array End
                                if ((scFlag(nArgValid-1).eq.1202).or.(scFlag(nArgValid-1).eq.1002)) then
                                    !! Push
                                    if ( isWord(scFlag(2:nArgValid-2)) ) then
                                        if (isAllTrue(numFlag(2:nArgValid-2))) then
                                            !! Push Number Value
                                            Call dictDB(dictID)%sLTable%push(keySaved, rArg(2:nArgValid-2) )
                                        else
                                            !! Push Char Value
                                            Call dictDB(dictID)%sLTable%push(keySaved, cArg(2:nArgValid-2) )
                                        end if
                                        keySaved = "";              isKeyActive = .FALSE.
                                    else
                                        Call writeErrorFileDict("Wrong contents in array. key : ", keySaved)
                                    end if
                                !! Wrong Array End
                                else
                                    Call writeErrorFileDict("Wrong Format. Bad array end", tmpLine)
                                end if

                            else if ((scFlag(nArgValid).eq.1202).or.(scFlag(nArgValid).eq.1002)) then

                                Call writeErrorFileDict("Wrong Format. Bad array end", tmpLine)

                            else
                                !! Add Array
                                if (isWord( scFlag(2:nArgValid) )) then
                                    nTmpData = nArgValid - 1
                                    !! Save Data
                                    if (nTmpData.ge.1) then
                                        nSaveData     = 0
                                        Call addSaveArray(nSaveData,            &
                                                          tmpSaveCArg,          &
                                                          tmpSaveRArg,          &
                                                          tmpSaveNumFlag,       &
                                                          tmpSaveFlag,          &
                                                          cArg(2:nArgValid),    &
                                                          rArg(2:nArgValid),    &
                                                          numFlag(2:nArgValid), &
                                                          scFlag(2:nArgValid))
                                        isArrayActive = .TRUE.
                                    end if
                                else
                                    Call writeErrorFileDict("Wrong line description for array. ", tmpLine)
                                end if

                            end if

                        else
                            Call writeErrorFileDict("Wrong Format. Bad array start (Another array exist.)", tmpLine)
                        end if

                    !! Array End
                    else if ((scFlag(1).eq.1202).or.(scFlag(1).eq.1002)) then

                        if (isArrayActive.and.(scFlag(2).eq.0)) then
                            !! Push
                            if ( isWord(tmpSaveFlag) ) then
                                if (isAllTrue(tmpSaveNumFlag)) then
                                    !! Push Number Value
                                    Call dictDB(dictID)%sLTable%push(keySaved, tmpSaveRArg)
                                else
                                    !! Push Char Value
                                    Call dictDB(dictID)%sLTable%push(keySaved, tmpSaveCArg)
                                end if
                                keySaved = "";              isKeyActive = .FALSE.
                            else
                                Call writeErrorFileDict("Wrong contents in array. key : ", keySaved)
                            end if
                            isArrayActive = .FALSE.
                        else
                            Call writeErrorFileDict("Wrong Format. Bad array end", tmpLine)
                        end if

                    !! Bad Line
                    else
                        Call writeErrorFileDict("Bad line.", tmpLine)
                    end if

                else

                end if
            end if
        end if

    end do

End Subroutine
