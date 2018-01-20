Subroutine initDict_Name(this, dictName)
    Implicit None
    class(typDictionary), intent(inout) :: this
    Character(len=CHAR_LEN), intent(in) :: dictName
    !! ------------------------------------------------------------------------
    ! Call initializeDictDataBase(this, dictName)
    this%dictName = dictName

End Subroutine

Subroutine initDict_NameLookUpTable(this, dictName, sLTable)
    Implicit None
    Class(typDictionary), intent(inout) :: this
    Character(len=CHAR_LEN), intent(in) :: dictName
    Type(typSLookUpTable), intent(in)   :: sLTable

    Call this%initialize(dictName)

End Subroutine

Subroutine initDict_NameFileIOIndex(this, dictName, fileIOIndex)
    Implicit None
    Class(typDictionary), intent(inout) :: this
    Character(len=CHAR_LEN), intent(in) :: dictName
    integer,intent(in)                  :: fileIOIndex
    !! Local Variables --------------------------------------------------------

End Subroutine

Recursive Subroutine initDict_NameFileCharArray(dictName, charArray, dictID)
    Implicit None
    Character(len=CHAR_LEN), intent(in)                    :: dictName
    Character(len=fileLineLength),dimension(:), intent(in) :: charArray
    Integer, intent(out)                                   :: dictID
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
    logical :: isKeyActive, isArrayActive, isDictActive
    integer :: ii

    character(len=CHAR_LEN)                :: keySaved
    !! Subroutine Start -------------------------------------------------------
    Character(len=fileLineLength),allocatable :: subDictCharArray(:)
    integer :: nLineSubDict
    integer :: subDictID

    Call initializeDictDataBase(dictName, dictID)

    nLine = sizeof(charArray) / sizeof(tmpLine)

    isKeyActive   = .FALSE.
    isDictActive  = .FALSE.
    isArrayActive = .FALSE.

    iLine = 0
    do
        iLine = iLine + 1
        if (iLine.ge.nLine) exit

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

                        !!... Need Add
                        Call readMakeCharArrayFromCharArray(charArray(iLine:), &
                                                            subDictCharArray, &
                                                            nLineSubDict, &
                                                            " ",&
                                                            "}" )
                        if (nLineSubDict.ge.1) then

                            !! Call Sub Dict
                            Call initDict_NameFileCharArray(keySaved, &
                                                            subDictCharArray(2:nLineSubDict),&
                                                            subDictID)

                            !! Skip line
                            iLine = iLine + nLineSubDict - 1

                            !! Add SubDict
                            Call dictDB(dictID)%subDictTable%push(keySaved, subDictID)

                        end if

                    !! Dictionary End
                    else if (scFlag(1).eq.1102) then

                        Call writeErrorFileDict("Bad line. Key : ", trim(keySaved))

                        exit

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

Subroutine destroyDictionary(this)
    Implicit None
    Class(typDictionary), intent(inout) :: this

    this%dictName = ""
    this%dictID   = 0

    Call this%sLTable%destroy()
    Call this%subDictTable%destroy()

End Subroutine
