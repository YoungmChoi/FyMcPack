Subroutine testDictionary()
    Implicit None
    Character(len=CHAR_LEN) :: dictName
    Type(typDictionary) tmpDict
    Type(typSLookUpTable) :: SLTable

    Call testMakeDict()

    ! Call mkSLTable(SLTable)
    !
    ! ! Call tmpDict%initialize(dictName)
    !
    ! Call tmpDict%initialize(dictName, SLTable)

end subroutine

Subroutine testMakeDict()
    Implicit None
    type(typDictionary) :: fileDict

    integer :: fid

    character(len=CHAR_LEN) ::fileDir
    character(len=CHAR_LEN) ::fileName
    character(len=CHAR_LEN) ::fileExt
    character(len=CHAR_LEN) ::filePath

    character(len=1000) :: tmpLine, modLine

    integer :: ios

    integer :: nArg
    character(len = CHAR_LEN), allocatable :: cArg(:), scArg(:), lcArg(:)
    integer, allocatable                   :: iArg(:)
    real(RP), allocatable                  :: rArg(:)
    logical,allocatable                    :: numFlag(:)
    integer,allocatable                    :: scFlag(:)
    logical :: iflag

    integer :: nArgValid, ii

    fid      = 101

    fileDir  = ""
    fileName = "testDict"
    fileExt  = ""

    filePath = trim(fileDir)//trim(fileName)//trim(fileExt)

    open(unit   = fid,        &
         file   = filePath,   &
         status = 'old', &
         iostat = ios)

    Call fileDict%initialize(fileName)

    DO
        !!... Read Line
        read(fid, "(a)", iostat = ios) tmpLine

        !!... Exit if file end
        IF (ios < 0) EXIT

        !! Modify Line to be separated correctly
        modLine = insertSpaceSpecialCharacter(tmpLine)

        !! Split Line
        Call splitLine(modLine, nArg, cArg, scArg, lcArg, iArg, rArg, numFlag, scFlag, iflag)

        !! No blank line
        if (nArg.ge.1) then

            !! Get Valid Line
            do ii = 1, nArg
                if (scFlag(ii).le.0) exit
            enddo
            nArgValid = ii
            !! write(*,*) trim(modLine), nArg, nArgValid

            write(*,*) "-----------------------------------------------------"
            write(*,*) trim(modLine)
            write(*,*) scFlag(1:nArgValid)

            !!... KeyWord Search

            !! One Line
            if (nArgValid.ge.3) then

                !! Correct Input (Start : Word, End : ";")
                if ((scFlag(1).eq.1).and.(scFlag(nArgValid).eq.0)) then

                    !! Simple Key (key and one value)
                    if (nArgValid.eq.3) then


                    !! Simple Key (key and multi value)
                    else

                    end if

                !! Wrong One Line Input
                else

                end if
            !! Multi Line
            else if (nArgValid.eq.1) then
            !! Key : Array Multi Line

            !! Key : Dictionary

            !! One Line But Error !!
            else

            end if

        end if

    END DO

    close(fid)

End Subroutine



Subroutine testShowIndex()
    Implicit None
    integer :: fid

    character(len=CHAR_LEN) ::fileDir
    character(len=CHAR_LEN) ::fileName
    character(len=CHAR_LEN) ::fileExt
    character(len=CHAR_LEN) ::filePath

    character(len=1000) :: tmpLine, modLine

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


Subroutine mkSLTable(SLTable)
    Type(typSLookUpTable) :: SLTable

    Call SLTable%initialize()

    Call SLTable%push("a", 123)
    Call SLTable%push("b", "is a")

End Subroutine
