Subroutine testDictionary()
    Implicit None
    Character(len=CHAR_LEN) :: dictName
    Type(typDictionary) tmpDict
    Type(typSLookUpTable) :: SLTable

    Call testProgram()

    ! Call mkSLTable(SLTable)
    !
    ! ! Call tmpDict%initialize(dictName)
    !
    ! Call tmpDict%initialize(dictName, SLTable)

end subroutine

Subroutine testProgram()
    Implicit None
    integer :: fid

    character(len=CHAR_LEN) ::fileDir
    character(len=CHAR_LEN) ::fileName
    character(len=CHAR_LEN) ::fileExt
    character(len=CHAR_LEN) ::filePath

    character(len=1000) :: tmpLine

    integer :: ios

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
        write(*,*) strHeader(trim(tmpLine)), findLineEnd(tmpLine), "    ",trim(tmpLine)
    END DO

    close(fid)

End Subroutine


Subroutine mkSLTable(SLTable)
    Type(typSLookUpTable) :: SLTable

    Call SLTable%initialize()

    Call SLTable%push("a", 123)
    Call SLTable%push("b", "is a")

End Subroutine
