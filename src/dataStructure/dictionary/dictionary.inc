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

Subroutine addDict(this, target)
    Implicit None
    Class(typDictionary), intent(inout) :: this
    Type(typDictionary), intent(in)     :: target

    !! Data Add
    Call this%sLTable%add(target%sLTable)

    !! Sub Dict Add
    Call this%subDictTable%add(target%subDictTable)

End Subroutine

Subroutine destroyDictionary(this)
    Implicit None
    Class(typDictionary), intent(inout) :: this

    this%dictName = ""
    this%dictID   = 0

    Call this%sLTable%destroy()
    Call this%subDictTable%destroy()

End Subroutine
