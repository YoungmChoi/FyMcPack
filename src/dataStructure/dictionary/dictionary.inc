Subroutine initDict_Name(this, dictName)
    Implicit None
    Class(typDictionary), intent(inout) :: this
    Character(len=CHAR_LEN), intent(in) :: dictName

    this%dictName = dictName

    Call this%sLTable%initialize()

End Subroutine

Subroutine initDict_NameLookUpTable(this, dictName, sLTable)
    Implicit None
    Class(typDictionary), intent(inout) :: this
    Character(len=CHAR_LEN), intent(in) :: dictName
    Type(typSLookUpTable), intent(in)   :: sLTable

    this%dictName = dictName
    this%sLTable  = sLTable

End Subroutine

Subroutine initDict_NameStringArray(this, dictName, stringArray)
    Implicit None
    Class(typDictionary), intent(inout) :: this
    Character(len=CHAR_LEN), intent(in) :: dictName
    type(typString), intent(in), allocatable, target :: stringArray(:)
    !!... Local Variables
    integer :: nString, istr
    type(typString) :: tmpString
    type(typString),pointer :: ptrString

    integer :: isComment

    this%dictName = dictName

    nString = sizeof(stringArray) / sizeof(tmpString)

    if (nString.ge.1) then
        !!... Check String
        do istr = 1, nString
            nullify(ptrString)
            ptrString => stringArray(istr)

            !! Call strHeader(ptrString, isComment)

        enddo
    end if

    nullify(ptrString)

End Subroutine
