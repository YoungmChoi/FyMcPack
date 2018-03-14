Subroutine initDict_Name(this, dictName)
    Implicit None
    Class(typDictionaryPtr), intent(inout) ::  this
    Character(len=*), intent(in)         :: dictName
    !! ------------------------------------------------------------------------
    integer :: dictID

    nullify(this%ptrDict)
    Call initializeDictDataBase(dictName, dictID)

    this%ptrDict => dictDB(dictID)

End Subroutine

Subroutine initDict_NameLookUpTable(this, dictName, sLTable)
    Implicit None
    Class(typDictionaryPtr), intent(inout)      :: this
    Character(len=*), intent(in)                :: dictName
    Type(typSLookUpTable), intent(in)           :: sLTable
    !! ------------------------------------------------------------------------

    Call this%initialize(dictName)

    this%ptrDict%sLTable  = sLTable
    this%ptrDict%sLTable%name = trim(dictName)//"_subDict"

End Subroutine

Subroutine initDict_File(this, fileDir, fileName, ext)
    Implicit None
    Class(typDictionaryPtr), intent(inout)  :: this
    Character(len=*), intent(in)            :: fileDir
    Character(len=*), intent(in)            :: fileName
    Character(len=*),optional               :: ext
    !! ------------------------------------------------------------------------
    Type(typFileIO) :: dictFileIO
    character(len = fileLineLength),allocatable :: charArray(:)
    integer :: nLine
    integer :: dictID
    !! ------------------------------------------------------------------------
    Call this%initialize(fileName)

    Call dictFileIO%initialize(fileDir, fileName, ext, "old")

    Call readMakeCharArrayFromFile(dictFileIO%fileIndex, charArray, nLine, " ", " ")

    Call initDictDB_NameCharArray(fileName, charArray, dictID)

    this%ptrDict => dictDB(dictID)

    Call dictFileIO%destroy()

End Subroutine

Subroutine printDictionary(this)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this

    write(*,*) " "
    write(*,*) "---------------------------------------------------------------"
    write(*,*) " "
    write(*,*) " Dictionary : ", trim(this%ptrDict%dictName)
    write(*,*) " "
    write(*,*) "---------------------------------------------------------------"
    write(*,*) " "
    write(*,*) "  - Data : "
    write(*,*) " "
    Call this%ptrDict%sLTable%print()
    write(*,*) " "
    write(*,*) "  - Sub Dictionary : "
    write(*,*) " "
    Call this%ptrDict%subDictTable%print()
    write(*,*) " "
    write(*,*) "---------------------------------------------------------------"

End Subroutine

function subDictionaryPtr(this, subDictName) result(ptrSubDict)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: subDictName
    type(typDictionaryPtr)               :: ptrSubDict
    integer :: subDictID

    Call this%ptrDict%subDictTable%getInt(subDictName, subDictID)

    ptrSubDict%ptrDict => dictDB(subDictID)

end function

Function getDictReal(this, key, ivar) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    integer,optional                     :: ivar
    real(rp)                             :: value

    Call this%ptrDict%sLTable%getReal(key, value, ivar)

End Function

Function getDictRealArray(this, key) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    real(rp),allocatable                 :: value(:)

    Call this%ptrDict%sLTable%getRealArray(key, value)

End Function

Function getDictInt(this, key, ivar) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    integer,optional                     :: ivar
    integer                              :: value

    Call this%ptrDict%sLTable%getInt(key, value, ivar)

End Function

Function getDictIntArray(this, key) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    integer, allocatable                 :: value(:)

    Call this%ptrDict%sLTable%getIntArray(key, value)

End Function

Function getDictChar(this, key, ivar) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    integer,optional                     :: ivar
    Character(len=CHAR_LEN)              :: value

    Call this%ptrDict%sLTable%getChar(key, value, ivar)

End Function

Function getDictCharArray(this, key) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    Character(len=CHAR_LEN),allocatable  :: value(:)

    Call this%ptrDict%sLTable%getCharArray(key, value)

End Function

Function getDictString(this, key, ivar) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    integer,optional                     :: ivar
    type(typString)                      :: value

    Call this%ptrDict%sLTable%getString(key, value, ivar)

End Function

Function getDictStringArray(this, key) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    type(typString),allocatable          :: value(:)

    Call this%ptrDict%sLTable%getStringArray(key, value)

End Function

Function getDictLogical(this, key, ivar) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    integer,optional                     :: ivar
    logical                              :: value

    Call this%ptrDict%sLTable%getLogical(key, value, ivar)

End Function

Function getDictLogicalArray(this, key) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    logical,allocatable     :: value(:)

    Call this%ptrDict%sLTable%getLogicalArray(key, value)

End Function

Function getDictRealOrDefault(this, key, defaultValue, ivar) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    real(rp), intent(in)                 :: defaultValue
    integer,optional                     :: ivar
    real(rp)                             :: value

    if (this%ptrDict%sLTable%keyExist(key)) then
        Call this%ptrDict%sLTable%getReal(key, value, ivar)
    else
        value = defaultValue
    end if

End Function

Function getDictRealArrayOrDefault(this, key, defaultValue) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    real(rp), dimension(:),intent(in)    :: defaultValue
    real(rp),allocatable                 :: value(:)

    if (this%ptrDict%sLTable%keyExist(key)) then
        Call this%ptrDict%sLTable%getRealArray(key, value)
    else
        value = defaultValue
    end if

End Function

Function getDictIntOrDefault(this, key, defaultValue, ivar) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    integer,intent(in)                   :: defaultValue
    integer,optional                     :: ivar
    integer                              :: value

    if (this%ptrDict%sLTable%keyExist(key)) then
        Call this%ptrDict%sLTable%getInt(key, value, ivar)
    else
        value = defaultValue
    end if

End Function

Function getDictIntArrayOrDefault(this, key, defaultValue) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    integer,dimension(:),intent(in)      :: defaultValue
    integer, allocatable                 :: value(:)

    if (this%ptrDict%sLTable%keyExist(key)) then
        Call this%ptrDict%sLTable%getIntArray(key, value)
    else
        value = defaultValue
    end if

End Function

Function getDictCharOrDefault(this, key, defaultValue, ivar) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    Character(len=*),intent(in)          :: defaultValue
    integer,optional                     :: ivar
    Character(len=CHAR_LEN)              :: value

    if (this%ptrDict%sLTable%keyExist(key)) then
        Call this%ptrDict%sLTable%getChar(key, value, ivar)
    else
        value = ""
        value = defaultValue
    end if

End Function

Function getDictCharArrayOrDefault(this, key, defaultValue) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    Character(len=*),dimension(:),intent(in) :: defaultValue
    Character(len=CHAR_LEN),allocatable  :: value(:)

    if (this%ptrDict%sLTable%keyExist(key)) then
        Call this%ptrDict%sLTable%getCharArray(key, value)
    else
        value = ""
        value = defaultValue
    end if

End Function

Function getDictStringOrDefault(this, key, defaultValue, ivar) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    type(typString),intent(in)           :: defaultValue
    integer,optional                     :: ivar
    type(typString)                      :: value

    if (this%ptrDict%sLTable%keyExist(key)) then
        Call this%ptrDict%sLTable%getString(key, value, ivar)
    else
        value = defaultValue
    end if

End Function

Function getDictStringArrayOrDefault(this, key, defaultValue) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    type(typString),dimension(:), intent(in) :: defaultValue
    type(typString),allocatable          :: value(:)

    if (this%ptrDict%sLTable%keyExist(key)) then
        Call this%ptrDict%sLTable%getStringArray(key, value)
    else
        value = defaultValue
    end if

End Function

Function getDictLogicalOrDefault(this, key, defaultValue, ivar) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    logical, intent(in)                  :: defaultValue
    integer,optional                     :: ivar
    logical                              :: value

    if (this%ptrDict%sLTable%keyExist(key)) then
        Call this%ptrDict%sLTable%getLogical(key, value, ivar)
    else
        value = defaultValue
    end if

End Function

Function getDictLogicalArrayOrDefault(this, key, defaultValue) result(value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    logical, dimension(:), intent(in)    :: defaultValue
    logical,allocatable                  :: value(:)

    if (this%ptrDict%sLTable%keyExist(key)) then
        Call this%ptrDict%sLTable%getLogicalArray(key, value)
    else
        value = defaultValue
    end if

End Function

Subroutine setDictReal(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    Real, intent(in)                     :: value
    Call this%ptrDict%sLTable%push(key, value)
End Subroutine

Subroutine setDictRealRP(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    Real(RP), intent(in)                 :: value
    Call this%ptrDict%sLTable%push(key, value)
End Subroutine

Subroutine setDictDble(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    Double Precision, intent(in)                 :: value
    Call this%ptrDict%sLTable%push(key, real(value,rp))
End Subroutine

Subroutine setDictRealArray(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    Real, dimension(:), intent(in)       :: value
    Call this%ptrDict%sLTable%push(key, value)
End Subroutine

Subroutine setDictRealRPArray(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    Real(RP), dimension(:), intent(in)   :: value
    Call this%ptrDict%sLTable%push(key, value)
End Subroutine

Subroutine setDictDbleArray(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    Double Precision, dimension(:), intent(in) :: value
    Call this%ptrDict%sLTable%push(key, real(value,rp))
End Subroutine

Subroutine setDictInt(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    Integer, intent(in)                     :: value
    Call this%ptrDict%sLTable%push(key, value)
End Subroutine

Subroutine setDictIntArray(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    Integer, dimension(:), intent(in)       :: value
    Call this%ptrDict%sLTable%push(key, value)
End Subroutine

Subroutine setDictChar(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    character(len=*), intent(in)         :: value
    Call this%ptrDict%sLTable%push(key, value)
End Subroutine

Subroutine setDictCharArray(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    character(len=CHAR_LEN),dimension(:), intent(in)         :: value
    Call this%ptrDict%sLTable%push(key, value)
End Subroutine

Subroutine setDictLogical(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    logical, intent(in)          :: value
    Call this%ptrDict%sLTable%push(key, value)
End Subroutine

Subroutine setDictLogicalArray(this, key, value)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Character(len=*),intent(in)          :: key
    logical, dimension(:), intent(in)     :: value
    Call this%ptrDict%sLTable%push(key, value)
End Subroutine

Subroutine setDictDict(this, subDict)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Type(typDictionaryPtr), intent(in)   :: subDict

    if (subDict%ptrDict%dictID.ge.1) then
        Call this%ptrDict%subDictTable%push(subDict%ptrDict%dictName, subDict%ptrDict%dictID)
    else
        write(*,*) "    [ERROR] setSubDict(subDict), subDict is not stored."
        write(*,*) "        subDict Name : ", subDict%ptrDict%dictName
        write(*,*) "        subDict ID   : ", subDict%ptrDict%dictID
        stop
    end if

End Subroutine

Subroutine addDictDict(this, from)
    Implicit None
    Class(typDictionaryPtr), intent(in)  :: this
    Type(typDictionaryPtr), intent(in)   :: from

    if (from%ptrDict%dictID.ge.1) then
        Call this%ptrDict%addDict(from%ptrDict)
    else
        write(*,*) "    [ERROR] addDict(fromDict), fromDict is not stored."
        write(*,*) "        fromDict Name : ", from%ptrDict%dictName
        write(*,*) "        fromDict ID   : ", from%ptrDict%dictID
        stop
    end if

End Subroutine
