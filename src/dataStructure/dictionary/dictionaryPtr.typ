Type, public :: typDictionaryPtr

    Type(typDictionary), pointer :: ptrDict

Contains

    !!!... Initialize dictionary with name
    !!
    !! Call initDict_Name(typDict, "dictName")
    !!
    procedure, pass, private :: initDict_Name

    !... Initialize dictionary with name and lookUpTable
    !
    ! Call initDict_Name(typDict, "dictName", typSLookUpTable)
    !
    procedure, pass, private :: initDict_NameLookUpTable

    !... Initialize dictionary with name and lookUpTable
    !
    ! Call initDict_Name(typDict, fileDir, fileName, ext)
    !
    procedure, pass, private :: initDict_File


    !! Public Procedure ---------------------------------------

    !... Initializer Overloading
    !
    ! Call initDict_Name(typDict, "dictName", typSLookUpTable)
    !
    generic :: initialize => initDict_Name, &
                             initDict_NameLookUpTable, &
                             initDict_File

    !!... Print Dictionary
    procedure, pass, public :: print => printDictionary

    !!... Return SubDictionary
    procedure, pass, public :: subDict => subDictionaryPtr

    !!... Get Real Values
    procedure, pass, public :: getReal => getDictReal

    !!... Get Real Array
    procedure, pass, public :: getRealArray => getDictRealArray

    !!... Get Char Values
    procedure, pass, public :: getChar => getDictChar

    !!... Get Char Array
    procedure, pass, public :: getCharArray => getDictCharArray

    !!... Get String Values
    procedure, pass, public :: getString => getDictString

    !!... Get String Array
    procedure, pass, public :: getStringArray => getDictStringArray

    !!... Get Logical Values
    procedure, pass, public :: getLogical => getDictLogical

    !!... Get Logical Array
    procedure, pass, public :: getLogicalArray => getDictLogicalArray


End Type
