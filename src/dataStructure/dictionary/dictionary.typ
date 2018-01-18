Type, public :: typDictionary

private

    character(len=CHAR_LEN), public  :: dictName

    type(typSLookUpTable), public    :: sLTable

    type(typSLookUpTable), public    :: subDictTable

Contains

    !!!... Initialize dictionary with name
    !!
    !! Call initDict_Name(typDict, "dictName")
    !!
    procedure, pass, private :: initDict_Name


    !!... Initialize dictionary with name and lookUpTable
    !!
    !! Call initDict_Name(typDict, "dictName", typSLookUpTable)
    !!
    procedure, pass, private :: initDict_NameLookUpTable

    !!... Initialize dictionary with name and stringArray
    !!
    !! Call initDict_NameStringArray(typDict, "dictName", typStringArray)
    !!
    procedure, pass, private :: initDict_NameStringArray


    !! Initialize Overloading
    generic :: initialize => initDict_Name, initDict_NameLookUpTable

End Type
