Type, public :: typSLookUpTable

private

    integer :: nKey = 0

    integer :: nTable = 0

    Character(len=CHAR_LEN) :: name

    logical :: strongSearch = .FALSE.

    type(typString), allocatable :: keys(:)

    type(typDataArray), allocatable :: dataArray(:)

contains

    !!!... Private Procedures
    procedure, pass, private :: initializeSLtable

    procedure, pass, private :: pushSLTableReal, pushSLTableRealArray

    procedure, pass, private :: pushSLTableRealRP, pushSLTableRealRPArray

    procedure, pass, private :: pushSLTableInt, pushSLTableIntArray

    procedure, pass, private :: pushSLTableChar, pushSLTableCharArray

    procedure, pass, private :: pushSLTableLogical, pushSLTableLogicalArray

    procedure, pass, private :: printSearchError

    procedure, pass, private :: searchKeyIndex

    procedure, pass, private :: reallocateSLTable

    procedure, pass, private :: destroy => destroySLTable

    !!!... Public Procedures

    generic :: initialize => initializeSLtable

    generic :: push => pushSLTableReal, pushSLTableRealArray, &
                       pushSLTableRealRP, pushSLTableRealRPArray, &
                       pushSLTableInt, pushSLTableIntArray, &
                       pushSLTableChar, pushSLTableCharArray, &
                       pushSLTableLogical, pushSLTableLogicalArray

    procedure, pass, public :: getReal => getSLTableReal

    procedure, pass, public :: getRealArray => getSLTableRealArray

    procedure, pass, public :: getInt => getSLTableInt

    procedure, pass, public :: getIntArray => getSLTableIntArray

    procedure, pass, public :: print => printSLTable

End Type
