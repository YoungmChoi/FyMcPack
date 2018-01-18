Type, private :: typDictDataBase

private

    integer :: nDictBase_

    Type(typDictionary), allocatable :: dictDataBase(:)

    logical,allocatable              :: dictActive(:)

End Type

Type(typDictDataBase), private :: dictDataBase

interface initializeDict

end interface
