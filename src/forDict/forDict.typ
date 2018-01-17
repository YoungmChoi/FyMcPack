Type typForDict

private

    integer :: newSize = 0

    integer :: curSize = 0

    integer :: nKey  = 0

    character(len=dictCharLen),allocatable :: keys(:)

    integer,allocatable :: keyi(:)

    logical,allocatable :: keyp(:)

    integer,allocatable :: keyNext(:)


    real(RP), allocatable :: valueR(:)

    integer, allocatable :: valueI(:)

    character(len=dictCharLen), allocatable :: valueS(:)

    logical, allocatable :: valueL(:)

    complex(RP), allocatable :: valueC(:)

contains

    procedure, pass, public :: intialize => intializeForDict

    procedure, pass, public :: clear => clearForDict

    final                   :: finalForDict

End Type
