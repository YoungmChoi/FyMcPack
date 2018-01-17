type, public :: typString

private

    integer :: nStr_

    Character(len=strMaxLength) :: str_

Contains

    procedure, pass, public :: set => setString

    procedure, pass, public :: get => getString

    procedure, pass, public :: clear => clearString

    procedure, pass, public :: getLower => getStringLower

    procedure, pass, public :: getUpper => getStringUpper

    procedure, pass, public :: getInt  => getStringInt

    procedure, pass, public :: getReal => getStringReal

    procedure, pass, private :: copyString

    procedure, pass, public :: destroy => clearString

    generic :: assignment(=) => copyString

end type typString

!! Subroutine Overloading  ----------------------------------------------------

!! Compare String & Character
interface isStringEqual

    module procedure isStringStringEqual

    module procedure isStringCharEqual

    module procedure isCharStringEqual

    module procedure isCharCharEqual

End interface


!! Parse String & Character
interface parse

    module procedure parseStringString

    module procedure parseCharChar

    module procedure parseStringChar

    module procedure parseCharString

end interface
