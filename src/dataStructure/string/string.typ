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

interface replace

    module procedure replaceChar

end interface


!! Parse String & Character
interface parse

    module procedure parseStringString

    module procedure parseCharChar

    module procedure parseStringChar

    module procedure parseCharString

end interface

!! Return string header type
!!
!!  -1 : blank line
!!   0 : no comment
!!
!!  1  : line comment
!!  2  : ";" is used.
!!  3  : "$" is used.
!!
!!  11 : comment block Start
!!  12 : comment block end
!!
!!  21 : "{" is used.
!!  22 : "}" is used.
!!
!!  31 : "(" is used.
!!  32 : ")" is used.
!!
!!  41 : "[" is used.
!!  42 : "]" is used.
!!

Character(len=2), parameter, private :: commentBlockStart = "/*"

Character(len=2), parameter, private :: commentBlockEnd   = "*/"

Character(len=17),parameter, private :: commentCharList = "/!#@%^&*.,*-+<>?:"

integer, parameter, private :: nCommentCharList = 17    !! It should be same with length of commentCharList

private :: charHeader

interface strHeader

    module procedure stringHeader

    module procedure charHeader

end interface

interface findLineEnd

    module procedure findStringLineEnd

    module procedure findCharLineEnd

end interface
