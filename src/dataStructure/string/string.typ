type, public :: typString

private

    integer :: nStr_

    Character(len=CHAR_LEN) :: str_

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

!! Parse String & Character
interface parseOneChar

    module procedure parseCharCharOne

end interface

interface compact

    module procedure compactChar

end interface

!! Return string header type
!!
!!  -1 : blank line
!!   1 : word
!!   0 : ";" is used.
!!
!!  -100  : line comment
!!
!!  111 : comment block Start
!!  112 : comment block end
!!
!!  122 : "$" is used.
!!
!!  1001 : "[" is used.
!!  1002 : "]" is used.

!!  1101 : "{" is used.
!!  1102 : "}" is used.
!!
!!  1201 : "(" is used.
!!  1202 : ")" is used.
!!

Character(len=7), parameter, private :: specialCharacter = ";(){}[]"

integer, parameter, private :: nSpecialCharacter = 8    !! It should be same with length of specialCharacter

Character(len=2), parameter, private :: commentBlockStart = "/*"

Character(len=2), parameter, private :: commentBlockEnd   = "*/"

Character(len=3),parameter, private :: commentCharList = "/!#"

integer, parameter, private :: nCommentCharList = 3    !! It should be same with length of commentCharList

private :: charHeader

interface strHeader

    module procedure stringHeader

    module procedure charHeader

end interface

interface findLineEnd

    module procedure findStringLineEnd

    module procedure findCharLineEnd

end interface
