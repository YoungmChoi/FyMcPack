    Subroutine testSLTable()
        Implicit None
        Type(typSLookUpTable)   :: sLTable
        real(rp),allocatable    :: resultR(:)
        integer,allocatable :: resultI(:)

        Call testPush(sLTable)

        write(*,*) sLTable%getReal("a", 2), sLTable%getInt("a", 3)

        Call sLTable%getRealArray("A", resultR)
        Call sLTable%getIntArray("A", resultI)

        write(*,*) resultR
        write(*,*) resultI

        !!!... Check getString




    end Subroutine

    Subroutine testPush(sLTable)
        Type(typSLookUpTable) :: sLTable
        integer :: nData
        Real(RP), allocatable :: valR(:)
        character(len=CHAR_LEN), allocatable :: valC(:)
        logical, allocatable :: valL(:)

        nData = 3
        allocate(valR(nData))
        allocate(valC(nData))
        allocate(valL(nData))

        valR(1) = 1
        valR(2) = 2
        valR(3) = 3

        valC(1) = "www"
        valC(2) = ""
        valC(3) = "wwa"

        valL(1) = .TRUE.
        valL(2) = .FALSE.
        valL(3) = .FALSE.

        Call sLTable%initialize()

        Call sLTable%push("a", 1)

        Call sLTable%push("a", "1 2 3")

        Call sLTable%push("ab", 2.0)
        Call sLTable%push("abc", (/ 1.2, 3.5, 1.1, 1.5 /) )
        Call sLTable%push("add", valR)

        Call sLTable%push("abcd", "a is equal to b")

        Call sLTable%push("strA", valC)

        Call sLTable%push("L", .TRUE.)

        Call sLTable%push("L21", valL)

        Call sLTable%print()

    End Subroutine
