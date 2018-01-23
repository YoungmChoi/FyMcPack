    Program main

        use modGlobal
        use modDataStructure

    Implicit None

        type(typDictionaryPtr)  :: dictPtr, subDict
        character(len=CHAR_LEN) :: fileDir
        character(len=CHAR_LEN) :: fileName
        character(len=CHAR_LEN) :: fileExt

        real(rp)                :: realval
        real(rp), allocatable   :: realArr(:)

        fileDir  = "../../"
        fileName = "testDict"
        fileExt  = ""

        Call dictPtr%initialize(fileDir, fileName, fileExt)
        Call dictPtr%print()

        write(*,*) dictPtr%getReal("varR")

        realArr = dictPtr%getRealArray("arrR")

        write(*,*) realArr

        subDict = dictPtr%subDict("dict1")

        Call subDict%print()

    End Program
