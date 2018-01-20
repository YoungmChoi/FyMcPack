Subroutine initializeDictDataBase(dictName, dictID)
    Character(len=CHAR_LEN), intent(in)         :: dictName
    integer,intent(out) :: dictID
    !! ------------------------------------------------------------------------
    Type(typDictionary), target, allocatable :: tmpDict(:)
    logical, allocatable             :: tmpDictActive(:)
    integer :: i
    !! ------------------------------------------------------------------------
    if (sizeof(dictDBActive).eq.0) then
        nDictDB_ = 0
        allocate(dictDB(1))
        allocate(dictDBActive(1))
    else
        Call move_alloc(dictDBActive, tmpDictActive)
        allocate(dictDBActive(nDictDB_ + 1))
        dictDBActive(1: nDictDB_) = tmpDictActive

        if (allocated(tmpDict)) then
            do i = 1, nDictDB_
                Call tmpDict(i)%destroy()
            enddo
            deallocate(tmpDict)
        end if
        allocate(tmpDict(nDictDB_))

        do i = 1, nDictDB_
            tmpDict(i) = dictDB(i)
        enddo

        if (allocated(dictDB)) then
            do i = 1, nDictDB_
                Call dictDB(i)%destroy()
            enddo
            deallocate(dictDB)
        end if
        allocate(dictDB(nDictDB_ + 1))

        do i = 1, nDictDB_
            dictDB(i) = tmpDict(i)
        enddo

    end if

    nDictDB_ = nDictDB_ + 1

    dictID                  = nDictDB_
    dictDBActive(dictID)    = .TRUE.

    dictDB(dictID)%dictName = dictName
    dictDB(dictID)%dictID   = dictID

    Call dictDB(dictID)%sLTable%initialize(trim(dictName)//"_lookUpTable")
    Call dictDB(dictID)%subDictTable%initialize(trim(dictName)//"_subDict")

End Subroutine
