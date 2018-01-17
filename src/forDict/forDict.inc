Subroutine intializeForDict(this)
    Implicit None
    class(typForDict),intent(inout) :: this

    !! Allocate Default Size
    allocate(this%keys(dictDefaultSize))
    allocate(this%keyi(dictDefaultSize))

    allocate(this%valueR(dictDefaultSize))
    allocate(this%valueI(dictDefaultSize))
    allocate(this%valueS(dictDefaultSize))
    allocate(this%valueL(dictDefaultSize))
    allocate(this%valueC(dictDefaultSize))

    this%keys(:) = ""
    this%valueR(:) = 0.0_RP
    this%valueI(:) = 0
    this%valueS(:) = ""
    this%valueL(:) = .FALSE.
    this%valueC(:) = (0.0_RP, 0.0_RP)

    this%curSize = dictDefaultSize

End Subroutine

Subroutine clearForDict(this)
    Implicit None
    class(typForDict),intent(inout) :: this

    if (allocated(this%keys)) deallocate(this%keys)
    if (allocated(this%keyi)) deallocate(this%keyi)

    if (allocated(this%valueR)) deallocate(this%valueR)
    if (allocated(this%valueI)) deallocate(this%valueI)
    if (allocated(this%valueS)) deallocate(this%valueS)
    if (allocated(this%valueL)) deallocate(this%valueL)
    if (allocated(this%valueC)) deallocate(this%valueC)

    this%newSize = 0
    this%curSize = 0
    this%nKey  = 0

End Subroutine

Subroutine finalForDict(this)
    Implicit None
    type(typForDict), intent(inout) :: this
    Call this%clear()
End Subroutine
