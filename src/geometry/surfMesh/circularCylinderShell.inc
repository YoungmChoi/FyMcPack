!! ------------------------------------------------------------------ !!
!! Procedures : Type circularCylinderShellMesh
!! ------------------------------------------------------------------ !!

Subroutine initializeCirularCylinderShellMeshDict(this, dict)
    Implicit None
    !! - Input & Output Variables ------------------------------------------
    class(typCircularCylinderShellSurfMesh), intent(inout) :: this
    Type(typDictionaryPtr) :: dict

    Real(RP)    :: radius, height
    integer     :: nR, nZ
    Real(RP)    :: x0, y0, z0
    Integer     :: nLineSub
    Logical     :: writeLog
    Real(RP)    :: rZ

    radius = dict%getReal("radius")
    height = dict%getReal("height")

    nR = dict%getReal("nR")
    nZ = dict%getReal("nZ")

    x0 = dict%getRealOrDefault("center", 0.0_RP, 1)
    y0 = dict%getRealOrDefault("center", 0.0_RP, 2)
    z0 = dict%getRealOrDefault("center", 0.0_RP, 3)

    rz = dict%getRealOrDefault("rZ", 1.0_RP)

    nLineSub  = dict%getIntOrDefault("nSubLine", 1)
    writeLog  = dict%getLogicalOrDefault("writeLog",.TRUE.)

    Call this%initialize(radius, height, nR, nZ, x0, y0, z0, rZ, nLineSub, writeLog)

End Subroutine

Subroutine initializeCirularCylinderShellMesh(this, radius, height, &
                                                    nR, nZ, &
                                                    x0, y0, z0, rZ, &
                                                    nLineSub, &
                                                    writeLog)
    Implicit None
    Class(typCircularCylinderShellSurfMesh), intent(inout)  :: this
    Real(RP), intent(in)        :: radius, height
    integer, intent(in)         :: nR, nZ
    Real(RP), optional          :: x0, y0, z0
    Real(RP), optional          :: rZ
    Integer, optional           :: nLineSub
    Logical, optional           :: writeLog

    integer                     :: ir, iz
    real(RP)                    :: theta, dTheta
    real(RP)                    :: z, dz
    real(RP)                    :: x, y, xx, yy, zz
    integer                     :: nNode, nPanel

    Integer, Dimension(4)       :: nodeLabel
    Integer, Dimension(2)       :: lineLabel
    Real(RP),Dimension(3)       :: refVector

    refVector(1) = 0.0_RP
    refVector(2) = 0.0_RP
    refVector(3) = 1.0_RP

    !! Check Parameter
    if (radius.gt.0.0_RP) then
        this%radius_ = radius
    else
        write(*,*) " "
        write(*,*) "    [ERROR] initializeCircularCylinderShell(...)"
        write(*,*) "       radius should be larger than 0."
        write(*,*) "       given radius : ", radius
        write(*,*) " "
        stop
    end if

    !! Check Parameter
    if (height.gt.0.0_RP) then
        this%height_ = height
    else
        write(*,*) " "
        write(*,*) "    [ERROR] initializeCircularCylinderShell(...)"
        write(*,*) "       height should be larger than 0."
        write(*,*) "       given height : ", height
        write(*,*) " "
        stop
    end if

    !! Check Parameters
    if (nLineSub.ge.1) then
        this%nLineSub = nLineSub
    else
        write(*,*) " "
        write(*,*) "    [Warning] : initializeCircularCylinderShell(...)"
        write(*,*) "       Condition : nLineSub should larger than 1. "
        write(*,*) "        - nLineSub = ", nLineSub
        write(*,*) " "
        write(*,*) "          nLineSub = 1 will be used."
        write(*,*) " "
        this%nLineSub = 1
    end if

    !! Check Parameters
    if ((nR.le.3).OR.(nZ.le.2)) then
        write(*,*) " "
        write(*,*) "    [ERROR] : initializeCircularCylinderShell(...)"
        write(*,*) "       Condition : nR >= 3 or nZ >= 2."
        write(*,*) "        - nR, nZ = ", nR, nZ
        write(*,*) " "
        STOP
    else
        this%nR_ = nR
        this%nZ_ = nZ
    end if


    this%x0_ = 0.0_RP;  this%y0_ = 0.0_RP;  this%z0_ = 0.0_RP
    if (present(x0)) this%x0_ = x0
    if (present(y0)) this%y0_ = y0
    if (present(z0)) this%z0_ = z0

    !! Set Parameters
    this%rZ_ = 1.0_RP
    if (present(rZ)) then
        if (rZ.gt.0.00001_RP) then
            this%rZ_ = rZ
        else
            write(*,*) " "
            write(*,*) "    [ERROR] initializeCircularCylinderShell(...)"
            write(*,*) "       rZ (meshRatio) should be larger than 0.00001."
            write(*,*) "       given rZ : ", rZ
            write(*,*) " "
            stop
        end if
    end if

    !! Generate Mesh

    this%isOpenTop = .TRUE.
    this%nNode     = nZ * nR
    this%nPanel    = (nZ - 1) * nR

    allocate(this%node(this%nNode))
    allocate(this%panel(this%nPanel))

    this%nLineNode = nR * nLineSub
    this%nLine     = nR * nLineSub

    allocate(this%lineNode(this%nLineNode))
    allocate(this%linePanel(this%nLine))

    if (this%rZ_.eq. 1) then
        dz = height  / (nZ - 1.0_RP)
    else
        dz = height  * (this%rZ_ - 1.0_RP) / (this%rZ_**(nZ-1) - 1.0_RP)
    end if

    dtheta  = 2.0_RP * pi / nR


    z = -0.5_RP * height
    !! Make Node
    nNode = 0
    do iz = 1, nZ
        do ir = 1, nR
            theta = dtheta * real(ir - 1.0_RP, rp)
            nNode = nNode + 1
            x = radius * dcos(theta)
            y = radius * dsin(theta)
            Call this%node(nNode)%set(nNode, x, y, z)
        enddo
        z  = z + dz
        dz = dz * this%rZ_
    enddo

    !! Make Panel
    nPanel = 0
    do iz = 1, nZ - 1
        do ir = 1, nR
            nPanel = nPanel + 1
            nodeLabel(1) = (iz - 1) * nR + ir
            nodeLabel(2) = (iz - 1) * nR + ir + 1
            nodeLabel(3) = iz       * nR + ir + 1
            nodeLabel(4) = iz       * nR + ir

            if (ir.eq.nR) then
                nodeLabel(2) = (iz-1) * nR + 1
                nodeLabel(3) = iz * nR + 1
            end if
            Call this%panel(nPanel)%set(4, this%node, nodeLabel)
        enddo
    enddo

    !! Make Line Node
    dtheta  = 2.0_RP * pi / this%nLine
    z = 0.5_RP * height

    nNode = 0
    do ir = 1, this%nLine
        theta = dtheta * real(ir - 1.0_RP, rp)
        x = radius * dcos(theta)
        y = radius * dsin(theta)
        nNode = nNode + 1
        Call this%lineNode(nNode)%set(nNode, x, y, z)
    enddo

    !! Make Line
    nPanel = 0
    do ir = 1, this%nLine
        nPanel   = nPanel + 1
        lineLabel(1) = ir
        lineLabel(2) = ir + 1
        if (ir.eq.this%nLine) lineLabel(2) = 1
        Call this%linePanel(nPanel)%set(2, this%lineNode, lineLabel, refVector)
    enddo

    !! Move Mesh with Origin
    do nNode = 1, this%nNode
        xx = this%node(nNode)%x() + this%x0_
        yy = this%node(nNode)%y() + this%y0_
        zz = this%node(nNode)%z() + this%z0_

        Call this%node(nNode)%move(xx, yy, zz)
    enddo

    !! Move Line Mesh with Origin
    do nNode = 1, this%nLineNode
        xx = this%lineNode(nNode)%x() + this%x0_
        yy = this%lineNode(nNode)%y() + this%y0_
        zz = this%lineNode(nNode)%z() + this%z0_

        Call this%lineNode(nNode)%move(xx, yy, zz)
    enddo

    !! Write Input Parameters ----------------------------------------------
    if (present(writeLog)) then
    if (writeLog.EQV..TRUE.) then
    write(*,1001) " "
    write(*,1001) " mkSurfaceMesh : Generate Circular Cylinder Shell Surface Mesh"
    write(*,1001) " "
    write(*,1002) "    - Radius                       : ", this%radius_
    write(*,1002) "    - Height                       : ", this%height_
    write(*,1003) "    - Nr, Nz, (Number of Nodes)    : ", Nr, Nz
    write(*,1002) "    - x0, y0, z0 (Center Position) : ", this%x0_, this%y0_, this%z0_
    write(*,1002) "    - rZ (meshRatio, vertical)     : ", this%rZ_
    write(*,1003) "    - Number of Sub Line Segment   : ", this%nLineSub
    write(*,1001) " "
    end if
    end if

    1001 FORMAT(a)
    1002 FORMAT(a, 3(f15.6))
    1003 FORMAT(a, 3(8x,i7))
    !! ---------------------------------------------------------------------

End Subroutine

!! ------------------------------------------------------------------ !!
!! End Procedures
!! ------------------------------------------------------------------ !!
