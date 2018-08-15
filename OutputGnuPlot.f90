subroutine OutputGnuPlot(Conf,Geom,iStep,CC)
    use StructVar_Mod
    use LoopVar_Mod
    implicit none

    type(Configulation) :: Conf
    type(Geometry),intent(in) :: Geom
    integer,intent(in) :: iStep
    type(CellCenter),intent(inout) :: CC

    character(len=64) :: cStep,cDensity,cVelocity,cPressure
    integer,allocatable :: CellNumber(:)

    allocate(CellNumber(2))

    call Conserve2Primitive(Geom,CC)

    write(cStep,*) iStep
    write(cDensity, '("Density/Density",i5.5,".txt")') iStep

    open(unit=1,file=cDensity,status='unknown')
        do iCenterX=1, Geom%CellNumber(1)
            write(1,*) Geom%Bound(1,1) + 2.0d0 * Geom%Width(1,1) * dble(iCenterX),CC%PrimitiveVariable(1,iCenterX,1,1)
        end do
    close(1)

    write(cVelocity, '("Velocity/Velocity",i5.5,".txt")') iStep

    open(unit=1,file=cVelocity,status='unknown')
        do iCenterX=1, Geom%CellNumber(1)
            write(1,*) Geom%Bound(1,1) + 2.0d0 * Geom%Width(1,1) * dble(iCenterX),CC%PrimitiveVariable(2,iCenterX,1,1)
        end do
    close(1)

    write(cPressure, '("Pressure/Pressure",i5.5,".txt")') iStep

    open(unit=1,file=cPressure,status='unknown')
        do iCenterX=1, Geom%CellNumber(1)
            write(1,*) Geom%Bound(1,1) + 2.0d0 * Geom%Width(1,1) * dble(iCenterX),CC%PrimitiveVariable(3,iCenterX,1,1)
        end do
    close(1)

return
end subroutine OutputGnuPlot
