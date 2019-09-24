    ! read all data from variable varname, return in it array values.
    ! on input, values should be an allocatable array with the correct
    ! with the correct number of dimensions.  If values is allocated,
    ! it we deallocated and reallocated.
    type(Dataset), intent(in) :: dset
    character(len=*), intent(in) :: varname
    integer ncerr, nvar, n1,n2,n3,n4
    nvar = get_nvar(dset,varname)
    if (dset%variables(nvar)%ndims /= 4) then
       print *,'rank of data array does not match number of variable dims'
       stop "stopped"
    endif
    n1 = dset%variables(nvar)%dimlens(1)
    n2 = dset%variables(nvar)%dimlens(2)
    n3 = dset%variables(nvar)%dimlens(3)
    n4 = dset%variables(nvar)%dimlens(4)
    if (allocated(values)) deallocate(values)
    allocate(values(n1,n2,n3,n4))
    ncerr = nf90_get_var(dset%ncid, dset%variables(nvar)%varid, values)
    call nccheck(ncerr)
