    ! read all data from variable varname, return in it array values.
    ! on input, values should be an allocatable array with the correct
    ! with the correct number of dimensions.  If values is allocated,
    ! it we deallocated and reallocated.
    type(Dataset), intent(in) :: dset
    character(len=*), intent(in) :: varname
    integer ncerr, nvar, n1,n2, ndim
    nvar = nvar_(dset,varname)
    if (dset%variables(nvar)%ndims /= 2) then
       print *,'rank of data array does not match number of variable dims'
       stop "stopped"
    endif
    n1 = dset%variables(nvar)%dimlens(1)
    n2 = dset%variables(nvar)%dimlens(2)
    if (allocated(values)) deallocate(values)
    allocate(values(n1,n2))
    ncerr = nf90_get_var(dset%ncid, dset%variables(nvar)%varid, values)
    call nccheck(ncerr)
