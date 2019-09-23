    ! read all data from variable varname, return in it array values.
    ! on input, values should be an allocatable array with the correct
    ! with the correct number of dimensions.  If values is allocated,
    ! it we deallocated and reallocated.
    type(Dataset), intent(in) :: dset
    character(len=*), intent(in) :: varname
    integer, intent(in), optional :: nslice
    integer ncerr, nvar, n1,n2,n3, ndim, ncount
    if (present(nslice)) then
       ncount = nslice
    else
       ncount = 1
    endif
    nvar = get_nvar(dset,varname)
    if (dset%variables(nvar)%ndims /= 2 .and. dset%variables(nvar)%ndims /= 3) then
       print *,'rank of data array != variable ndims (or ndims-1)'
       stop "stopped"
    endif
    n1 = dset%variables(nvar)%dimlens(1)
    n2 = dset%variables(nvar)%dimlens(2)
    if (dset%variables(nvar)%ndims == 3) n3 = dset%variables(nvar)%dimlens(4)
    if (allocated(values)) deallocate(values)
    allocate(values(n1,n2))
    if (dset%variables(nvar)%ndims == 3 .and. n3 == 1) then
       ! 3d variable, return 2d slice along last dimension
       ncerr = nf90_get_var(dset%ncid, dset%variables(nvar)%varid, values,&
               start=(/1,1,ncount/), count=(/n1,n2,1/))
    else
       ncerr = nf90_get_var(dset%ncid, dset%variables(nvar)%varid, values)
    endif
    call nccheck(ncerr)
