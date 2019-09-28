    ! read attribute 'attname' return in 'values'.  If optional
    ! argument 'varname' is given, an variable attribute is returned.
    ! if the attribute is an 1d array, values should be an allocatable 1d
    ! array of the correct type. if values is allocated, it be deallocated
    ! and reallocated.
    type(Dataset), intent(in) :: dset
    character(len=*), intent(in), optional :: varname
    character(len=*), intent(in) :: attname
    integer, intent(out), optional :: errcode
    integer ncerr, varid, nvar, nlen
    logical return_errcode
    if(present(errcode)) then
        return_errcode=.true.
    else
        return_errcode=.false.
    endif
    if(present(varname))then
        nvar = get_nvar(dset,varname)
        varid = dset%variables(nvar)%varid
    else
        varid = NF90_GLOBAL
    endif 
    ncerr = nf90_inquire_attribute(dset%ncid, varid, attname, len=nlen)
    if (return_errcode) then
        errcode=ncerr
    else
        call nccheck(ncerr)
    endif
    if (allocated(values)) deallocate(values)
    allocate(values(nlen))
    ncerr = nf90_get_att(dset%ncid, varid, trim(attname), values)
    if (return_errcode) then
        errcode=ncerr
    else
        call nccheck(ncerr)
    endif
