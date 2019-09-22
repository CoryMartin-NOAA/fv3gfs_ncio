    ! read attribute 'attname' return in 'values'.  If optional
    ! argument 'varname' is given, an variable attribute is returned.
    ! if the attribute is an 1d array, values should be an allocatable 1d
    ! array of the correct type. if values is allocated, it be deallocated
    ! and reallocated.
    type(Dataset), intent(in) :: dset
    character(len=*), intent(in), optional :: varname
    character(len=*), intent(in) :: attname
    integer ncerr, varid, nvar, nlen
    if(present(varname))then
        nvar = get_nvar(dset,varname)
        varid = dset%variables(nvar)%varid
    else
        varid = NF90_GLOBAL
    endif 
    ncerr = nf90_redef(dset%ncid)
    call nccheck(ncerr)
    ncerr = nf90_put_att(dset%ncid, varid, trim(attname), values)
    call nccheck(ncerr)
    ncerr = nf90_enddef(dset%ncid)
    call nccheck(ncerr)
