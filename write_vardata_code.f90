    type(Dataset), intent(in) :: dset
    character(len=*), intent(in) :: varname
    integer ncerr, nvar
    nvar = nvar_(dset,varname)
    ncerr = nf90_put_var(dset%ncid, dset%variables(nvar)%varid, values)
    call nccheck(ncerr)
